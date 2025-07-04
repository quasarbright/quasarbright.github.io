// WebGL program for complex function visualization
let gl;
let program;
let positionBuffer;
let positionAttributeLocation;
let resolutionUniformLocation;
let maxIterationsUniformLocation;
let centerUniformLocation;
let zoomUniformLocation;
let initialZUniformLocation;
let timeUniformLocation; // Added for animated hue shift
let coloringModeUniformLocation; // Added for coloring mode selection

// Custom parameters storage
let customParameters = {};
let customParameterUniformLocations = {};

// Visualization parameters
let maxIterations = 500;
let center = [0.0, 0.0];
let zoom = 1.0;
let currentFunctionGLSL = "return csquare(z) + c;"; // Default Mandelbrot function
let initialZ = [0.0, 0.0]; // Initial value of z (z_0)
let coloringMode = 0; // 0 = escape, 1 = convergence

// Parameter control modes
let zControlMode = "dot";  // "dot" or "pixel"
let customParameterModes = {}; // name -> "dot" or "pixel"
let customParameterColors = {}; // name -> color for dot

// Built-in parameter colors
const builtInColors = {
    "z₀": "#00AAFF"
};

// Mouse interaction
let isDragging = false;
let lastMousePos = { x: 0, y: 0 };
let isDraggingDot = false;
let isHoveringDot = false;
let activeDot = null; // Which dot is being dragged: "z", "c", or a custom parameter name

// Canvas elements
let glCanvas;
let overlayCanvas;
let overlayCtx;

// Debug info
let debugInfo;

// Shader source code
let vertexShaderSource = '';
let fragmentShaderSourceTemplate = '';

// Function to validate GLSL syntax
function validateGLSL(code) {
    // Basic validation to catch common errors
    const errors = [];
    
    // Check for unbalanced braces/parentheses
    let braceCount = 0;
    let parenCount = 0;
    
    for (let i = 0; i < code.length; i++) {
        if (code[i] === '{') braceCount++;
        if (code[i] === '}') braceCount--;
        if (code[i] === '(') parenCount++;
        if (code[i] === ')') parenCount--;
        
        if (braceCount < 0) errors.push("Unbalanced braces: extra closing brace");
        if (parenCount < 0) errors.push("Unbalanced parentheses: extra closing parenthesis");
    }
    
    if (braceCount > 0) errors.push("Unbalanced braces: missing closing brace");
    if (parenCount > 0) errors.push("Unbalanced parentheses: missing closing parenthesis");
    
    // Check for missing semicolons at the end of statements
    if (!code.trim().endsWith(';') && !code.trim().endsWith('}')) {
        errors.push("Missing semicolon at the end");
    }
    
    // Check for return statement
    if (!code.includes('return')) {
        errors.push("Missing return statement");
    }
    
    return {
        valid: errors.length === 0,
        errors: errors
    };
}

// Apply function to shader
function applyFunctionToShader(functionCode) {
    console.log("Applying function to shader:", functionCode);
    console.log("Current custom parameters:", customParameters);
    console.log("Current custom parameter modes:", customParameterModes);
    
    // Validate the function code
    const validation = validateGLSL(functionCode);
    if (!validation.valid) {
        return {
            success: false,
            message: "GLSL syntax error: " + validation.errors.join(", ")
        };
    }
    
    // Check if the function is trying to use custom parameters that don't exist
    const paramWarning = checkForMissingParameters(functionCode);
    
    try {
        // Generate custom parameter uniforms
        let customUniformsCode = '';
        Object.keys(customParameters).forEach(paramName => {
            customUniformsCode += `uniform vec2 u_param_${paramName};\n`;
        });
        
        console.log("Generated custom uniforms code:", customUniformsCode);
        
        // Create a preamble that defines local variables for each custom parameter
        let paramPreamble = '';
        Object.keys(customParameters).forEach(paramName => {
            paramPreamble += `    vec2 ${paramName} = getParameterValue(u_param_${paramName}, pixelPos);\n`;
        });
        
        console.log("Generated parameter preamble:", paramPreamble);
        
        // Create the full function code with parameter preamble
        const fullFunctionCode = `// USER_FUNCTION_PLACEHOLDER\nvec2 complex_function(vec2 z, vec2 pixelPos) {\n${paramPreamble}    ${functionCode}\n}`;
        
        console.log("Full function code:", fullFunctionCode);
        
        // Start with a fresh copy of the template
        let fragmentShaderSource = fragmentShaderSourceTemplate;
        
        // Insert custom uniforms at the custom uniforms placeholder
        if (customUniformsCode) {
            fragmentShaderSource = fragmentShaderSource.replace(
                '// CUSTOM_UNIFORMS_PLACEHOLDER',
                customUniformsCode
            );
        }
        
        // Find the function placeholder in the modified shader
        // Use a more flexible regex that will match the function regardless of the parameter list
        const placeholderRegex = /\/\/ USER_FUNCTION_PLACEHOLDER[\s\S]*?vec2 complex_function\([^)]*\)[^{]*{[\s\S]*?}/;
        const placeholderMatch = fragmentShaderSource.match(placeholderRegex);
        
        if (!placeholderMatch) {
            console.error("Could not find function placeholder in shader template");
            console.log("Fragment shader source:", fragmentShaderSource);
            return {
                success: false,
                message: "Error: Could not find function placeholder in shader template"
            };
        }
        
        console.log("Found placeholder match:", placeholderMatch[0]);
        
        // Replace the placeholder with the user function
        fragmentShaderSource = fragmentShaderSource.replace(
            placeholderRegex,
            fullFunctionCode
        );
        
        console.log("Fragment shader with new function:", fragmentShaderSource);
        
        // Create and compile the new shader program
        const success = createShaderProgram(vertexShaderSource, fragmentShaderSource);
        
        if (success) {
            currentFunctionGLSL = functionCode;
            
            // Show parameter warning if needed
            if (paramWarning) {
                showInfo(paramWarning);
            }
            
            return {
                success: true,
                message: "Function applied successfully"
            };
        } else {
            return {
                success: false,
                message: "Error compiling shader with the provided function"
            };
        }
    } catch (error) {
        console.error('Error applying function to shader:', error);
        return {
            success: false,
            message: "Error: " + error.message
        };
    }
}

// Check if the function is trying to use custom parameters that don't exist
function checkForMissingParameters(functionCode) {
    // Look for both direct parameter names and u_param_ references
    const directParamRegex = /\b(\w+)\b/g;
    const prefixedParamRegex = /u_param_(\w+)/g;
    const usedParams = new Set();
    let match;
    
    // Check for direct parameter names (excluding common GLSL keywords and function names)
    const keywords = new Set([
        'return', 'vec2', 'vec3', 'vec4', 'float', 'int', 'bool',
        'if', 'else', 'for', 'while', 'break', 'continue',
        'z', 'abs', 'sin', 'cos', 'tan', 'exp', 'log',
        'length', 'dot', 'cross', 'normalize', 'mix', 'clamp',
        'min', 'max', 'fract', 'floor', 'ceil', 'mod',
        'csquare', 'ccube', 'cpow', 'cmul', 'cdiv', 'csin', 'ccos', 'cexp', 'clog',
        'pixelPos', 'getParameterValue' // Add these to the keywords to exclude them
    ]);
    
    // Extract all identifiers from the function code
    while ((match = directParamRegex.exec(functionCode)) !== null) {
        const name = match[1];
        if (!keywords.has(name)) {
            usedParams.add(name);
        }
    }
    
    // Check for prefixed parameter references
    while ((match = prefixedParamRegex.exec(functionCode)) !== null) {
        usedParams.add(match[1]);
    }
    
    // Check if all used parameters exist
    const missingParams = [];
    usedParams.forEach(param => {
        if (!customParameters[param]) {
            missingParams.push(param);
        }
    });
    
    if (missingParams.length > 0) {
        return `Warning: You're using parameter(s) that haven't been added yet: ${missingParams.join(', ')}. Add them in the Custom Parameters section.`;
    }
    
    return null;
}

// Create shader program
function createShaderProgram(vertexSource, fragmentSource) {
    // Create shaders
    const vertexShader = loadShader(gl, gl.VERTEX_SHADER, vertexSource);
    const fragmentShader = loadShader(gl, gl.FRAGMENT_SHADER, fragmentSource);
    
    if (!vertexShader || !fragmentShader) {
        return false;
    }
    
    // Create program
    const newProgram = gl.createProgram();
    gl.attachShader(newProgram, vertexShader);
    gl.attachShader(newProgram, fragmentShader);
    gl.linkProgram(newProgram);
    
    if (!gl.getProgramParameter(newProgram, gl.LINK_STATUS)) {
        const error = gl.getProgramInfoLog(newProgram);
        console.error('Program linking error:', error);
        showError('Program linking error: ' + error);
        gl.deleteProgram(newProgram);
        return false;
    }
    
    // Clean up old program if it exists
    if (program) {
        gl.deleteProgram(program);
    }
    
    // Use the new program
    program = newProgram;
    
    // Get attribute and uniform locations
    positionAttributeLocation = gl.getAttribLocation(program, 'a_position');
    resolutionUniformLocation = gl.getUniformLocation(program, 'u_resolution');
    maxIterationsUniformLocation = gl.getUniformLocation(program, 'u_maxIterations');
    centerUniformLocation = gl.getUniformLocation(program, 'u_center');
    zoomUniformLocation = gl.getUniformLocation(program, 'u_zoom');
    initialZUniformLocation = gl.getUniformLocation(program, 'u_initialZ');
    timeUniformLocation = gl.getUniformLocation(program, 'u_time'); // Get time uniform location
    coloringModeUniformLocation = gl.getUniformLocation(program, 'u_coloringMode'); // Get coloring mode uniform location
    
    // Get custom parameter uniform locations
    customParameterUniformLocations = {};
    Object.keys(customParameters).forEach(paramName => {
        const location = gl.getUniformLocation(program, `u_param_${paramName}`);
        if (location !== null) {
            customParameterUniformLocations[paramName] = location;
        } else {
            console.warn(`Could not find uniform location for parameter ${paramName}`);
        }
    });
    
    return true;
}

// Load shader from source
function loadShader(gl, type, source) {
    const shader = gl.createShader(type);
    gl.shaderSource(shader, source);
    gl.compileShader(shader);
    
    if (!gl.getShaderParameter(shader, gl.COMPILE_STATUS)) {
        const error = gl.getShaderInfoLog(shader);
        console.error('Shader compilation error:', error);
        console.error('Shader source:', source);
        showError('Shader compilation error: ' + error);
        gl.deleteShader(shader);
        return null;
    }
    
    return shader;
}

// Show error message
function showError(message) {
    debugInfo.style.display = 'block';
    debugInfo.innerHTML = message;
    debugInfo.style.color = 'red';
}

// Show info message
function showInfo(message) {
    // Only show info messages after initialization is complete
    if (document.readyState === 'complete') {
        debugInfo.style.display = 'block';
        debugInfo.innerHTML = message;
        debugInfo.style.color = 'white';
        
        // Hide after 3 seconds
        setTimeout(() => {
            if (debugInfo.innerHTML === message) {
                debugInfo.style.display = 'none';
            }
        }, 3000);
    } else {
        // Just log to console during initialization
        console.log("Info:", message);
    }
}

// Load shaders from file
async function loadShaderFromFile(url) {
    try {
        const response = await fetch(url);
        if (!response.ok) {
            throw new Error(`Failed to load ${url}: ${response.status} ${response.statusText}`);
        }
        return response.text();
    } catch (error) {
        console.error(`Error loading shader from ${url}:`, error);
        showError(`Error loading shader from ${url}: ${error.message}`);
        throw error;
    }
}

// Convert screen coordinates to complex plane coordinates
function screenToComplex(x, y, canvas) {
    const aspectRatio = canvas.width / canvas.height;
    // Note: y is inverted because screen coordinates have origin at top-left
    const normalizedX = (x / canvas.width) * 2 - 1;
    const normalizedY = 1 - (y / canvas.height) * 2; // Invert Y
    
    return [
        center[0] + normalizedX * aspectRatio / zoom,
        center[1] + normalizedY / zoom
    ];
}

// Convert complex plane coordinates to screen coordinates
function complexToScreen(cx, cy, canvas) {
    const aspectRatio = canvas.width / canvas.height;
    
    // Convert from complex coordinates to normalized device coordinates
    const normalizedX = ((cx - center[0]) * zoom / aspectRatio + 1) / 2;
    const normalizedY = ((cy - center[1]) * zoom + 1) / 2;
    
    // Convert to screen coordinates (invert Y)
    return {
        x: normalizedX * canvas.width,
        y: (1 - normalizedY) * canvas.height // Invert Y
    };
}

// Check if a point is near a dot
function isNearDot(x, y, param) {
    let dotPos;
    if (param === "z₀") {
        dotPos = complexToScreen(initialZ[0], initialZ[1], glCanvas);
    } else if (customParameters[param] && customParameterModes[param] === 'dot') {
        // Check for custom parameter dots
        const customParam = customParameters[param];
        dotPos = complexToScreen(customParam[0], customParam[1], glCanvas);
    }
    
    if (!dotPos) return false;
    
    const distance = Math.sqrt(
        Math.pow(x - dotPos.x, 2) + 
        Math.pow(y - dotPos.y, 2)
    );
    
    // Consider the point near if it's within 15 pixels of the dot
    return distance < 15;
}

// Draw dots on the overlay canvas
function drawDots() {
    // Clear the overlay canvas
    overlayCtx.clearRect(0, 0, overlayCanvas.width, overlayCanvas.height);
    
    // Draw z₀ dot if in dot mode
    if (zControlMode === "dot") {
        const zDotScreenPos = complexToScreen(initialZ[0], initialZ[1], glCanvas);
        const zDotRadius = (isHoveringDot && activeDot === "z₀") ? 8 : 6;
        
        // Draw the dot with white outline
        overlayCtx.beginPath();
        overlayCtx.arc(zDotScreenPos.x, zDotScreenPos.y, zDotRadius, 0, Math.PI * 2);
        overlayCtx.fillStyle = builtInColors["z₀"]; // Blue
        overlayCtx.fill();
        overlayCtx.lineWidth = 2;
        overlayCtx.strokeStyle = 'white';
        overlayCtx.stroke();
        
        // Add a label
        overlayCtx.fillStyle = 'white';
        overlayCtx.font = '12px Arial';
        overlayCtx.fillText('z₀', zDotScreenPos.x + zDotRadius + 2, zDotScreenPos.y - zDotRadius - 2);
    }
    
    // Draw custom parameter dots if in dot mode
    Object.keys(customParameters).forEach(paramName => {
        if (customParameterModes[paramName] === 'dot') {
            const param = customParameters[paramName];
            const dotScreenPos = complexToScreen(param[0], param[1], glCanvas);
            const dotRadius = (isHoveringDot && activeDot === paramName) ? 8 : 6;
            const dotColor = customParameterColors[paramName] || getRandomColor(paramName);
            
            // Draw the dot with white outline
            overlayCtx.beginPath();
            overlayCtx.arc(dotScreenPos.x, dotScreenPos.y, dotRadius, 0, Math.PI * 2);
            overlayCtx.fillStyle = dotColor;
            overlayCtx.fill();
            overlayCtx.lineWidth = 2;
            overlayCtx.strokeStyle = 'white';
            overlayCtx.stroke();
            
            // Add a label
            overlayCtx.fillStyle = 'white';
            overlayCtx.font = '12px Arial';
            overlayCtx.fillText(paramName, dotScreenPos.x + dotRadius + 2, dotScreenPos.y - dotRadius - 2);
        }
    });
}

// Generate a deterministic color from a string
function getRandomColor(str) {
    // Simple hash function
    let hash = 0;
    for (let i = 0; i < str.length; i++) {
        hash = str.charCodeAt(i) + ((hash << 5) - hash);
    }
    
    // Convert to hex color
    let color = '#';
    for (let i = 0; i < 3; i++) {
        const value = (hash >> (i * 8)) & 0xFF;
        color += ('00' + value.toString(16)).substr(-2);
    }
    
    // Store the color for future use
    customParameterColors[str] = color;
    
    return color;
}

// Resize both canvases
function resizeCanvases() {
    const width = window.innerWidth;
    const height = window.innerHeight;
    
    // Resize WebGL canvas
    glCanvas.width = width;
    glCanvas.height = height;
    
    // Resize overlay canvas
    overlayCanvas.width = width;
    overlayCanvas.height = height;
    
    if (gl) {
        gl.viewport(0, 0, width, height);
        draw();
    }
}

// Set up about modal
function setupAboutModal() {
    const aboutButton = document.getElementById('about-button');
    const modalOverlay = document.getElementById('modal-overlay');
    const closeModal = document.getElementById('close-modal');
    
    // Show modal when About button is clicked
    aboutButton.addEventListener('click', () => {
        modalOverlay.style.display = 'flex';
    });
    
    // Hide modal when close button is clicked
    closeModal.addEventListener('click', () => {
        modalOverlay.style.display = 'none';
    });
    
    // Hide modal when clicking outside the modal content
    modalOverlay.addEventListener('click', (e) => {
        if (e.target === modalOverlay) {
            modalOverlay.style.display = 'none';
        }
    });
    
    // Close modal with Escape key
    document.addEventListener('keydown', (e) => {
        if (e.key === 'Escape' && modalOverlay.style.display === 'flex') {
            modalOverlay.style.display = 'none';
        }
    });
}

// Initialize WebGL
async function init() {
    // Set up debug info
    debugInfo = document.getElementById('debug');
    debugInfo.style.display = 'none'; // Ensure it's hidden by default
    
    // Get canvases
    glCanvas = document.getElementById('glCanvas');
    overlayCanvas = document.getElementById('overlayCanvas');
    overlayCtx = overlayCanvas.getContext('2d');
    
    // Handle canvas resize
    window.addEventListener('resize', resizeCanvases);
    
    // Initialize WebGL context
    gl = glCanvas.getContext('webgl', { preserveDrawingBuffer: true });
    if (!gl) {
        showError('WebGL not supported in your browser');
        return;
    }
    
    // Ensure initialZ is set to (0,0)
    initialZ = [0.0, 0.0];
    
    // Add 'c' as a custom parameter on startup (for convenience, but it can be deleted)
    customParameters["c"] = [-0.7559, -0.0709]; // Start with an interesting Julia set value
    
    // Start in Julia set mode instead of Mandelbrot set mode
    zControlMode = "pixel"; // z₀ = pixel for Julia set
    customParameterModes["c"] = "dot"; // c = dot for Julia set
    customParameterColors["c"] = "#FF5500"; // Orange
    
    console.log("Custom parameters after initialization:", customParameters);
    console.log("Custom parameter modes after initialization:", customParameterModes);
    
    showInfo('WebGL context created successfully');
    
    try {
        // Load shaders
        showInfo('Loading shaders...');
        vertexShaderSource = await loadShaderFromFile('vertex.glsl');
        fragmentShaderSourceTemplate = await loadShaderFromFile('fragment.glsl');
        
        console.log("Loaded fragment shader template:", fragmentShaderSourceTemplate.substring(0, 500) + "...");
        
        showInfo('Shaders loaded successfully');
        
        // Create position buffer (a rectangle covering the screen)
        positionBuffer = gl.createBuffer();
        gl.bindBuffer(gl.ARRAY_BUFFER, positionBuffer);
        
        const positions = [
            -1.0, -1.0,
             1.0, -1.0,
            -1.0,  1.0,
             1.0,  1.0
        ];
        
        gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(positions), gl.STATIC_DRAW);
        
        // Set up about modal
        setupAboutModal();
        
        // Set up mouse events for interaction
        setupMouseEvents();
        
        // Set up function input
        setupFunctionInput();
        
        // Set up coloring mode selector
        setupColoringModeSelector();
        
        // Set up parameter controls
        setupParameterControls();
        
        // Initial resize
        resizeCanvases();
        
        // Get the initial function from the input field
        const functionInput = document.getElementById('function-input');
        currentFunctionGLSL = functionInput.value || "return csquare(z) + c;";
        
        // Apply the initial function to create the shader program
        const result = applyFunctionToShader(currentFunctionGLSL);
        if (!result.success) {
            showError(result.message);
        } else {
            console.log("Initial function applied successfully");
        }
        
        // Start the animation loop
        function animate() {
            draw();
            requestAnimationFrame(animate);
        }
        
        // Start the animation loop
        animate();
        
        // Show debug info for troubleshooting
        debugInfo.style.display = 'none';
        
    } catch (error) {
        console.error('Initialization error:', error);
        showError('Initialization error: ' + error.message);
    }
}

// Set up function input
function setupFunctionInput() {
    const functionInput = document.getElementById('function-input');
    const applyButton = document.getElementById('apply-function');
    const examples = document.querySelectorAll('.example');
    
    // Set default function if input is empty
    if (!functionInput.value.trim()) {
        functionInput.value = "return csquare(z) + c;";
    }
    
    // Apply function when button is clicked
    applyButton.addEventListener('click', () => {
        const funcString = functionInput.value;
        console.log("Function input value:", funcString);
        
        // Apply the function to the shader
        const result = applyFunctionToShader(funcString);
        
        if (result.success) {
            showInfo('Function applied: ' + result.message);
        } else {
            showError(result.message);
        }
        
        // Redraw with new function
        draw();
    });
    
    // Apply function when Ctrl+Enter is pressed
    functionInput.addEventListener('keydown', (e) => {
        if (e.key === 'Enter' && e.ctrlKey) {
            e.preventDefault(); // Prevent adding a newline
            applyButton.click();
        }
    });
    
    // Set up example functions
    updateFunctionExamples();
}

// Set up parameter controls
function setupParameterControls() {
    // Set up all parameters
    refreshParametersUI();
    
    // Add new parameter button
    const addParamBtn = document.getElementById('add-param-btn');
    const newParamName = document.getElementById('new-param-name');
    const newParamValueReal = document.getElementById('new-param-value-real');
    const newParamValueImag = document.getElementById('new-param-value-imag');
    
    addParamBtn.addEventListener('click', () => {
        const name = newParamName.value.trim();
        const realValue = parseFloat(newParamValueReal.value) || 0;
        const imagValue = parseFloat(newParamValueImag.value) || 0;
        
        if (name && name !== "z₀") {
            // Add the parameter
            customParameters[name] = [realValue, imagValue];
            customParameterModes[name] = 'dot'; // Default to dot mode
            
            // Clear the input fields
            newParamName.value = '';
            newParamValueReal.value = '';
            newParamValueImag.value = '';
            
            // Refresh the UI
            refreshParametersUI();
            
            // Recompile the shader with the new parameter
            applyFunctionToShader(currentFunctionGLSL);
        } else if (name === "z₀") {
            showError("Cannot add parameter with reserved name: z₀");
        }
    });
}

// Refresh the parameters UI
function refreshParametersUI() {
    const container = document.getElementById('parameters-container');
    
    // Clear the container
    container.innerHTML = '';
    
    // Add built-in z₀ parameter
    addParameterRow(container, "z₀", initialZ, zControlMode, builtInColors["z₀"], true);
    
    // Add custom parameters
    Object.keys(customParameters).forEach(paramName => {
        const param = customParameters[paramName];
        const mode = customParameterModes[paramName] || 'dot';
        const color = customParameterColors[paramName] || getRandomColor(paramName);
        
        addParameterRow(container, paramName, param, mode, color, false);
    });
}

// Add a parameter row to the UI
function addParameterRow(container, name, value, mode, color, isBuiltIn) {
    const paramRow = document.createElement('div');
    paramRow.className = 'parameter-row';
    paramRow.setAttribute('data-param-name', name);
    
    // Parameter name
    const nameElem = document.createElement('div');
    nameElem.className = 'parameter-name';
    nameElem.textContent = name;
    paramRow.appendChild(nameElem);
    
    // Parameter value container
    const valueContainer = document.createElement('div');
    valueContainer.className = 'parameter-value-container';
    
    // For dot mode, create input fields
    const realInput = document.createElement('input');
    realInput.type = 'number';
    realInput.className = 'param-input param-real';
    realInput.step = '0.0001';
    realInput.value = value[0].toFixed(4);
    realInput.style.width = '60px';
    
    const plusSign = document.createElement('span');
    plusSign.textContent = ' + ';
    
    const imagInput = document.createElement('input');
    imagInput.type = 'number';
    imagInput.className = 'param-input param-imag';
    imagInput.step = '0.0001';
    imagInput.value = value[1].toFixed(4);
    imagInput.style.width = '60px';
    
    const iSymbol = document.createElement('span');
    iSymbol.textContent = 'i';
    iSymbol.style.marginLeft = '4px'; // Add space between input and i symbol
    
    // For pixel mode, show placeholder text
    const pixelText = document.createElement('div');
    pixelText.className = 'pixel-text';
    pixelText.textContent = '(pixel position)';
    pixelText.style.opacity = '0.5';
    pixelText.style.fontStyle = 'italic';
    
    // Add event listeners to update parameter values when inputs change
    function updateFromInputs() {
        const realValue = parseFloat(realInput.value) || 0;
        const imagValue = parseFloat(imagInput.value) || 0;
        
        if (name === "z₀") {
            initialZ = [realValue, imagValue];
        } else {
            customParameters[name] = [realValue, imagValue];
        }
        
        draw(); // Redraw to update the visualization
    }
    
    realInput.addEventListener('change', updateFromInputs);
    imagInput.addEventListener('change', updateFromInputs);
    
    // Add elements to the value container based on mode
    if (mode === 'pixel') {
        valueContainer.appendChild(pixelText);
        realInput.style.display = 'none';
        plusSign.style.display = 'none';
        imagInput.style.display = 'none';
        iSymbol.style.display = 'none';
    } else {
        valueContainer.appendChild(realInput);
        valueContainer.appendChild(plusSign);
        valueContainer.appendChild(imagInput);
        valueContainer.appendChild(iSymbol);
        pixelText.style.display = 'none';
    }
    
    valueContainer.appendChild(realInput);
    valueContainer.appendChild(plusSign);
    valueContainer.appendChild(imagInput);
    valueContainer.appendChild(iSymbol);
    valueContainer.appendChild(pixelText);
    
    paramRow.appendChild(valueContainer);
    
    // Parameter controls
    const controlsElem = document.createElement('div');
    controlsElem.className = 'parameter-controls-group';
    
    // Mode selection - dot or pixel
    const modeSelect = document.createElement('select');
    modeSelect.className = 'mode-select';
    modeSelect.innerHTML = `
        <option value="dot" ${mode === 'dot' ? 'selected' : ''}>Dot</option>
        <option value="pixel" ${mode === 'pixel' ? 'selected' : ''}>Pixel</option>
    `;
    
    // Add event listener
    if (isBuiltIn) {
        // For z₀
        modeSelect.addEventListener('change', () => {
            zControlMode = modeSelect.value;
            
            // Update the value display
            updateParameterValueDisplay(name, initialZ, modeSelect.value);
            
            draw();
        });
    } else {
        // For custom parameters
        modeSelect.addEventListener('change', () => {
            customParameterModes[name] = modeSelect.value;
            
            // Update the value display
            updateParameterValueDisplay(name, customParameters[name], modeSelect.value);
            
            draw();
        });
    }
    
    controlsElem.appendChild(modeSelect);
    
    // Color indicator for dot mode
    const colorIndicator = document.createElement('div');
    colorIndicator.className = 'color-indicator';
    colorIndicator.style.backgroundColor = color;
    colorIndicator.style.display = mode === 'dot' ? 'inline-block' : 'none';
    controlsElem.appendChild(colorIndicator);
    
    // Pixel indicator for pixel mode
    const pixelIndicator = document.createElement('div');
    pixelIndicator.className = 'pixel-indicator';
    pixelIndicator.style.display = mode === 'pixel' ? 'inline-block' : 'none';
    pixelIndicator.title = 'Using pixel position';
    controlsElem.appendChild(pixelIndicator);
    
    // Update indicators display when mode changes
    modeSelect.addEventListener('change', () => {
        colorIndicator.style.display = modeSelect.value === 'dot' ? 'inline-block' : 'none';
        pixelIndicator.style.display = modeSelect.value === 'pixel' ? 'inline-block' : 'none';
        
        // Show/hide input fields based on mode
        if (modeSelect.value === 'pixel') {
            realInput.style.display = 'none';
            plusSign.style.display = 'none';
            imagInput.style.display = 'none';
            iSymbol.style.display = 'none';
            pixelText.style.display = 'inline';
        } else {
            realInput.style.display = 'inline';
            plusSign.style.display = 'inline';
            imagInput.style.display = 'inline';
            iSymbol.style.display = 'inline';
            pixelText.style.display = 'none';
        }
    });
    
    // Delete button
    const deleteBtn = document.createElement('button');
    deleteBtn.className = 'small-button';
    deleteBtn.textContent = 'Delete';
    
    // Disable delete button for z₀ since it's required
    if (isBuiltIn) {
        deleteBtn.disabled = true;
        deleteBtn.title = "Required parameter";
    } else {
        // For custom parameters, allow deletion
        deleteBtn.addEventListener('click', () => {
            delete customParameters[name];
            delete customParameterModes[name];
            delete customParameterColors[name];
            refreshParametersUI();
            applyFunctionToShader(currentFunctionGLSL);
        });
    }
    
    controlsElem.appendChild(deleteBtn);
    
    paramRow.appendChild(controlsElem);
    container.appendChild(paramRow);
}

// Helper function to update parameter value display
function updateParameterValueDisplay(name, value, mode) {
    const paramRow = document.querySelector(`.parameter-row[data-param-name="${name}"]`);
    if (paramRow) {
        const realInput = paramRow.querySelector('.param-real');
        const imagInput = paramRow.querySelector('.param-imag');
        const pixelText = paramRow.querySelector('.pixel-text');
        const plusSign = paramRow.querySelector('.param-input + span');
        const iSymbol = paramRow.querySelector('.param-imag + span');
        
        if (realInput && imagInput && pixelText) {
            if (mode === 'pixel') {
                realInput.style.display = 'none';
                if (plusSign) plusSign.style.display = 'none';
                imagInput.style.display = 'none';
                if (iSymbol) iSymbol.style.display = 'none';
                pixelText.style.display = 'inline';
            } else {
                realInput.value = value[0].toFixed(4);
                imagInput.value = value[1].toFixed(4);
                realInput.style.display = 'inline';
                if (plusSign) plusSign.style.display = 'inline';
                imagInput.style.display = 'inline';
                if (iSymbol) {
                    iSymbol.style.display = 'inline';
                    iSymbol.style.marginLeft = '4px'; // Ensure spacing is maintained
                }
                pixelText.style.display = 'none';
            }
        }
    }
}

// Set up mouse events
function setupMouseEvents() {
    // Use the glCanvas for events, but we'll draw the dot on the overlayCanvas
    glCanvas.addEventListener('mousedown', (e) => {
        // Check if we're clicking on a dot
        if (zControlMode === "dot" && isNearDot(e.clientX, e.clientY, "z₀")) {
            isDraggingDot = true;
            activeDot = "z₀";
        } else {
            // Check for custom parameter dots
            let foundCustomDot = false;
            Object.keys(customParameters).forEach(paramName => {
                if (customParameterModes[paramName] === 'dot' && isNearDot(e.clientX, e.clientY, paramName)) {
                    isDraggingDot = true;
                    activeDot = paramName;
                    foundCustomDot = true;
                }
            });
            
            if (!foundCustomDot) {
                isDragging = true;
            }
        }
        lastMousePos = { x: e.clientX, y: e.clientY };
    });
    
    glCanvas.addEventListener('mouseup', () => {
        isDragging = false;
        isDraggingDot = false;
        activeDot = null;
    });
    
    glCanvas.addEventListener('mouseleave', () => {
        isDragging = false;
        isDraggingDot = false;
        isHoveringDot = false;
        activeDot = null;
        drawDots(); // Update dot appearance
    });
    
    glCanvas.addEventListener('mousemove', (e) => {
        if (isDraggingDot) {
            // Update the appropriate parameter based on mouse position
            const complexCoords = screenToComplex(e.clientX, e.clientY, glCanvas);
            
            if (activeDot === "z₀") {
                initialZ = complexCoords;
                // Update the display
                updateParameterValueDisplay("z₀", initialZ, "dot");
            } else if (customParameters[activeDot]) {
                // Update custom parameter
                customParameters[activeDot] = complexCoords;
                // Update the display
                updateParameterValueDisplay(activeDot, complexCoords, "dot");
            }
            
            draw();
        } else if (isDragging) {
            const dx = e.clientX - lastMousePos.x;
            const dy = e.clientY - lastMousePos.y;
            
            const aspectRatio = glCanvas.width / glCanvas.height;
            center[0] -= dx / zoom / glCanvas.width * 2 * aspectRatio;
            center[1] += dy / zoom / glCanvas.height * 2;
            
            lastMousePos = { x: e.clientX, y: e.clientY };
            draw();
        } else {
            // Check if we're hovering over a dot
            const wasHovering = isHoveringDot;
            const oldActiveDot = activeDot;
            
            isHoveringDot = false;
            activeDot = null;
            
            // Check z dot first
            if (zControlMode === "dot" && isNearDot(e.clientX, e.clientY, "z₀")) {
                isHoveringDot = true;
                activeDot = "z₀";
            } 
            // Then check custom parameter dots
            else {
                Object.keys(customParameters).forEach(paramName => {
                    if (customParameterModes[paramName] === 'dot' && isNearDot(e.clientX, e.clientY, paramName)) {
                        isHoveringDot = true;
                        activeDot = paramName;
                    }
                });
            }
            
            // Only redraw if hover state changed
            if (wasHovering !== isHoveringDot || oldActiveDot !== activeDot) {
                drawDots();
            }
        }
    });
    
    glCanvas.addEventListener('wheel', (e) => {
        e.preventDefault();
        
        // Get mouse position in normalized device coordinates
        const rect = glCanvas.getBoundingClientRect();
        const mouseX = (e.clientX - rect.left) / glCanvas.width * 2 - 1;
        const mouseY = 1 - (e.clientY - rect.top) / glCanvas.height * 2;
        
        // Convert to complex plane coordinates
        const aspectRatio = glCanvas.width / glCanvas.height;
        const mouseComplex = [
            center[0] + mouseX * aspectRatio / zoom,
            center[1] + mouseY / zoom
        ];
        
        // Adjust zoom
        const zoomFactor = e.deltaY < 0 ? 1.1 : 0.9;
        zoom *= zoomFactor;
        
        // Adjust center to keep mouse position fixed in complex plane
        center[0] = mouseComplex[0] - mouseX * aspectRatio / zoom;
        center[1] = mouseComplex[1] - mouseY / zoom;
        
        draw();
    });
    
    // Add keyboard controls
    document.addEventListener('keydown', (e) => {
        // Ignore keyboard shortcuts when typing in input fields
        if (e.target.tagName === 'INPUT' || e.target.tagName === 'TEXTAREA') {
            return;
        }
        
        switch (e.key) {
            case '+':
            case '=': // For keyboards without numpad
                // Increase iterations
                maxIterations += 50;
                document.getElementById('iterationCount').textContent = maxIterations;
                draw();
                break;
            case '-':
                // Decrease iterations
                maxIterations = Math.max(10, maxIterations - 50);
                document.getElementById('iterationCount').textContent = maxIterations;
                draw();
                break;
            case 'r':
            case 'R':
                // Reset view
                resetView();
                break;
            case 'd':
                // Toggle debug info
                debugInfo.style.display = debugInfo.style.display === 'none' ? 'block' : 'none';
                break;
            case 'j':
                if (zControlMode !== "pixel" || customParameterModes["c"] !== "dot") {
                    // Add 'c' parameter if it doesn't exist
                    if (!customParameters["c"]) {
                        customParameters["c"] = [-0.7559, -0.0709];
                        customParameterModes["c"] = "dot";
                        customParameterColors["c"] = "#FF5500";
                        showInfo("Added 'c' parameter for Julia set mode");
                        refreshParametersUI();
                    } else if (zControlMode !== "pixel" || customParameterModes["c"] !== "dot") {
                        // Update control modes
                        zControlMode = "pixel";
                        customParameterModes["c"] = "dot";
                        
                        // Update the UI
                        refreshParametersUI();
                        
                        draw();
                        showInfo('Switched to Julia set mode');
                    }
                }
                break;
            case 'm':
                if (zControlMode !== "dot" || customParameterModes["c"] !== "pixel") {
                    // Add 'c' parameter if it doesn't exist
                    if (!customParameters["c"]) {
                        customParameters["c"] = [-0.7559, -0.0709];
                        customParameterModes["c"] = "pixel";
                        customParameterColors["c"] = "#FF5500";
                        showInfo("Added 'c' parameter for Mandelbrot set mode");
                        refreshParametersUI();
                    } else if (zControlMode !== "dot" || customParameterModes["c"] !== "pixel") {
                        // Update control modes
                        zControlMode = "dot";
                        customParameterModes["c"] = "pixel";
                        
                        // Update the UI
                        refreshParametersUI();
                        
                        draw();
                        showInfo('Switched to Mandelbrot set mode');
                    }
                }
                break;
            default:
                return;
        }
        
        draw();
    });
}

// Get custom parameter value based on its control mode
function getCustomParameterValue(paramName) {
    const mode = customParameterModes[paramName] || 'dot';
    
    if (mode === 'pixel') {
        // For pixel mode, return special value to indicate that the shader should use pixel position
        return [-999.0, -999.0];
    } else {
        // For dot mode, return the stored value
        return customParameters[paramName] || [-0.7559, -0.0709];
    }
}

// Get shader uniform values based on control modes
function getShaderParameters() {
    // For the shader, we need to pass either the actual value or a special value to indicate "use pixel position"
    // Using (-999, -999) as a special value to indicate "use pixel position" instead of (0,0)
    let shaderInitialZ = [-999.0, -999.0]; // Special value for "use pixel position"
    
    // If z is controlled by a dot, pass the actual value
    if (zControlMode === "dot") {
        shaderInitialZ = initialZ;
    }
    
    return { shaderInitialZ };
}

// Draw the complex function visualization
function draw() {
    try {
        // Draw the fractal on the WebGL canvas
        gl.clearColor(0.0, 0.0, 0.0, 1.0);
        gl.clear(gl.COLOR_BUFFER_BIT);
        
        gl.useProgram(program);
        
        // Set up position attribute
        gl.enableVertexAttribArray(positionAttributeLocation);
        gl.bindBuffer(gl.ARRAY_BUFFER, positionBuffer);
        gl.vertexAttribPointer(positionAttributeLocation, 2, gl.FLOAT, false, 0, 0);
        
        // Get shader parameters based on control modes
        const { shaderInitialZ } = getShaderParameters();
        
        console.log("Drawing with parameters:");
        console.log("- Initial Z:", shaderInitialZ);
        console.log("- Center:", center);
        console.log("- Zoom:", zoom);
        console.log("- Max Iterations:", maxIterations);
        
        // Set uniforms
        gl.uniform2f(resolutionUniformLocation, glCanvas.width, glCanvas.height);
        gl.uniform1i(maxIterationsUniformLocation, maxIterations);
        gl.uniform2f(centerUniformLocation, center[0], center[1]);
        gl.uniform1f(zoomUniformLocation, zoom);
        gl.uniform2f(initialZUniformLocation, shaderInitialZ[0], shaderInitialZ[1]);
        
        // Set time uniform for animated hue shift
        gl.uniform1f(timeUniformLocation, performance.now() / 1000.0);
        
        // Set coloring mode uniform
        gl.uniform1i(coloringModeUniformLocation, coloringMode);
        
        console.log("Setting custom parameter uniforms:");
        
        // Set custom parameter uniforms
        Object.keys(customParameterUniformLocations).forEach(paramName => {
            const location = customParameterUniformLocations[paramName];
            if (location !== null) {
                const paramValue = getCustomParameterValue(paramName);
                console.log(`- ${paramName}:`, paramValue, "at location:", location);
                gl.uniform2f(location, paramValue[0], paramValue[1]);
            } else {
                console.warn(`No uniform location for parameter ${paramName}`);
            }
        });
        
        // Draw
        gl.drawArrays(gl.TRIANGLE_STRIP, 0, 4);
        
        // Draw the parameter dots on the overlay canvas
        drawDots();
        
        // Update debug info if visible
        if (debugInfo.style.display !== 'none' && debugInfo.style.color !== 'red') {
            let modeDescription = "Custom Mode";
            
            // Check for Mandelbrot or Julia set modes
            if (customParameters["c"]) {
                if (zControlMode === "dot" && customParameterModes["c"] === "pixel") {
                    modeDescription = "Mandelbrot Set";
                } else if (zControlMode === "pixel" && customParameterModes["c"] === "dot") {
                    modeDescription = "Julia Set";
                }
            }
            
            // Format complex number for display
            function formatComplex(value) {
                const realPart = value[0].toFixed(4);
                const imagPart = Math.abs(value[1]).toFixed(4);
                const sign = value[1] >= 0 ? '+' : '-';
                return `${realPart}${sign}${imagPart}i`;
            }
            
            // Format center as complex number
            const centerComplex = formatComplex(center);
            
            let customParamsDebug = '';
            Object.keys(customParameters).forEach(name => {
                const param = customParameters[name];
                const mode = customParameterModes[name] || 'dot';
                
                if (mode === 'pixel') {
                    customParamsDebug += `${name}: (pixel position) [${mode}]<br>`;
                } else {
                    const valueDisplay = formatComplex(param);
                    customParamsDebug += `${name}: ${valueDisplay} [${mode}]<br>`;
                }
            });
            
            // Format initialZ as complex number
            const initialZDisplay = zControlMode === 'pixel' ? '(pixel position)' : formatComplex(initialZ);
            
            debugInfo.innerHTML = `Debug info:<br>
                Mode: ${modeDescription}<br>
                Resolution: ${glCanvas.width}x${glCanvas.height}<br>
                Center: ${centerComplex}<br>
                Zoom: ${zoom.toFixed(2)}<br>
                Max Iterations: ${maxIterations}<br>
                Initial Z: ${initialZDisplay} [${zControlMode}]<br>
                ${customParamsDebug}
                Current Function: ${currentFunctionGLSL}`;
        }
        
    } catch (error) {
        console.error('Draw error:', error);
        showError('Draw error: ' + error.message);
    }
}

// Initialize the application when the DOM is fully loaded
document.addEventListener('DOMContentLoaded', function() {
    // Ensure debug panel is hidden on load
    const debugPanel = document.getElementById('debug');
    if (debugPanel) {
        debugPanel.style.display = 'none';
    }
    
    // Initialize the application
    init().catch(error => {
        console.error('Application error:', error);
        showError('Application error: ' + error.message);
    });
});

// Function to update the function examples in the UI
function updateFunctionExamples() {
    const examples = document.querySelectorAll('.example');
    examples.forEach(example => {
        // Update the event listener
        const oldFunc = example.getAttribute('data-func');
        example.addEventListener('click', () => {
            const functionInput = document.getElementById('function-input');
            functionInput.value = oldFunc;
            
            // Special handling for Newton's Method: add r1, r2, r3 parameters if they don't exist
            if (oldFunc.includes("Newton's method")) {
                // Define the roots as three points evenly spaced on a circle
                if (!customParameters["r1"]) {
                    customParameters["r1"] = [1.0, 0.0]; // 1 + 0i
                    customParameterModes["r1"] = "dot";
                    customParameterColors["r1"] = getRandomColor("r1");
                }
                if (!customParameters["r2"]) {
                    customParameters["r2"] = [-0.5, 0.866]; // -0.5 + 0.866i (120 degrees)
                    customParameterModes["r2"] = "dot";
                    customParameterColors["r2"] = getRandomColor("r2");
                }
                if (!customParameters["r3"]) {
                    customParameters["r3"] = [-0.5, -0.866]; // -0.5 - 0.866i (240 degrees)
                    customParameterModes["r3"] = "dot";
                    customParameterColors["r3"] = getRandomColor("r3");
                }
                
                // Switch to convergence coloring mode for Newton's Method
                coloringMode = 1;
                
                // Set z₀ to pixel mode for Newton's Method
                zControlMode = "pixel";
                
                // Update the radio buttons if they exist
                const convergenceRadio = document.getElementById('convergence-coloring');
                if (convergenceRadio) {
                    convergenceRadio.checked = true;
                }
                
                // Refresh the UI to show the new parameters
                refreshParametersUI();
                
                // Show info about the added parameters
                showInfo("Added r1, r2, r3 parameters for Newton's Method. Try dragging the dots to change the roots!");
            }
            
            document.getElementById('apply-function').click();
        });
    });
}

// Function to set up the coloring mode selector
function setupColoringModeSelector() {
    // Create the coloring mode section
    const controlsDiv = document.getElementById('controls');
    const parametersSection = document.querySelector('.section-title');
    
    // Create the coloring mode section
    const coloringSectionDiv = document.createElement('div');
    coloringSectionDiv.className = 'section-title';
    coloringSectionDiv.textContent = 'Coloring Mode';
    
    // Create the coloring mode selector
    const coloringSelectorDiv = document.createElement('div');
    coloringSelectorDiv.className = 'coloring-selector';
    coloringSelectorDiv.style.padding = '10px 0';
    coloringSelectorDiv.style.marginBottom = '15px';
    
    // Create the radio buttons for coloring modes
    const escapeRadio = document.createElement('input');
    escapeRadio.type = 'radio';
    escapeRadio.id = 'escape-coloring';
    escapeRadio.name = 'coloring-mode';
    escapeRadio.value = '0';
    escapeRadio.checked = coloringMode === 0;
    
    const escapeLabel = document.createElement('label');
    escapeLabel.htmlFor = 'escape-coloring';
    escapeLabel.textContent = 'Escape';
    escapeLabel.style.marginRight = '20px';
    escapeLabel.style.cursor = 'pointer';
    
    const convergenceRadio = document.createElement('input');
    convergenceRadio.type = 'radio';
    convergenceRadio.id = 'convergence-coloring';
    convergenceRadio.name = 'coloring-mode';
    convergenceRadio.value = '1';
    convergenceRadio.checked = coloringMode === 1;
    
    const convergenceLabel = document.createElement('label');
    convergenceLabel.htmlFor = 'convergence-coloring';
    convergenceLabel.textContent = 'Convergence';
    convergenceLabel.style.cursor = 'pointer';
    
    // Add event listeners
    escapeRadio.addEventListener('change', () => {
        if (escapeRadio.checked) {
            coloringMode = 0;
            draw();
        }
    });
    
    convergenceRadio.addEventListener('change', () => {
        if (convergenceRadio.checked) {
            coloringMode = 1;
            draw();
        }
    });
    
    // Assemble the coloring selector
    coloringSelectorDiv.appendChild(escapeRadio);
    coloringSelectorDiv.appendChild(escapeLabel);
    coloringSelectorDiv.appendChild(convergenceRadio);
    coloringSelectorDiv.appendChild(convergenceLabel);
    
    // Insert the coloring section before the parameters section
    controlsDiv.insertBefore(coloringSelectorDiv, parametersSection);
    controlsDiv.insertBefore(coloringSectionDiv, coloringSelectorDiv);
}

// Reset view to default
function resetView() {
    center = [0.0, 0.0];
    zoom = 1.0;
    
    // Reset c parameter if it exists
    if (customParameters["c"]) {
        customParameters["c"] = [-0.7559, -0.0709];
    }
    
    // Reset z₀ parameter
    initialZ = [0.0, 0.0];
    
    // Redraw
    draw();
    
    showInfo('View reset');
} 