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
let paramCUniformLocation;

// Custom parameters storage
let customParameters = {};
let customParameterUniformLocations = {};

// Visualization parameters
let maxIterations = 100;
let center = [-0.5, 0.0];
let zoom = 1.0;
let currentFunctionGLSL = "return csquare(z) + c;"; // Default Mandelbrot function
let initialZ = [0.0, 0.0]; // Initial value of z (z_0)
let paramC = [0.0, 0.0];   // Parameter c for Julia sets

// Parameter control modes
let zControlMode = "dot";  // "dot" or "pixel"
let cControlMode = "pixel"; // "dot" or "pixel"
// Custom parameter control modes
let customParameterModes = {}; // name -> "dot", "pixel", or "fixed"
let customParameterColors = {}; // name -> color for dot

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
        
        // Create a preamble that defines local variables for each custom parameter
        let paramPreamble = '';
        Object.keys(customParameters).forEach(paramName => {
            paramPreamble += `    vec2 ${paramName} = u_param_${paramName};\n`;
        });
        
        // Create the full function code with parameter preamble
        const fullFunctionCode = `vec2 complex_function(vec2 z, vec2 c) {\n${paramPreamble}    ${functionCode}\n}`;
        
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
        const placeholderRegex = /\/\/ USER_FUNCTION_PLACEHOLDER\s*vec2 complex_function\(vec2 z, vec2 c\) \{[\s\S]*?\}/;
        const placeholderMatch = fragmentShaderSource.match(placeholderRegex);
        
        if (!placeholderMatch) {
            console.error("Could not find function placeholder in shader template");
            return {
                success: false,
                message: "Error: Could not find function placeholder in shader template"
            };
        }
        
        // Replace the placeholder with the user function
        fragmentShaderSource = fragmentShaderSource.replace(
            placeholderRegex,
            fullFunctionCode
        );
        
        console.log("Fragment shader with new function:", fragmentShaderSource.substring(0, 500) + "...");
        
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
        'z', 'c', 'abs', 'sin', 'cos', 'tan', 'exp', 'log',
        'length', 'dot', 'cross', 'normalize', 'mix', 'clamp',
        'min', 'max', 'fract', 'floor', 'ceil', 'mod',
        'csquare', 'ccube', 'cpow', 'cmul', 'cdiv', 'csin', 'ccos', 'cexp', 'clog'
    ]);
    
    // We don't need to check for direct parameter names anymore since we're adding them as local variables
    
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
        return `Warning: You're using parameter(s) that haven't been added yet: ${missingParams.join(', ')}. Add them in the Parameters tab.`;
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
    paramCUniformLocation = gl.getUniformLocation(program, 'u_paramC');
    
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
    debugInfo.style.display = 'block';
    debugInfo.innerHTML = message;
    debugInfo.style.color = 'white';
    
    // Hide after 3 seconds
    setTimeout(() => {
        if (debugInfo.innerHTML === message) {
            debugInfo.style.display = 'none';
        }
    }, 3000);
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
    if (param === 'z') {
        dotPos = complexToScreen(initialZ[0], initialZ[1], glCanvas);
    } else if (param === 'c') {
        dotPos = complexToScreen(paramC[0], paramC[1], glCanvas);
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
        const zDotRadius = (isHoveringDot && activeDot === "z") ? 8 : 6;
        
        // Draw the dot with white outline
        overlayCtx.beginPath();
        overlayCtx.arc(zDotScreenPos.x, zDotScreenPos.y, zDotRadius, 0, Math.PI * 2);
        overlayCtx.fillStyle = '#00AAFF'; // Blue
        overlayCtx.fill();
        overlayCtx.lineWidth = 2;
        overlayCtx.strokeStyle = 'white';
        overlayCtx.stroke();
        
        // Add a label
        overlayCtx.fillStyle = 'white';
        overlayCtx.font = '12px Arial';
        overlayCtx.fillText('z₀', zDotScreenPos.x + zDotRadius + 2, zDotScreenPos.y - zDotRadius - 2);
    }
    
    // Draw c dot if in dot mode
    if (cControlMode === "dot") {
        const cDotScreenPos = complexToScreen(paramC[0], paramC[1], glCanvas);
        const cDotRadius = (isHoveringDot && activeDot === "c") ? 8 : 6;
        
        // Draw the dot with white outline
        overlayCtx.beginPath();
        overlayCtx.arc(cDotScreenPos.x, cDotScreenPos.y, cDotRadius, 0, Math.PI * 2);
        overlayCtx.fillStyle = '#FF5500'; // Orange
        overlayCtx.fill();
        overlayCtx.lineWidth = 2;
        overlayCtx.strokeStyle = 'white';
        overlayCtx.stroke();
        
        // Add a label
        overlayCtx.fillStyle = 'white';
        overlayCtx.font = '12px Arial';
        overlayCtx.fillText('c', cDotScreenPos.x + cDotRadius + 2, cDotScreenPos.y - cDotRadius - 2);
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

// Initialize WebGL
async function init() {
    // Set up debug info
    debugInfo = document.getElementById('debug');
    
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
        
        // Set up tab switching
        setupTabSwitching();
        
        // Set up mouse events for interaction
        setupMouseEvents();
        
        // Set up function input
        setupFunctionInput();
        
        // Set up parameter controls
        setupParameterControls();
        
        // Initial resize
        resizeCanvases();
        
        // Get the initial function from the input field
        const functionInput = document.getElementById('function-input');
        currentFunctionGLSL = functionInput.value;
        
        // Apply the initial function to create the shader program
        const result = applyFunctionToShader(currentFunctionGLSL);
        if (!result.success) {
            showError(result.message);
        }
        
        // Hide debug info after initial setup
        setTimeout(() => {
            debugInfo.style.display = 'none';
        }, 1000);
        
    } catch (error) {
        console.error('Initialization error:', error);
        showError('Initialization error: ' + error.message);
    }
}

// Set up tab switching
function setupTabSwitching() {
    const tabs = document.querySelectorAll('.tab');
    const tabContents = document.querySelectorAll('.tab-content');
    
    tabs.forEach(tab => {
        tab.addEventListener('click', () => {
            // Remove active class from all tabs and content
            tabs.forEach(t => t.classList.remove('active'));
            tabContents.forEach(content => content.classList.remove('active'));
            
            // Add active class to clicked tab
            tab.classList.add('active');
            
            // Add active class to corresponding content
            const tabId = tab.getAttribute('data-tab');
            const content = document.getElementById(tabId + '-tab');
            if (content) {
                content.classList.add('active');
            }
        });
    });
}

// Set up function input
function setupFunctionInput() {
    const functionInput = document.getElementById('function-input');
    const applyButton = document.getElementById('apply-function');
    const examples = document.querySelectorAll('.example');
    
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
    // Z control radio buttons
    const zRadios = document.querySelectorAll('input[name="z-control"]');
    zRadios.forEach(radio => {
        radio.addEventListener('change', () => {
            zControlMode = radio.value;
            draw();
        });
    });
    
    // C control radio buttons
    const cRadios = document.querySelectorAll('input[name="c-control"]');
    cRadios.forEach(radio => {
        radio.addEventListener('change', () => {
            cControlMode = radio.value;
            draw();
        });
    });
    
    // Custom parameters
    setupCustomParameterControls();
}

// Set up custom parameter controls
function setupCustomParameterControls() {
    const container = document.getElementById('custom-params-container');
    const newParamName = document.getElementById('new-param-name');
    const newParamValueReal = document.getElementById('new-param-value-real');
    const newParamValueImag = document.getElementById('new-param-value-imag');
    const addParamBtn = document.getElementById('add-param-btn');
    
    // Function to refresh the custom parameters UI
    function refreshCustomParamsUI() {
        // Clear the container
        container.innerHTML = '';
        
        // Add each parameter
        Object.keys(customParameters).forEach(paramName => {
            const param = customParameters[paramName];
            const mode = customParameterModes[paramName] || 'fixed';
            const color = customParameterColors[paramName] || getRandomColor(paramName);
            
            const paramRow = document.createElement('div');
            paramRow.className = 'custom-param-row';
            
            // Parameter name
            const nameElem = document.createElement('div');
            nameElem.className = 'custom-param-name';
            nameElem.textContent = paramName;
            paramRow.appendChild(nameElem);
            
            // Parameter value
            const valueElem = document.createElement('div');
            valueElem.className = 'custom-param-value';
            valueElem.textContent = `(${param[0].toFixed(2)}, ${param[1].toFixed(2)})`;
            paramRow.appendChild(valueElem);
            
            // Parameter controls
            const controlsElem = document.createElement('div');
            controlsElem.className = 'custom-param-controls';
            
            // Mode selection
            const modeSelect = document.createElement('select');
            modeSelect.innerHTML = `
                <option value="fixed" ${mode === 'fixed' ? 'selected' : ''}>Fixed</option>
                <option value="dot" ${mode === 'dot' ? 'selected' : ''}>Dot</option>
                <option value="pixel" ${mode === 'pixel' ? 'selected' : ''}>Pixel</option>
            `;
            modeSelect.addEventListener('change', () => {
                customParameterModes[paramName] = modeSelect.value;
                draw();
            });
            controlsElem.appendChild(modeSelect);
            
            // Color indicator for dot mode
            const colorIndicator = document.createElement('div');
            colorIndicator.className = 'color-indicator';
            colorIndicator.style.backgroundColor = color;
            colorIndicator.style.display = mode === 'dot' ? 'inline-block' : 'none';
            controlsElem.appendChild(colorIndicator);
            
            // Update modeSelect display when mode changes
            modeSelect.addEventListener('change', () => {
                colorIndicator.style.display = modeSelect.value === 'dot' ? 'inline-block' : 'none';
            });
            
            // Delete button
            const deleteBtn = document.createElement('button');
            deleteBtn.className = 'small-button';
            deleteBtn.textContent = 'Delete';
            deleteBtn.addEventListener('click', () => {
                delete customParameters[paramName];
                delete customParameterModes[paramName];
                delete customParameterColors[paramName];
                refreshCustomParamsUI();
                applyFunctionToShader(currentFunctionGLSL);
            });
            controlsElem.appendChild(deleteBtn);
            
            paramRow.appendChild(controlsElem);
            container.appendChild(paramRow);
        });
    }
    
    // Add new parameter button
    addParamBtn.addEventListener('click', () => {
        const name = newParamName.value.trim();
        const realValue = parseFloat(newParamValueReal.value) || 0;
        const imagValue = parseFloat(newParamValueImag.value) || 0;
        
        if (name) {
            // Add the parameter
            customParameters[name] = [realValue, imagValue];
            customParameterModes[name] = 'fixed'; // Default to fixed mode
            
            // Clear the input fields
            newParamName.value = '';
            newParamValueReal.value = '';
            newParamValueImag.value = '';
            
            // Refresh the UI
            refreshCustomParamsUI();
            
            // Recompile the shader with the new parameter
            applyFunctionToShader(currentFunctionGLSL);
        }
    });
    
    // Initial UI refresh
    refreshCustomParamsUI();
}

// Set up mouse events
function setupMouseEvents() {
    // Use the glCanvas for events, but we'll draw the dot on the overlayCanvas
    glCanvas.addEventListener('mousedown', (e) => {
        // Check if we're clicking on a dot
        if (zControlMode === "dot" && isNearDot(e.clientX, e.clientY, "z")) {
            isDraggingDot = true;
            activeDot = "z";
        } else if (cControlMode === "dot" && isNearDot(e.clientX, e.clientY, "c")) {
            isDraggingDot = true;
            activeDot = "c";
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
            
            if (activeDot === "z") {
                initialZ = complexCoords;
            } else if (activeDot === "c") {
                paramC = complexCoords;
            } else if (customParameters[activeDot]) {
                // Update custom parameter
                customParameters[activeDot] = complexCoords;
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
            if (zControlMode === "dot" && isNearDot(e.clientX, e.clientY, "z")) {
                isHoveringDot = true;
                activeDot = "z";
            } 
            // Then check c dot
            else if (cControlMode === "dot" && isNearDot(e.clientX, e.clientY, "c")) {
                isHoveringDot = true;
                activeDot = "c";
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
        
        // Adjust center to zoom toward mouse position
        center[0] = mouseComplex[0] - (mouseComplex[0] - center[0]) / zoomFactor;
        center[1] = mouseComplex[1] - (mouseComplex[1] - center[1]) / zoomFactor;
        
        draw();
    });
    
    // Add keyboard controls
    document.addEventListener('keydown', (e) => {
        switch (e.key) {
            case '+':
            case '=':
                maxIterations = Math.min(2000, maxIterations + 50);
                document.getElementById('iterationCount').textContent = maxIterations;
                break;
            case '-':
                maxIterations = Math.max(50, maxIterations - 50);
                document.getElementById('iterationCount').textContent = maxIterations;
                break;
            case 'r':
                // Reset view
                center = [-0.5, 0.0];
                zoom = 1.0;
                maxIterations = 100;
                initialZ = [0.0, 0.0]; // Reset initial Z
                paramC = [0.0, 0.0];   // Reset parameter C
                document.getElementById('iterationCount').textContent = maxIterations;
                break;
            case 'd':
                // Toggle debug info
                debugInfo.style.display = debugInfo.style.display === 'none' ? 'block' : 'none';
                break;
            case 'j':
                // Quick switch to Julia set mode
                if (cControlMode !== "dot") {
                    // Save current pixel position as c parameter
                    const centerOfScreen = screenToComplex(glCanvas.width/2, glCanvas.height/2, glCanvas);
                    paramC = centerOfScreen;
                    
                    // Update radio buttons
                    document.querySelector('input[name="z-control"][value="pixel"]').checked = true;
                    document.querySelector('input[name="c-control"][value="dot"]').checked = true;
                    
                    // Update control modes
                    zControlMode = "pixel";
                    cControlMode = "dot";
                    
                    draw();
                    showInfo('Switched to Julia set mode');
                }
                break;
            case 'm':
                // Quick switch to Mandelbrot set mode
                if (zControlMode !== "dot") {
                    // Update radio buttons
                    document.querySelector('input[name="z-control"][value="dot"]').checked = true;
                    document.querySelector('input[name="c-control"][value="pixel"]').checked = true;
                    
                    // Update control modes
                    zControlMode = "dot";
                    cControlMode = "pixel";
                    
                    draw();
                    showInfo('Switched to Mandelbrot set mode');
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
    const mode = customParameterModes[paramName] || 'fixed';
    
    if (mode === 'fixed') {
        // Return the stored value
        return customParameters[paramName] || [0.0, 0.0];
    } else if (mode === 'pixel') {
        // For pixel mode, return [0,0] to indicate that the shader should use pixel position
        return [0.0, 0.0];
    } else if (mode === 'dot') {
        // For dot mode, return the stored value
        return customParameters[paramName] || [0.0, 0.0];
    }
    
    return [0.0, 0.0];
}

// Get shader uniform values based on control modes
function getShaderParameters() {
    // For the shader, we need to pass either the actual value or (0,0) to indicate "use pixel position"
    let shaderInitialZ = [0.0, 0.0];
    let shaderParamC = [0.0, 0.0];
    
    // If z is controlled by a dot, pass the actual value
    if (zControlMode === "dot") {
        shaderInitialZ = initialZ;
    }
    
    // If c is controlled by a dot, pass the actual value
    if (cControlMode === "dot") {
        shaderParamC = paramC;
    }
    
    return { shaderInitialZ, shaderParamC };
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
        const { shaderInitialZ, shaderParamC } = getShaderParameters();
        
        // Set uniforms
        gl.uniform2f(resolutionUniformLocation, glCanvas.width, glCanvas.height);
        gl.uniform1i(maxIterationsUniformLocation, maxIterations);
        gl.uniform2f(centerUniformLocation, center[0], center[1]);
        gl.uniform1f(zoomUniformLocation, zoom);
        gl.uniform2f(initialZUniformLocation, shaderInitialZ[0], shaderInitialZ[1]);
        gl.uniform2f(paramCUniformLocation, shaderParamC[0], shaderParamC[1]);
        
        // Set custom parameter uniforms
        Object.keys(customParameterUniformLocations).forEach(paramName => {
            const location = customParameterUniformLocations[paramName];
            if (location !== null) {
                const paramValue = getCustomParameterValue(paramName);
                gl.uniform2f(location, paramValue[0], paramValue[1]);
            }
        });
        
        // Draw
        gl.drawArrays(gl.TRIANGLE_STRIP, 0, 4);
        
        // Draw the parameter dots on the overlay canvas
        drawDots();
        
        // Update debug info if visible
        if (debugInfo.style.display !== 'none' && debugInfo.style.color !== 'red') {
            const modeDescription = 
                zControlMode === "dot" && cControlMode === "pixel" ? "Mandelbrot Set" :
                zControlMode === "pixel" && cControlMode === "dot" ? "Julia Set" :
                "Custom Mode";
            
            let customParamsDebug = '';
            Object.keys(customParameters).forEach(name => {
                const param = customParameters[name];
                const mode = customParameterModes[name] || 'fixed';
                const actualValue = getCustomParameterValue(name);
                const effectiveValue = mode === 'pixel' ? 'pixel position' : `(${actualValue[0].toFixed(4)}, ${actualValue[1].toFixed(4)})`;
                
                customParamsDebug += `${name}: ${effectiveValue} [${mode}]<br>`;
            });
            
            debugInfo.innerHTML = `Debug info:<br>
                Mode: ${modeDescription}<br>
                Resolution: ${glCanvas.width}x${glCanvas.height}<br>
                Center: (${center[0].toFixed(4)}, ${center[1].toFixed(4)})<br>
                Zoom: ${zoom.toFixed(2)}<br>
                Max Iterations: ${maxIterations}<br>
                Initial Z: (${initialZ[0].toFixed(4)}, ${initialZ[1].toFixed(4)}) [${zControlMode}]<br>
                Param C: (${paramC[0].toFixed(4)}, ${paramC[1].toFixed(4)}) [${cControlMode}]<br>
                ${customParamsDebug}
                Current Function: ${currentFunctionGLSL}`;
        }
    } catch (error) {
        console.error('Draw error:', error);
        showError('Draw error: ' + error.message);
    }
}

// Start the application
init().catch(error => {
    console.error('Application error:', error);
    showError('Application error: ' + error.message);
});

// Add UI info
const infoDiv = document.createElement('div');
infoDiv.style.position = 'absolute';
infoDiv.style.bottom = '10px';
infoDiv.style.left = '10px';
infoDiv.style.color = 'white';
infoDiv.style.fontFamily = 'monospace';
infoDiv.style.backgroundColor = 'rgba(0,0,0,0.5)';
infoDiv.style.padding = '10px';
infoDiv.innerHTML = `
    Mouse: drag to pan, wheel to zoom<br>
    +/-: increase/decrease iterations (current: <span id="iterationCount">${maxIterations}</span>)<br>
    R: reset view<br>
    D: toggle debug info
`;
document.body.appendChild(infoDiv);

// Function to update the function examples in the UI
function updateFunctionExamples() {
    const examples = document.querySelectorAll('.example');
    examples.forEach(example => {
        // Update the event listener
        const oldFunc = example.getAttribute('data-func');
        example.addEventListener('click', () => {
            const functionInput = document.getElementById('function-input');
            functionInput.value = oldFunc;
            document.getElementById('apply-function').click();
        });
    });
} 