// WebGL program for complex function visualization
let gl;
let program;
let positionBuffer;
let positionAttributeLocation;
let resolutionUniformLocation;
let maxIterationsUniformLocation;
let centerUniformLocation;
let zoomUniformLocation;

// Visualization parameters
let maxIterations = 100;
let center = [-0.5, 0.0];
let zoom = 1.0;
let currentFunctionGLSL = "return csquare(z) + c;"; // Default Mandelbrot function

// Mouse interaction
let isDragging = false;
let lastMousePos = { x: 0, y: 0 };

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
    
    try {
        // Create the full function code
        const fullFunctionCode = `vec2 complex_function(vec2 z, vec2 c) {\n    ${functionCode}\n}`;
        
        console.log("Full function code:", fullFunctionCode);
        
        // Find the function placeholder in the template
        const placeholderRegex = /\/\/ USER_FUNCTION_PLACEHOLDER\s*vec2 complex_function\(vec2 z, vec2 c\) \{[\s\S]*?\}/;
        const match = fragmentShaderSourceTemplate.match(placeholderRegex);
        
        if (!match) {
            console.error("Could not find function placeholder in shader template");
            return {
                success: false,
                message: "Error: Could not find function placeholder in shader template"
            };
        }
        
        console.log("Found placeholder:", match[0]);
        
        // Replace the placeholder in the fragment shader
        const fragmentShaderSource = fragmentShaderSourceTemplate.replace(
            placeholderRegex,
            fullFunctionCode
        );
        
        console.log("Fragment shader with new function:", fragmentShaderSource.substring(0, 500) + "...");
        
        // Create and compile the new shader program
        const success = createShaderProgram(vertexShaderSource, fragmentShaderSource);
        
        if (success) {
            currentFunctionGLSL = functionCode;
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

// Initialize WebGL
async function init() {
    // Set up debug info
    debugInfo = document.getElementById('debug');
    
    const canvas = document.getElementById('glCanvas');
    
    // Handle canvas resize
    function resizeCanvas() {
        canvas.width = window.innerWidth;
        canvas.height = window.innerHeight;
        if (gl) {
            gl.viewport(0, 0, canvas.width, canvas.height);
            draw();
        }
    }
    
    window.addEventListener('resize', resizeCanvas);
    
    // Initialize WebGL context
    gl = canvas.getContext('webgl', { preserveDrawingBuffer: true });
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
        
        // Set up mouse events for interaction
        setupMouseEvents(canvas);
        
        // Set up function input
        setupFunctionInput();
        
        // Initial resize
        resizeCanvas();
        
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
    examples.forEach(example => {
        example.addEventListener('click', () => {
            const funcCode = example.dataset.func;
            functionInput.value = funcCode;
            applyButton.click();
        });
    });
}

// Set up mouse events
function setupMouseEvents(canvas) {
    canvas.addEventListener('mousedown', (e) => {
        isDragging = true;
        lastMousePos = { x: e.clientX, y: e.clientY };
    });
    
    canvas.addEventListener('mouseup', () => {
        isDragging = false;
    });
    
    canvas.addEventListener('mouseleave', () => {
        isDragging = false;
    });
    
    canvas.addEventListener('mousemove', (e) => {
        if (isDragging) {
            const dx = e.clientX - lastMousePos.x;
            const dy = e.clientY - lastMousePos.y;
            
            const aspectRatio = canvas.width / canvas.height;
            center[0] -= dx / zoom / canvas.width * 2 * aspectRatio;
            center[1] += dy / zoom / canvas.height * 2;
            
            lastMousePos = { x: e.clientX, y: e.clientY };
            draw();
        }
    });
    
    canvas.addEventListener('wheel', (e) => {
        e.preventDefault();
        
        // Get mouse position in normalized device coordinates
        const rect = canvas.getBoundingClientRect();
        const mouseX = (e.clientX - rect.left) / canvas.width * 2 - 1;
        const mouseY = 1 - (e.clientY - rect.top) / canvas.height * 2;
        
        // Convert to complex plane coordinates
        const aspectRatio = canvas.width / canvas.height;
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
                document.getElementById('iterationCount').textContent = maxIterations;
                break;
            case 'd':
                // Toggle debug info
                debugInfo.style.display = debugInfo.style.display === 'none' ? 'block' : 'none';
                break;
            default:
                return;
        }
        
        draw();
    });
}

// Draw the complex function visualization
function draw() {
    try {
        gl.clearColor(0.0, 0.0, 0.0, 1.0);
        gl.clear(gl.COLOR_BUFFER_BIT);
        
        gl.useProgram(program);
        
        // Set up position attribute
        gl.enableVertexAttribArray(positionAttributeLocation);
        gl.bindBuffer(gl.ARRAY_BUFFER, positionBuffer);
        gl.vertexAttribPointer(positionAttributeLocation, 2, gl.FLOAT, false, 0, 0);
        
        // Set uniforms
        gl.uniform2f(resolutionUniformLocation, gl.canvas.width, gl.canvas.height);
        gl.uniform1i(maxIterationsUniformLocation, maxIterations);
        gl.uniform2f(centerUniformLocation, center[0], center[1]);
        gl.uniform1f(zoomUniformLocation, zoom);
        
        // Draw
        gl.drawArrays(gl.TRIANGLE_STRIP, 0, 4);
        
        // Update debug info if visible
        if (debugInfo.style.display !== 'none' && debugInfo.style.color !== 'red') {
            debugInfo.innerHTML = `Debug info:<br>
                Resolution: ${gl.canvas.width}x${gl.canvas.height}<br>
                Center: (${center[0].toFixed(4)}, ${center[1].toFixed(4)})<br>
                Zoom: ${zoom.toFixed(2)}<br>
                Max Iterations: ${maxIterations}<br>
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