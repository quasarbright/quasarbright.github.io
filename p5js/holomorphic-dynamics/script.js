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
let maxIterations = 500;
let center = [-0.5, 0.0];
let zoom = 1.0;

// Mouse interaction
let isDragging = false;
let lastMousePos = { x: 0, y: 0 };

// Debug info
let debugInfo = document.createElement('div');

// Load shader from source
function loadShader(gl, type, source) {
    const shader = gl.createShader(type);
    gl.shaderSource(shader, source);
    gl.compileShader(shader);
    
    if (!gl.getShaderParameter(shader, gl.COMPILE_STATUS)) {
        const error = gl.getShaderInfoLog(shader);
        console.error('Shader compilation error:', error);
        debugInfo.innerHTML += `<br>Shader compilation error: ${error}`;
        gl.deleteShader(shader);
        return null;
    }
    
    return shader;
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
        debugInfo.innerHTML += `<br>Error loading shader from ${url}: ${error.message}`;
        throw error;
    }
}

// Initialize WebGL
async function init() {
    // Set up debug info
    debugInfo.style.position = 'absolute';
    debugInfo.style.top = '10px';
    debugInfo.style.left = '10px';
    debugInfo.style.color = 'red';
    debugInfo.style.fontFamily = 'monospace';
    debugInfo.style.backgroundColor = 'rgba(0,0,0,0.7)';
    debugInfo.style.padding = '10px';
    debugInfo.style.zIndex = '1000';
    debugInfo.innerHTML = 'Debug info:';
    document.body.appendChild(debugInfo);

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
        const error = 'WebGL not supported in your browser';
        console.error(error);
        debugInfo.innerHTML += `<br>${error}`;
        return;
    }
    
    debugInfo.innerHTML += '<br>WebGL context created successfully';
    
    try {
        // Load shaders
        debugInfo.innerHTML += '<br>Loading shaders...';
        const vertexShaderSource = await loadShaderFromFile('vertex.glsl');
        const fragmentShaderSource = await loadShaderFromFile('fragment.glsl');
        
        debugInfo.innerHTML += '<br>Shaders loaded successfully';
        debugInfo.innerHTML += `<br>Vertex shader length: ${vertexShaderSource.length}`;
        debugInfo.innerHTML += `<br>Fragment shader length: ${fragmentShaderSource.length}`;
        
        // Create shader program
        const vertexShader = loadShader(gl, gl.VERTEX_SHADER, vertexShaderSource);
        const fragmentShader = loadShader(gl, gl.FRAGMENT_SHADER, fragmentShaderSource);
        
        if (!vertexShader || !fragmentShader) {
            throw new Error('Shader compilation failed');
        }
        
        program = gl.createProgram();
        gl.attachShader(program, vertexShader);
        gl.attachShader(program, fragmentShader);
        gl.linkProgram(program);
        
        if (!gl.getProgramParameter(program, gl.LINK_STATUS)) {
            const error = gl.getProgramInfoLog(program);
            console.error('Program linking error:', error);
            debugInfo.innerHTML += `<br>Program linking error: ${error}`;
            return;
        }
        
        debugInfo.innerHTML += '<br>Shader program linked successfully';
        
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
        
        // Get attribute and uniform locations
        positionAttributeLocation = gl.getAttribLocation(program, 'a_position');
        resolutionUniformLocation = gl.getUniformLocation(program, 'u_resolution');
        maxIterationsUniformLocation = gl.getUniformLocation(program, 'u_maxIterations');
        centerUniformLocation = gl.getUniformLocation(program, 'u_center');
        zoomUniformLocation = gl.getUniformLocation(program, 'u_zoom');
        
        debugInfo.innerHTML += `<br>Uniform locations: res=${!!resolutionUniformLocation}, maxIter=${!!maxIterationsUniformLocation}, center=${!!centerUniformLocation}, zoom=${!!zoomUniformLocation}`;
        
        // Set up mouse events for interaction
        setupMouseEvents(canvas);
        
        // Initial resize
        resizeCanvas();
        
        // Initial draw
        draw();
        
        // Check if the canvas is still black after drawing
        setTimeout(() => {
            const pixels = new Uint8Array(4);
            gl.readPixels(canvas.width/2, canvas.height/2, 1, 1, gl.RGBA, gl.UNSIGNED_BYTE, pixels);
            debugInfo.innerHTML += `<br>Center pixel: rgba(${pixels[0]}, ${pixels[1]}, ${pixels[2]}, ${pixels[3]})`;
            
            // If all pixels are 0, the screen is black
            if (pixels[0] === 0 && pixels[1] === 0 && pixels[2] === 0) {
                debugInfo.innerHTML += '<br>Screen is black. Try adjusting parameters.';
            }
        }, 500);
        
    } catch (error) {
        console.error('Initialization error:', error);
        debugInfo.innerHTML += `<br>Initialization error: ${error.message}`;
    }
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
        
        // Update debug info
        debugInfo.innerHTML = `Debug info:<br>
            Resolution: ${gl.canvas.width}x${gl.canvas.height}<br>
            Center: (${center[0].toFixed(4)}, ${center[1].toFixed(4)})<br>
            Zoom: ${zoom.toFixed(2)}<br>
            Max Iterations: ${maxIterations}`;
    } catch (error) {
        console.error('Draw error:', error);
        debugInfo.innerHTML += `<br>Draw error: ${error.message}`;
    }
}

// Start the application
init().catch(error => {
    console.error('Application error:', error);
    if (debugInfo) {
        debugInfo.innerHTML += `<br>Application error: ${error.message}`;
    }
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