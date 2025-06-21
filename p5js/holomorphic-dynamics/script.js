// WebGL program for Mandelbrot set visualization
let gl;
let program;
let positionBuffer;
let positionAttributeLocation;
let resolutionUniformLocation;
let maxIterationsUniformLocation;
let centerUniformLocation;
let zoomUniformLocation;

// Mandelbrot parameters
let maxIterations = 100;
let center = [-0.5, 0.0];
let zoom = 1.0;

// Mouse interaction
let isDragging = false;
let lastMousePos = { x: 0, y: 0 };

// Load shader from source
function loadShader(gl, type, source) {
    const shader = gl.createShader(type);
    gl.shaderSource(shader, source);
    gl.compileShader(shader);
    
    if (!gl.getShaderParameter(shader, gl.COMPILE_STATUS)) {
        console.error('Shader compilation error:', gl.getShaderInfoLog(shader));
        gl.deleteShader(shader);
        return null;
    }
    
    return shader;
}

// Load shaders from file
async function loadShaderFromFile(url) {
    const response = await fetch(url);
    return response.text();
}

// Initialize WebGL
async function init() {
    const canvas = document.getElementById('glCanvas');
    
    // Handle canvas resize
    function resizeCanvas() {
        canvas.width = window.innerWidth;
        canvas.height = window.innerHeight;
        gl.viewport(0, 0, canvas.width, canvas.height);
        draw();
    }
    
    window.addEventListener('resize', resizeCanvas);
    
    // Initialize WebGL context
    gl = canvas.getContext('webgl');
    if (!gl) {
        alert('WebGL not supported in your browser');
        return;
    }
    
    // Load shaders
    const vertexShaderSource = await loadShaderFromFile('vertex.glsl');
    const fragmentShaderSource = await loadShaderFromFile('fragment.glsl');
    
    // Create shader program
    const vertexShader = loadShader(gl, gl.VERTEX_SHADER, vertexShaderSource);
    const fragmentShader = loadShader(gl, gl.FRAGMENT_SHADER, fragmentShaderSource);
    
    program = gl.createProgram();
    gl.attachShader(program, vertexShader);
    gl.attachShader(program, fragmentShader);
    gl.linkProgram(program);
    
    if (!gl.getProgramParameter(program, gl.LINK_STATUS)) {
        console.error('Program linking error:', gl.getProgramInfoLog(program));
        return;
    }
    
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
    
    // Set up mouse events for interaction
    setupMouseEvents(canvas);
    
    // Initial resize
    resizeCanvas();
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
                maxIterations = Math.min(1000, maxIterations + 10);
                break;
            case '-':
                maxIterations = Math.max(10, maxIterations - 10);
                break;
            case 'r':
                // Reset view
                center = [-0.5, 0.0];
                zoom = 1.0;
                maxIterations = 100;
                break;
            default:
                return;
        }
        
        draw();
    });
}

// Draw the Mandelbrot set
function draw() {
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
}

// Start the application
init().catch(console.error);

// Add UI info
const infoDiv = document.createElement('div');
infoDiv.style.position = 'absolute';
infoDiv.style.bottom = '10px';
infoDiv.style.left = '10px';
infoDiv.style.color = 'white';
infoDiv.style.fontFamily = 'monospace';
infoDiv.style.backgroundColor = 'rgba(0,0,0,0.5)';
infoDiv.style.padding = '5px';
infoDiv.innerHTML = `
    Mouse: drag to pan, wheel to zoom<br>
    +/-: increase/decrease iterations<br>
    R: reset view
`;
document.body.appendChild(infoDiv); 