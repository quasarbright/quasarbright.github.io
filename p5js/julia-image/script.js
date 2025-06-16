// WebGL variables
let gl;
let program;
let positionBuffer;
let texture;
let isImageLoaded = false;

// Julia set parameters
const initialJuliaC = { x: -0.549, y: -0.452 };
let juliaC = {...initialJuliaC};
let maxIterations = 50;
let tileSize = 2.0;
let isMouseDragging = false;

// Canvas view parameters
let centerX = 0;
let centerY = 0;
let zoomLevel = 1.0;

// DOM elements
const canvas = document.getElementById('glCanvas');
const fileInput = document.getElementById('imageUpload');
const resetButton = document.getElementById('resetButton');
const saveButton = document.getElementById('saveButton');
const cValueDisplay = document.getElementById('cValue');

// Make canvas fullscreen
function resizeCanvas() {
    canvas.width = window.innerWidth;
    canvas.height = window.innerHeight;
    if (gl) {
        gl.viewport(0, 0, canvas.width, canvas.height);
        if (isImageLoaded) render();
    }
}

// Shader locations
let u_image;
let u_resolution;
let u_juliaC;
let u_tileSize;
let u_maxIterations;
let u_center;
let u_zoom;

// Initialize WebGL when the page loads
window.onload = function() {
    // Set up fullscreen canvas
    resizeCanvas();
    window.addEventListener('resize', resizeCanvas);
    
    initWebGL();
    setupEventListeners();
    
    // Update initial c value display
    cValueDisplay.textContent = `c: ${juliaC.x.toFixed(3)} + ${juliaC.y.toFixed(3)}i`;
    
    // Show initial message on canvas
    const ctx = canvas.getContext('2d');
    ctx.fillStyle = '#111';
    ctx.fillRect(0, 0, canvas.width, canvas.height);
    ctx.font = '20px Arial';
    ctx.fillStyle = 'white';
    ctx.textAlign = 'center';
    ctx.textBaseline = 'middle';
    ctx.fillText('Upload an image to begin', canvas.width / 2, canvas.height / 2);
};

// Initialize WebGL
function initWebGL() {
    // Initialize the GL context
    gl = canvas.getContext('webgl');
    
    // Only continue if WebGL is available and working
    if (!gl) {
        alert('Unable to initialize WebGL. Your browser may not support it.');
        return;
    }
    
    // Create shader program
    const vertexShaderSource = document.getElementById('vertexShader').textContent;
    const fragmentShaderSource = document.getElementById('fragmentShader').textContent;
    program = createProgram(gl, vertexShaderSource, fragmentShaderSource);
    
    // Look up uniform locations
    u_image = gl.getUniformLocation(program, 'u_image');
    u_resolution = gl.getUniformLocation(program, 'u_resolution');
    u_juliaC = gl.getUniformLocation(program, 'u_juliaC');
    u_tileSize = gl.getUniformLocation(program, 'u_tileSize');
    u_maxIterations = gl.getUniformLocation(program, 'u_maxIterations');
    u_center = gl.getUniformLocation(program, 'u_center');
    u_zoom = gl.getUniformLocation(program, 'u_zoom');
    
    // Create a buffer for the position of the rectangle corners
    positionBuffer = gl.createBuffer();
    gl.bindBuffer(gl.ARRAY_BUFFER, positionBuffer);
    
    // Set rectangle vertices (covers the entire canvas)
    const positions = [
        -1.0, -1.0,
         1.0, -1.0,
        -1.0,  1.0,
        -1.0,  1.0,
         1.0, -1.0,
         1.0,  1.0,
    ];
    gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(positions), gl.STATIC_DRAW);
}

// Helper function to convert screen coordinates to complex plane coordinates
function toComplex(x, y) {
    // Convert screen coordinates to normalized device coordinates (-1 to 1)
    const ndcX = (2.0 * x / canvas.width) - 1.0;
    const ndcY = 1.0 - (2.0 * y / canvas.height); // Flip Y since screen Y is top-down
    
    // Apply aspect ratio correction
    const aspectRatio = canvas.width / canvas.height;
    let correctedX = ndcX;
    let correctedY = ndcY;
    
    if (aspectRatio > 1.0) {
        correctedX *= aspectRatio;
    } else {
        correctedY /= aspectRatio;
    }
    
    // Apply zoom and center offset
    return {
        x: (correctedX * 2.0) / zoomLevel + centerX,
        y: (correctedY * 2.0) / zoomLevel + centerY
    };
}

// Helper function to convert complex coordinates to screen coordinates
function fromComplex(cx, cy) {
    // Apply aspect ratio correction
    const aspectRatio = canvas.width / canvas.height;
    let correctedX = cx - centerX;
    let correctedY = cy - centerY;
    
    // Apply zoom
    correctedX = (correctedX * zoomLevel) / 2.0;
    correctedY = (correctedY * zoomLevel) / 2.0;
    
    // Unapply aspect ratio correction
    if (aspectRatio > 1.0) {
        correctedX /= aspectRatio;
    } else {
        correctedY *= aspectRatio;
    }
    
    // Convert normalized device coordinates to screen coordinates
    return {
        x: (correctedX + 1.0) * canvas.width / 2.0,
        y: (1.0 - correctedY) * canvas.height / 2.0 // Flip Y since screen Y is top-down
    };
}

// Linear interpolation helper
function lerp(a, b, r) {
    return a + r * (b - a);
}

// Set up event listeners
function setupEventListeners() {
    // File upload handler
    fileInput.addEventListener('change', handleFileUpload);
    
    // Canvas mouse events
    canvas.addEventListener('mousedown', function(e) {
        if (!isImageLoaded) return;
        isMouseDragging = true;
        updateJuliaC(e);
    });
    
    canvas.addEventListener('mousemove', function(e) {
        if (!isImageLoaded || !isMouseDragging) return;
        updateJuliaC(e);
    });
    
    window.addEventListener('mouseup', function() {
        isMouseDragging = false;
    });
    
    // Zoom with mouse wheel
    canvas.addEventListener('wheel', function(e) {
        e.preventDefault();
        if (!isImageLoaded) return;
        
        const rect = canvas.getBoundingClientRect();
        const mouseX = e.clientX - rect.left;
        const mouseY = e.clientY - rect.top;
        
        // Get complex coordinates at mouse position before zoom
        const complexBefore = toComplex(mouseX, mouseY);
        
        // Determine zoom direction and factor
        const zoomFactor = e.deltaY > 0 ? 0.9 : 1.1; // Zoom out (0.9) or in (1.1)
        
        // Update zoom level
        zoomLevel *= zoomFactor;
        
        // Get complex coordinates at same mouse position after zoom
        const complexAfter = toComplex(mouseX, mouseY);
        
        // Adjust center to keep mouse position fixed in complex plane
        centerX += complexBefore.x - complexAfter.x;
        centerY += complexBefore.y - complexAfter.y;
        
        // Re-render
        render();
    });
    
    // Reset button
    resetButton.addEventListener('click', function() {
        if (!isImageLoaded) return;
        
        // Reset parameters
        juliaC = {...initialJuliaC}
        maxIterations = 50;
        tileSize = 2.0;
        centerX = 0;
        centerY = 0;
        zoomLevel = 1.0;
        
        // Update UI
        cValueDisplay.textContent = `c: ${juliaC.x.toFixed(3)} + ${juliaC.y.toFixed(3)}i`;
        
        // Re-render
        render();
    });
    
    // Save button
    saveButton.addEventListener('click', function() {
        if (!isImageLoaded) return;
        
        // Create a temporary link to download the canvas content
        const link = document.createElement('a');
        link.download = 'julia_transformed_image.png';
        link.href = canvas.toDataURL('image/png');
        link.click();
    });
    
    // Keyboard shortcuts
    window.addEventListener('keydown', function(e) {
        if (!isImageLoaded) return;
        
        switch(e.key) {
            case 's':
            case 'S':
                // Save image
                saveButton.click();
                break;
        }
    });
}

// Handle file upload
function handleFileUpload(event) {
    const file = event.target.files[0];
    if (!file || !file.type.match(/^image/)) return;
    
    const reader = new FileReader();
    reader.onload = function(e) {
        const image = new Image();
        image.onload = function() {
            // Create texture from the uploaded image
            texture = gl.createTexture();
            gl.bindTexture(gl.TEXTURE_2D, texture);
            
            // Set texture parameters
            gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE);
            gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE);
            gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.LINEAR);
            gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.LINEAR);
            
            // Upload the image into the texture
            gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA, gl.RGBA, gl.UNSIGNED_BYTE, image);
            
            // Set image as loaded and enable buttons
            isImageLoaded = true;
            resetButton.disabled = false;
            saveButton.disabled = false;
            
            // Render the initial state
            render();
        };
        image.src = e.target.result;
    };
    reader.readAsDataURL(file);
}

// Update Julia set parameter based on mouse position
function updateJuliaC(event) {
    const rect = canvas.getBoundingClientRect();
    const x = event.clientX - rect.left;
    const y = event.clientY - rect.top;
    
    // Use the new coordinate conversion for consistency
    const complexCoords = toComplex(x, y);
    juliaC.x = complexCoords.x;
    juliaC.y = complexCoords.y;
    
    // Update display
    cValueDisplay.textContent = `c: ${juliaC.x.toFixed(3)} + ${juliaC.y.toFixed(3)}i`;
    
    // Re-render
    render();
}

// Render the Julia set with the current parameters
function render() {
    // Set viewport and clear
    gl.viewport(0, 0, canvas.width, canvas.height);
    gl.clearColor(0, 0, 0, 1);
    gl.clear(gl.COLOR_BUFFER_BIT);
    
    // Use the shader program
    gl.useProgram(program);
    
    // Set up the position attribute
    const positionAttributeLocation = gl.getAttribLocation(program, 'a_position');
    gl.enableVertexAttribArray(positionAttributeLocation);
    gl.bindBuffer(gl.ARRAY_BUFFER, positionBuffer);
    gl.vertexAttribPointer(positionAttributeLocation, 2, gl.FLOAT, false, 0, 0);
    
    // Set the uniforms
    gl.uniform2f(u_resolution, canvas.width, canvas.height);
    gl.uniform2f(u_juliaC, juliaC.x, juliaC.y);
    gl.uniform1f(u_tileSize, tileSize);
    gl.uniform1i(u_maxIterations, maxIterations);
    gl.uniform2f(u_center, centerX, centerY);
    gl.uniform1f(u_zoom, zoomLevel);
    
    // Set the texture
    gl.activeTexture(gl.TEXTURE0);
    gl.bindTexture(gl.TEXTURE_2D, texture);
    gl.uniform1i(u_image, 0);
    
    // Draw the rectangle
    gl.drawArrays(gl.TRIANGLES, 0, 6);
}

// Create a shader program from vertex and fragment shader sources
function createProgram(gl, vertexShaderSource, fragmentShaderSource) {
    // Create and compile the vertex shader
    const vertexShader = gl.createShader(gl.VERTEX_SHADER);
    gl.shaderSource(vertexShader, vertexShaderSource);
    gl.compileShader(vertexShader);
    
    // Check if compilation was successful
    if (!gl.getShaderParameter(vertexShader, gl.COMPILE_STATUS)) {
        console.error('Vertex shader compilation failed:', gl.getShaderInfoLog(vertexShader));
        gl.deleteShader(vertexShader);
        return null;
    }
    
    // Create and compile the fragment shader
    const fragmentShader = gl.createShader(gl.FRAGMENT_SHADER);
    gl.shaderSource(fragmentShader, fragmentShaderSource);
    gl.compileShader(fragmentShader);
    
    // Check if compilation was successful
    if (!gl.getShaderParameter(fragmentShader, gl.COMPILE_STATUS)) {
        console.error('Fragment shader compilation failed:', gl.getShaderInfoLog(fragmentShader));
        gl.deleteShader(vertexShader);
        gl.deleteShader(fragmentShader);
        return null;
    }
    
    // Create the shader program
    const program = gl.createProgram();
    gl.attachShader(program, vertexShader);
    gl.attachShader(program, fragmentShader);
    gl.linkProgram(program);
    
    // Check if linking was successful
    if (!gl.getProgramParameter(program, gl.LINK_STATUS)) {
        console.error('Shader program linking failed:', gl.getProgramInfoLog(program));
        gl.deleteProgram(program);
        gl.deleteShader(vertexShader);
        gl.deleteShader(fragmentShader);
        return null;
    }
    
    return program;
} 