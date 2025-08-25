// GPU Conway's Game of Life
// Adapted from majority-automaton-gpu

// WebGL context
let gl;
// Shader program for rendering and updating
let renderProgram;
let updateProgram;
// Textures for ping-pong rendering
let textures = [];
let framebuffers = [];
// Current texture index (0 or 1)
let currentTexture = 0;
// Grid dimensions
let gridWidth, gridHeight;
// Animation frame ID for cancellation
let animationId;
let isPaused = false;
// Mouse position for interaction
let mouseX = 0;
let mouseY = 0;

// Vertex positions for a full-screen quad
const quadPositions = new Float32Array([
    -1.0, -1.0,
     1.0, -1.0,
    -1.0,  1.0,
     1.0,  1.0
]);

// Initialize WebGL when the page loads
window.onload = function() {
    const canvas = document.getElementById('glCanvas');
    // Make canvas fullscreen
    canvas.width = window.innerWidth;
    canvas.height = window.innerHeight;
    
    // Initialize WebGL context
    gl = canvas.getContext('webgl', { preserveDrawingBuffer: true });
    if (!gl) {
        console.error('WebGL not supported');
        return;
    }
    
    // Set grid dimensions to match the canvas pixel-for-pixel
    const scale = 2; // 2x2 pixels per cell for better visibility
    gridWidth = Math.floor(canvas.width / scale);
    gridHeight = Math.floor(canvas.height / scale);
    
    console.log('Grid dimensions:', gridWidth, 'x', gridHeight);
    
    // Initialize shaders and textures
    initShaders();
    initTextures();
    
    // Start the animation loop
    animate();
    
    // Handle window resize
    window.addEventListener('resize', onResize);
    
    // Add mouse interaction
    canvas.addEventListener('mousemove', function(e) {
        const rect = canvas.getBoundingClientRect();
        mouseX = (e.clientX - rect.left) / canvas.width;
        mouseY = 1.0 - (e.clientY - rect.top) / canvas.height; // Flip Y coordinate
    });
    

};

function onResize() {
    const canvas = document.getElementById('glCanvas');
    canvas.width = window.innerWidth;
    canvas.height = window.innerHeight;
    
    const scale = 2;
    gridWidth = Math.floor(canvas.width / scale);
    gridHeight = Math.floor(canvas.height / scale);
    
    gl.viewport(0, 0, canvas.width, canvas.height);
    
    // Reinitialize textures with new size
    initTextures();
}

function initShaders() {
    // Vertex shader (same for both programs)
    const vertexShaderSource = `
        attribute vec2 a_position;
        varying vec2 v_texCoord;
        
        void main() {
            // Convert from clip space (-1 to 1) to texture space (0 to 1)
            v_texCoord = (a_position + 1.0) * 0.5;
            gl_Position = vec4(a_position, 0.0, 1.0);
        }
    `;
    
    // Fragment shader for rendering (just display the texture)
    const renderFragmentShaderSource = `
        precision mediump float;
        varying vec2 v_texCoord;
        uniform sampler2D u_texture;
        
        void main() {
            float state = texture2D(u_texture, v_texCoord).r;
            // Live cells are white, dead cells are black
            gl_FragColor = vec4(state, state, state, 1.0);
        }
    `;
    
    // Fragment shader for Conway's Game of Life update
    const updateFragmentShaderSource = `
        precision mediump float;
        varying vec2 v_texCoord;
        uniform sampler2D u_texture;
        uniform vec2 u_textureSize;
        uniform float u_random;
        uniform vec2 u_mousePos;
        uniform float u_aspectRatio;
        
        // Get the state of a cell at the given texture coordinates
        float getState(vec2 coord) {
            // Use clamping for edges (cells outside are considered dead)
            coord = clamp(coord, vec2(0.0), vec2(1.0));
            return texture2D(u_texture, coord).r;
        }
        
        // Get the state at the current position plus offset
        float getStateOffset(float dx, float dy) {
            vec2 offset = vec2(dx, dy) / u_textureSize;
            return getState(v_texCoord + offset);
        }
        
        // Pseudo-random number generator for initialization
        float rand(vec2 co) {
            return fract(sin(dot(co, vec2(12.9898, 78.233)) + u_random) * 43758.5453);
        }
        
        void main() {
            // Initialize with random pattern if u_random is large (> 100)
            if (u_random > 100.0) {
                float randomValue = rand(v_texCoord);
                gl_FragColor = vec4(randomValue > 0.7 ? 1.0 : 0.0, 0.0, 0.0, 1.0);
                return;
            }
            
            // Check if we're near the mouse position
            float mouseRadius = 0.05; // Radius around mouse in texture coordinates
            
            // Account for aspect ratio to make a perfect circle
            vec2 aspectCorrectedPos = v_texCoord;
            vec2 aspectCorrectedMouse = u_mousePos;
            aspectCorrectedPos.x *= u_aspectRatio;
            aspectCorrectedMouse.x *= u_aspectRatio;
            
            float distToMouse = distance(aspectCorrectedPos, aspectCorrectedMouse);
            if (distToMouse < mouseRadius) {
                // Force cells near mouse to be alive
                gl_FragColor = vec4(1.0, 0.0, 0.0, 1.0);
                return;
            }
            
            // Get current cell state
            float currentState = getState(v_texCoord);
            
            // Count living neighbors
            float neighbors = 0.0;
            for (int dy = -1; dy <= 1; dy++) {
                for (int dx = -1; dx <= 1; dx++) {
                    if (dx != 0 || dy != 0) { // Skip the center cell
                        neighbors += getStateOffset(float(dx), float(dy));
                    }
                }
            }
            
            // Conway's Game of Life rules
            float newState = 0.0;
            if (currentState > 0.5) {
                // Cell is alive
                if (neighbors >= 1.5 && neighbors <= 3.5) {
                    newState = 1.0; // Stay alive with 2 or 3 neighbors
                }
            } else {
                // Cell is dead
                if (neighbors >= 2.5 && neighbors <= 3.5) {
                    newState = 1.0; // Become alive with exactly 3 neighbors
                }
            }
            
            gl_FragColor = vec4(newState, 0.0, 0.0, 1.0);
        }
    `;
    
    // Create and compile shaders
    const vertexShader = createShader(gl.VERTEX_SHADER, vertexShaderSource);
    const renderFragmentShader = createShader(gl.FRAGMENT_SHADER, renderFragmentShaderSource);
    const updateFragmentShader = createShader(gl.FRAGMENT_SHADER, updateFragmentShaderSource);
    
    // Create shader programs
    renderProgram = createProgram(vertexShader, renderFragmentShader);
    updateProgram = createProgram(vertexShader, updateFragmentShader);
    
    // Create vertex buffer
    const positionBuffer = gl.createBuffer();
    gl.bindBuffer(gl.ARRAY_BUFFER, positionBuffer);
    gl.bufferData(gl.ARRAY_BUFFER, quadPositions, gl.STATIC_DRAW);
    
    console.log('Shaders initialized successfully');
}

function createShader(type, source) {
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

function createProgram(vertexShader, fragmentShader) {
    const program = gl.createProgram();
    gl.attachShader(program, vertexShader);
    gl.attachShader(program, fragmentShader);
    gl.linkProgram(program);
    
    if (!gl.getProgramParameter(program, gl.LINK_STATUS)) {
        console.error('Program linking error:', gl.getProgramInfoLog(program));
        gl.deleteProgram(program);
        return null;
    }
    
    return program;
}

function initTextures() {
    // Clean up existing textures and framebuffers
    textures.forEach(texture => gl.deleteTexture(texture));
    framebuffers.forEach(framebuffer => gl.deleteFramebuffer(framebuffer));
    
    textures = [];
    framebuffers = [];
    
    // Create two textures for ping-pong rendering
    for (let i = 0; i < 2; i++) {
        const texture = gl.createTexture();
        gl.bindTexture(gl.TEXTURE_2D, texture);
        
        // Set texture parameters
        gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE);
        gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE);
        gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.NEAREST);
        gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.NEAREST);
        
        // Create texture with grid dimensions
        gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA, gridWidth, gridHeight, 0, gl.RGBA, gl.UNSIGNED_BYTE, null);
        
        textures.push(texture);
        
        // Create framebuffer for off-screen rendering
        const framebuffer = gl.createFramebuffer();
        gl.bindFramebuffer(gl.FRAMEBUFFER, framebuffer);
        gl.framebufferTexture2D(gl.FRAMEBUFFER, gl.COLOR_ATTACHMENT0, gl.TEXTURE_2D, texture, 0);
        
        framebuffers.push(framebuffer);
    }
    
    console.log('Textures initialized:', gridWidth, 'x', gridHeight);
    
    // Initialize with random pattern
    initializeRandomTexture();
}

function initializeRandomTexture() {
    gl.useProgram(updateProgram);
    
    // Bind the first texture as the target
    gl.bindFramebuffer(gl.FRAMEBUFFER, framebuffers[0]);
    gl.viewport(0, 0, gridWidth, gridHeight);
    
    // Set up vertex attributes
    const positionAttribute = gl.getAttribLocation(updateProgram, 'a_position');
    gl.enableVertexAttribArray(positionAttribute);
    gl.bindBuffer(gl.ARRAY_BUFFER, gl.createBuffer());
    gl.bufferData(gl.ARRAY_BUFFER, quadPositions, gl.STATIC_DRAW);
    gl.vertexAttribPointer(positionAttribute, 2, gl.FLOAT, false, 0, 0);
    
    // Set uniforms for random initialization
    gl.uniform2f(gl.getUniformLocation(updateProgram, 'u_textureSize'), gridWidth, gridHeight);
    gl.uniform1f(gl.getUniformLocation(updateProgram, 'u_random'), Math.random() * 1000 + 1000); // > 100 to trigger initialization
    
    // Draw to initialize the texture
    gl.drawArrays(gl.TRIANGLE_STRIP, 0, 4);
    
    currentTexture = 0;
    console.log('Random texture initialized');
}

function updateGrid() {
    if (isPaused) return;
    
    // Use the update program
    gl.useProgram(updateProgram);
    
    // Bind the target framebuffer (opposite of current)
    const targetTexture = (currentTexture + 1) % 2;
    gl.bindFramebuffer(gl.FRAMEBUFFER, framebuffers[targetTexture]);
    gl.viewport(0, 0, gridWidth, gridHeight);
    
    // Bind the current texture as input
    gl.activeTexture(gl.TEXTURE0);
    gl.bindTexture(gl.TEXTURE_2D, textures[currentTexture]);
    
    // Set up vertex attributes
    const positionAttribute = gl.getAttribLocation(updateProgram, 'a_position');
    gl.enableVertexAttribArray(positionAttribute);
    gl.bindBuffer(gl.ARRAY_BUFFER, gl.createBuffer());
    gl.bufferData(gl.ARRAY_BUFFER, quadPositions, gl.STATIC_DRAW);
    gl.vertexAttribPointer(positionAttribute, 2, gl.FLOAT, false, 0, 0);
    
    // Set uniforms
    gl.uniform1i(gl.getUniformLocation(updateProgram, 'u_texture'), 0);
    gl.uniform2f(gl.getUniformLocation(updateProgram, 'u_textureSize'), gridWidth, gridHeight);
    gl.uniform1f(gl.getUniformLocation(updateProgram, 'u_random'), Math.random()); // Small value for normal operation
    gl.uniform2f(gl.getUniformLocation(updateProgram, 'u_mousePos'), mouseX, mouseY);
    gl.uniform1f(gl.getUniformLocation(updateProgram, 'u_aspectRatio'), gl.canvas.width / gl.canvas.height);
    
    // Update the grid
    gl.drawArrays(gl.TRIANGLE_STRIP, 0, 4);
    
    // Swap textures
    currentTexture = targetTexture;
}

function renderGrid() {
    // Use the render program
    gl.useProgram(renderProgram);
    
    // Render to the main canvas
    gl.bindFramebuffer(gl.FRAMEBUFFER, null);
    gl.viewport(0, 0, gl.canvas.width, gl.canvas.height);
    
    // Bind the current texture
    gl.activeTexture(gl.TEXTURE0);
    gl.bindTexture(gl.TEXTURE_2D, textures[currentTexture]);
    
    // Set up vertex attributes
    const positionAttribute = gl.getAttribLocation(renderProgram, 'a_position');
    gl.enableVertexAttribArray(positionAttribute);
    gl.bindBuffer(gl.ARRAY_BUFFER, gl.createBuffer());
    gl.bufferData(gl.ARRAY_BUFFER, quadPositions, gl.STATIC_DRAW);
    gl.vertexAttribPointer(positionAttribute, 2, gl.FLOAT, false, 0, 0);
    
    // Set uniforms
    gl.uniform1i(gl.getUniformLocation(renderProgram, 'u_texture'), 0);
    
    // Render the grid
    gl.drawArrays(gl.TRIANGLE_STRIP, 0, 4);
}

function animate() {
    updateGrid();
    renderGrid();
    
    // Continue animation loop
    animationId = requestAnimationFrame(animate);
} 