// GPU Majority Cellular Automaton
// Based on https://quasarbright.github.io/p5js/majority-automaton/

// WebGL context
let gl;
// Shader program for rendering and updating the automaton
let renderProgram;
let updateProgram;
// Textures for ping-pong rendering
let textures = [];
let framebuffers = [];
// Current texture index (0 or 1)
let currentTexture = 0;
// Number of different states/colors (can be changed by the user)
let numGroups = 4;
// Grid dimensions
let gridWidth, gridHeight;
// Animation frame ID for cancellation
let animationId;
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
    // Each cell corresponds to one pixel on the screen
    const scale = 1; // 1:1 pixel mapping
    gridWidth = Math.floor(canvas.width / scale);
    gridHeight = Math.floor(canvas.height / scale);
    
    // Initialize shaders and textures
    initShaders();
    initTextures();
    
    // Start the animation loop
    animate();
    
    // Handle window resize
    window.addEventListener('resize', onResize);
    
    // Add event listener for reset button
    document.getElementById('resetButton').addEventListener('click', function() {
        // Reset with random pattern
        initializeRandomTexture();
    });
    
    // Add event listener for number of colors input
    document.getElementById('numColorsInput').addEventListener('input', function() {
        // Get the value from the input
        const value = parseInt(this.value);
        
        // Ensure the value is at least 2
        if (value >= 2) {
            numGroups = value + 1;
            // Reset with the new number of groups
            initializeRandomTexture();
        } else {
            // If the value is less than 2, reset it to 2
            this.value = 2;
            numGroups = 2;
            initializeRandomTexture();
        }
    });
};

// Handle window resize
function onResize() {
    const canvas = document.getElementById('glCanvas');
    canvas.width = window.innerWidth;
    canvas.height = window.innerHeight;
    
    // Update grid dimensions to match the canvas pixel-for-pixel
    const scale = 1; // 1:1 pixel mapping
    gridWidth = Math.floor(canvas.width / scale);
    gridHeight = Math.floor(canvas.height / scale);
    
    // Reinitialize textures with new dimensions
    initTextures();
    
    // Update viewport
    gl.viewport(0, 0, canvas.width, canvas.height);
}

// Initialize shaders
function initShaders() {
    // Vertex shader source - same for both programs
    const vertexShaderSource = `
        attribute vec2 a_position;
        varying vec2 v_texCoord;
        
        void main() {
            gl_Position = vec4(a_position, 0.0, 1.0);
            // Convert from clip space (-1 to 1) to texture space (0 to 1)
            v_texCoord = a_position * 0.5 + 0.5;
        }
    `;
    
    // Fragment shader for rendering the grid
    const renderFragmentShaderSource = `
        precision mediump float;
        varying vec2 v_texCoord;
        uniform sampler2D u_texture;
        uniform float u_numGroups;
        
        // HSV to RGB conversion
        vec3 hsv2rgb(vec3 c) {
            vec4 K = vec4(1.0, 2.0 / 3.0, 1.0 / 3.0, 3.0);
            vec3 p = abs(fract(c.xxx + K.xyz) * 6.0 - K.www);
            return c.z * mix(K.xxx, clamp(p - K.xxx, 0.0, 1.0), c.y);
        }
        
        void main() {
            // Get the state value (0 to 1)
            float state = texture2D(u_texture, v_texCoord).r;
            
            // Convert state to a hue value (0 to 1)
            float hue = state;
            
            // Generate a color using HSV
            // Use full saturation and value for vibrant colors
            vec3 hsv = vec3(hue, 1.0, 1.0);
            vec3 rgb = hsv2rgb(hsv);
            
            gl_FragColor = vec4(rgb, 1.0);
        }
    `;
    
    // Fragment shader for updating the grid based on the majority rule
    const updateFragmentShaderSource = `
        precision mediump float;
        varying vec2 v_texCoord;
        uniform sampler2D u_texture;
        uniform vec2 u_textureSize;
        uniform float u_numGroups;
        uniform float u_random;
        
        // Get the state of a cell at the given texture coordinates
        float getState(vec2 coord) {
            // Clamp coordinates to prevent out-of-bounds access
            coord = clamp(coord, vec2(0.0), vec2(1.0));
            return texture2D(u_texture, coord).r;
        }
        
        // Get the state at the current position plus offset
        float getStateOffset(float dx, float dy) {
            vec2 offset = vec2(dx, dy) / u_textureSize;
            return getState(v_texCoord + offset);
        }
        
        // Pseudo-random number generator
        // Based on https://stackoverflow.com/a/17479300
        float rand(vec2 co) {
            return fract(sin(dot(co, vec2(12.9898, 78.233)) + u_random) * 43758.5453);
        }
        
        void main() {
            // Also consider the current cell's state
            float currentState = getState(v_texCoord);
            
            // Create an array to count occurrences of each state
            // We'll use a fixed-size array and only use the first u_numGroups elements
            float counts[16]; // Support up to 16 states
            for (int i = 0; i < 16; i++) {
                counts[i] = 0.0;
            }
            
            // Check all 8 neighbors
            for (int dy = -1; dy <= 1; dy++) {
                for (int dx = -1; dx <= 1; dx++) {
                    if (dx != 0 || dy != 0) { // Skip the center cell
                        float stateValue = getStateOffset(float(dx), float(dy));
                        
                        // Convert normalized state value to state index
                        // stateValue is in [0,1], we need to map it to [0,numGroups-1]
                        float stateIndex = floor(stateValue * u_numGroups);
                        
                        // Increment the count for this state
                        // We need to handle each possible state individually since WebGL 1.0
                        // doesn't support dynamic indexing of arrays
                        for (int i = 0; i < 16; i++) {
                            if (float(i) == stateIndex && float(i) < u_numGroups) {
                                counts[i] += 1.0;
                            }
                        }
                    }
                }
            }
            
            // Find the maximum count
            float maxCount = 0.0;
            for (int i = 0; i < 16; i++) {
                if (float(i) < u_numGroups) {
                    maxCount = max(maxCount, counts[i]);
                }
            }
            
            // Find all states with the maximum count
            bool isMax[16]; // Track which states have the maximum count
            float numMaxStates = 0.0;
            
            for (int i = 0; i < 16; i++) {
                isMax[i] = false;
                if (float(i) < u_numGroups) {
                    if (abs(counts[i] - maxCount) < 0.01) {
                        isMax[i] = true;
                        numMaxStates += 1.0;
                    }
                }
            }
            
            // Choose a random state from the ones with maximum count
            float randomValue = rand(v_texCoord + vec2(u_random));
            float randomThreshold = randomValue * numMaxStates;
            
            // Select the state based on the random value
            float selectedState = 0.0;
            float cumulative = 0.0;
            
            for (int i = 0; i < 16; i++) {
                if (float(i) < u_numGroups && isMax[i]) {
                    cumulative += 1.0;
                    if (randomThreshold < cumulative) {
                        selectedState = float(i);
                        break;
                    }
                }
            }
            
            // Normalize the selected state to [0,1]
            float normalizedState = selectedState / (u_numGroups - 1.0);
            
            // Output the selected state
            gl_FragColor = vec4(normalizedState, 0.0, 0.0, 1.0);
        }
    `;
    
    // Create shader programs
    renderProgram = createProgram(vertexShaderSource, renderFragmentShaderSource);
    updateProgram = createProgram(vertexShaderSource, updateFragmentShaderSource);
}

// Create a shader program from vertex and fragment shader sources
function createProgram(vertexSource, fragmentSource) {
    // Create and compile vertex shader
    const vertexShader = gl.createShader(gl.VERTEX_SHADER);
    gl.shaderSource(vertexShader, vertexSource);
    gl.compileShader(vertexShader);
    
    // Check for compilation errors
    if (!gl.getShaderParameter(vertexShader, gl.COMPILE_STATUS)) {
        console.error('Vertex shader compilation error:', gl.getShaderInfoLog(vertexShader));
        gl.deleteShader(vertexShader);
        return null;
    }
    
    // Create and compile fragment shader
    const fragmentShader = gl.createShader(gl.FRAGMENT_SHADER);
    gl.shaderSource(fragmentShader, fragmentSource);
    gl.compileShader(fragmentShader);
    
    // Check for compilation errors
    if (!gl.getShaderParameter(fragmentShader, gl.COMPILE_STATUS)) {
        console.error('Fragment shader compilation error:', gl.getShaderInfoLog(fragmentShader));
        gl.deleteShader(vertexShader);
        gl.deleteShader(fragmentShader);
        return null;
    }
    
    // Create shader program and link shaders
    const program = gl.createProgram();
    gl.attachShader(program, vertexShader);
    gl.attachShader(program, fragmentShader);
    gl.linkProgram(program);
    
    // Check for linking errors
    if (!gl.getProgramParameter(program, gl.LINK_STATUS)) {
        console.error('Shader program linking error:', gl.getProgramInfoLog(program));
        gl.deleteProgram(program);
        gl.deleteShader(vertexShader);
        gl.deleteShader(fragmentShader);
        return null;
    }
    
    return program;
}

// Initialize textures for ping-pong rendering
function initTextures() {
    // Clean up existing textures and framebuffers
    for (let i = 0; i < textures.length; i++) {
        gl.deleteTexture(textures[i]);
        gl.deleteFramebuffer(framebuffers[i]);
    }
    
    textures = [];
    framebuffers = [];
    
    // Create two textures for ping-pong rendering
    for (let i = 0; i < 2; i++) {
        // Create texture
        const texture = gl.createTexture();
        gl.bindTexture(gl.TEXTURE_2D, texture);
        
        // Set texture parameters
        gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE);
        gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE);
        gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.NEAREST);
        gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.NEAREST);
        
        // Allocate texture memory
        gl.texImage2D(
            gl.TEXTURE_2D,
            0,
            gl.RGBA,
            gridWidth,
            gridHeight,
            0,
            gl.RGBA,
            gl.UNSIGNED_BYTE,
            null
        );
        
        textures.push(texture);
        
        // Create framebuffer
        const framebuffer = gl.createFramebuffer();
        gl.bindFramebuffer(gl.FRAMEBUFFER, framebuffer);
        gl.framebufferTexture2D(
            gl.FRAMEBUFFER,
            gl.COLOR_ATTACHMENT0,
            gl.TEXTURE_2D,
            texture,
            0
        );
        
        framebuffers.push(framebuffer);
    }
    
    // Initialize the first texture with random data
    initializeRandomTexture();
}

// Initialize the texture with random data
function initializeRandomTexture() {
    // Create random data for the grid
    const data = new Uint8Array(gridWidth * gridHeight * 4);
    
    // Create a random pattern
    const counts = [0, 0, 0, 0];
    
    for (let y = 0; y < gridHeight; y++) {
        for (let x = 0; x < gridWidth; x++) {
            const i = y * gridWidth + x;
            
            // Truly random pattern - position-agnostic
            const state = Math.floor(Math.random() * numGroups);
            
            counts[state]++;
            
            // Map state to a normalized value between 0 and 1
            // For example, if numGroups = 5, the states would be mapped to 0, 0.25, 0.5, 0.75, 1.0
            const normalizedValue = state / (numGroups - 1);
            const value = Math.floor(normalizedValue * 255);
            
            data[i * 4] = value;
            data[i * 4 + 1] = 0;
            data[i * 4 + 2] = 0;
            data[i * 4 + 3] = 255;
        }
    }
    
    // Upload data to the first texture
    gl.bindTexture(gl.TEXTURE_2D, textures[0]);
    gl.texSubImage2D(
        gl.TEXTURE_2D,
        0,
        0,
        0,
        gridWidth,
        gridHeight,
        gl.RGBA,
        gl.UNSIGNED_BYTE,
        data
    );
    
    // Reset current texture index
    currentTexture = 0;
    
    // Debug: Log the distribution of states
    console.log(`Initialized texture with random pattern distribution:`, counts);
}

// Create and bind a buffer for the quad positions
function setupQuad(program) {
    const positionBuffer = gl.createBuffer();
    gl.bindBuffer(gl.ARRAY_BUFFER, positionBuffer);
    gl.bufferData(gl.ARRAY_BUFFER, quadPositions, gl.STATIC_DRAW);
    
    const positionLocation = gl.getAttribLocation(program, 'a_position');
    gl.enableVertexAttribArray(positionLocation);
    gl.vertexAttribPointer(positionLocation, 2, gl.FLOAT, false, 0, 0);
}

// Update the automaton state using the update shader
function updateAutomaton() {
    // Use the update program
    gl.useProgram(updateProgram);
    
    // Set up the quad
    setupQuad(updateProgram);
    
    // Set uniforms
    const textureLocation = gl.getUniformLocation(updateProgram, 'u_texture');
    const textureSizeLocation = gl.getUniformLocation(updateProgram, 'u_textureSize');
    const numGroupsLocation = gl.getUniformLocation(updateProgram, 'u_numGroups');
    const randomLocation = gl.getUniformLocation(updateProgram, 'u_random');
    
    gl.uniform1i(textureLocation, 0);
    gl.uniform2f(textureSizeLocation, gridWidth, gridHeight);
    gl.uniform1f(numGroupsLocation, numGroups);
    gl.uniform1f(randomLocation, Math.random());
    
    // Bind the current texture
    gl.activeTexture(gl.TEXTURE0);
    gl.bindTexture(gl.TEXTURE_2D, textures[currentTexture]);
    
    // Render to the other texture
    const nextTexture = 1 - currentTexture;
    gl.bindFramebuffer(gl.FRAMEBUFFER, framebuffers[nextTexture]);
    gl.viewport(0, 0, gridWidth, gridHeight);
    
    // Draw the quad
    gl.drawArrays(gl.TRIANGLE_STRIP, 0, 4);
    
    // Switch to the next texture
    currentTexture = nextTexture;
}

// Render the current state to the canvas
function renderAutomaton() {
    // Use the render program
    gl.useProgram(renderProgram);
    
    // Set up the quad
    setupQuad(renderProgram);
    
    // Set uniforms
    const textureLocation = gl.getUniformLocation(renderProgram, 'u_texture');
    const numGroupsLocation = gl.getUniformLocation(renderProgram, 'u_numGroups');
    
    gl.uniform1i(textureLocation, 0);
    gl.uniform1f(numGroupsLocation, numGroups);
    
    // Bind the current texture
    gl.activeTexture(gl.TEXTURE0);
    gl.bindTexture(gl.TEXTURE_2D, textures[currentTexture]);
    
    // Render to the canvas
    gl.bindFramebuffer(gl.FRAMEBUFFER, null);
    gl.viewport(0, 0, gl.canvas.width, gl.canvas.height);
    
    // Draw the quad
    gl.drawArrays(gl.TRIANGLE_STRIP, 0, 4);
}

// Animation loop
function animate() {
    // Update the automaton state
    updateAutomaton();
    
    // Render the current state
    renderAutomaton();
    
    // Request the next frame
    animationId = requestAnimationFrame(animate);
}
