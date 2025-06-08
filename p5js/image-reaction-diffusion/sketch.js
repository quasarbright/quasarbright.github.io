// WebGL context and shader programs
let gl;
let reactionProgram;
let displayProgram;
let vertexShader;
let fragmentShader;

// Buffers and textures
let positionBuffer;
let textureCoordBuffer;
let framebuffers = [];
let textures = [];
let currentBuffer = 0;

// Simulation state
let isRunning = false;
let speed = 30;
let lastUpdate = 0;
let ditheredImage = null;

// Reaction-diffusion parameters
const params = {
    feed: 0.055,
    kill: 0.062,
    dA: 1.0,
    dB: 0.5,
    dt: 1.0
};

// Initialize WebGL
async function initGL() {
    const canvas = document.getElementById('glCanvas');
    gl = canvas.getContext('webgl', { preserveDrawingBuffer: true });
    
    if (!gl) {
        alert('WebGL not supported');
        return;
    }
    
    // Set canvas size
    canvas.width = 800;
    canvas.height = 800;
    gl.viewport(0, 0, canvas.width, canvas.height);
    
    // Create vertex shader
    vertexShader = createShader(gl.VERTEX_SHADER, `
        attribute vec4 aVertexPosition;
        attribute vec2 aTextureCoord;
        varying highp vec2 vTextureCoord;
        void main(void) {
            gl_Position = aVertexPosition;
            vTextureCoord = aTextureCoord;
        }
    `);
    
    // Create reaction-diffusion program
    const reactionFrag = await fetch('reaction-diffusion.frag').then(r => r.text());
    fragmentShader = createShader(gl.FRAGMENT_SHADER, reactionFrag);
    reactionProgram = createProgram(vertexShader, fragmentShader);
    
    // Create display program
    const displayFrag = `
        precision mediump float;
        uniform sampler2D u_texture;
        varying vec2 vTextureCoord;
        void main() {
            vec4 color = texture2D(u_texture, vTextureCoord);
            float a = color.r;  // A component
            float b = color.g;  // B component
            // Visualize the B component (the pattern)
            float value = b;
            gl_FragColor = vec4(value, value, value, 1.0);
        }
    `;
    const displayFragShader = createShader(gl.FRAGMENT_SHADER, displayFrag);
    displayProgram = createProgram(vertexShader, displayFragShader);
    
    // Create buffers
    positionBuffer = gl.createBuffer();
    gl.bindBuffer(gl.ARRAY_BUFFER, positionBuffer);
    gl.bufferData(gl.ARRAY_BUFFER, new Float32Array([
        -1.0, -1.0,
         1.0, -1.0,
        -1.0,  1.0,
         1.0,  1.0,
    ]), gl.STATIC_DRAW);
    
    textureCoordBuffer = gl.createBuffer();
    gl.bindBuffer(gl.ARRAY_BUFFER, textureCoordBuffer);
    gl.bufferData(gl.ARRAY_BUFFER, new Float32Array([
        0.0, 0.0,
        1.0, 0.0,
        0.0, 1.0,
        1.0, 1.0,
    ]), gl.STATIC_DRAW);
    
    // Create framebuffers and textures for ping-pong
    for (let i = 0; i < 2; i++) {
        const texture = gl.createTexture();
        gl.bindTexture(gl.TEXTURE_2D, texture);
        gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA, canvas.width, canvas.height, 0, gl.RGBA, gl.UNSIGNED_BYTE, null);
        gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.NEAREST);
        gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.NEAREST);
        gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE);
        gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE);
        
        const framebuffer = gl.createFramebuffer();
        gl.bindFramebuffer(gl.FRAMEBUFFER, framebuffer);
        gl.framebufferTexture2D(gl.FRAMEBUFFER, gl.COLOR_ATTACHMENT0, gl.TEXTURE_2D, texture, 0);
        
        framebuffers.push(framebuffer);
        textures.push(texture);
    }
    
    setupControls();
}

// Helper function to create shader
function createShader(type, source) {
    const shader = gl.createShader(type);
    gl.shaderSource(shader, source);
    gl.compileShader(shader);
    
    if (!gl.getShaderParameter(shader, gl.COMPILE_STATUS)) {
        console.error('Shader compile error:', gl.getShaderInfoLog(shader));
        gl.deleteShader(shader);
        return null;
    }
    
    return shader;
}

// Helper function to create program
function createProgram(vertexShader, fragmentShader) {
    const program = gl.createProgram();
    gl.attachShader(program, vertexShader);
    gl.attachShader(program, fragmentShader);
    gl.linkProgram(program);
    
    if (!gl.getProgramParameter(program, gl.LINK_STATUS)) {
        console.error('Program link error:', gl.getProgramInfoLog(program));
        return null;
    }
    
    return program;
}

// Setup UI controls
function setupControls() {
    const startBtn = document.getElementById('startBtn');
    const pauseBtn = document.getElementById('pauseBtn');
    const resetBtn = document.getElementById('resetBtn');
    const speedSlider = document.getElementById('speedSlider');
    const speedValue = document.getElementById('speedValue');
    const imageUpload = document.getElementById('imageUpload');
    
    startBtn.addEventListener('click', () => {
        isRunning = true;
        requestAnimationFrame(render);
    });
    
    pauseBtn.addEventListener('click', () => {
        isRunning = false;
    });
    
    resetBtn.addEventListener('click', () => {
        if (ditheredImage) {
            initializeFromImage(ditheredImage);
        }
    });
    
    speedSlider.addEventListener('input', () => {
        speed = speedSlider.value;
        speedValue.textContent = speed;
    });
    
    imageUpload.addEventListener('change', (e) => {
        const file = e.target.files[0];
        if (file) {
            const reader = new FileReader();
            reader.onload = (event) => {
                const img = new Image();
                img.onload = () => {
                    ditheredImage = ditherImage(img);
                    initializeFromImage(ditheredImage);
                    // Start paused so user can see initial state
                    isRunning = false;
                };
                img.src = event.target.result;
            };
            reader.readAsDataURL(file);
        }
    });
}

// Convert image to black and white using thresholding
function ditherImage(img) {
    const canvas = document.createElement('canvas');
    canvas.width = gl.canvas.width;
    canvas.height = gl.canvas.height;
    const ctx = canvas.getContext('2d');
    
    // Draw image scaled to canvas size
    ctx.fillStyle = 'white';
    ctx.fillRect(0, 0, canvas.width, canvas.height);
    
    // Calculate scaling to maintain aspect ratio
    const scale = Math.min(canvas.width / img.width, canvas.height / img.height);
    const w = img.width * scale;
    const h = img.height * scale;
    const x = (canvas.width - w) / 2;
    const y = (canvas.height - h) / 2;
    
    ctx.drawImage(img, x, y, w, h);
    
    // Convert to black and white using simple thresholding
    const imageData = ctx.getImageData(0, 0, canvas.width, canvas.height);
    const data = imageData.data;
    
    for (let i = 0; i < data.length; i += 4) {
        // Convert to grayscale
        const gray = (data[i] * 0.299 + data[i + 1] * 0.587 + data[i + 2] * 0.114);
        // Simple threshold at 128
        const value = gray < 128 ? 0 : 255;
        data[i] = value;     // R
        data[i + 1] = value; // G
        data[i + 2] = value; // B
        data[i + 3] = 255;   // A
    }
    
    ctx.putImageData(imageData, 0, 0);
    return canvas;
}

// Initialize simulation from image
function initializeFromImage(image) {
    // Clear both textures
    for (let i = 0; i < 2; i++) {
        gl.bindTexture(gl.TEXTURE_2D, textures[i]);
        gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA, gl.canvas.width, gl.canvas.height, 0, gl.RGBA, gl.UNSIGNED_BYTE, null);
    }
    
    // Initialize first texture with image data
    const canvas = document.createElement('canvas');
    canvas.width = gl.canvas.width;
    canvas.height = gl.canvas.height;
    const ctx = canvas.getContext('2d');
    ctx.drawImage(image, 0, 0);
    
    const imageData = ctx.getImageData(0, 0, canvas.width, canvas.height);
    const data = new Uint8Array(imageData.data.buffer);
    
    // Convert to reaction-diffusion initial conditions
    // For black pixels (0), set A=0, B=1
    // For white pixels (255), set A=1, B=0
    for (let i = 0; i < data.length; i += 4) {
        const isBlack = data[i] < 128;
        data[i] = isBlack ? 0 : 255;     // A channel (red) - white pixels become A=1
        data[i + 1] = isBlack ? 255 : 0; // B channel (green) - black pixels become B=1
        data[i + 2] = 0;     // Blue channel
        data[i + 3] = 255;   // Alpha channel
    }
    
    gl.bindTexture(gl.TEXTURE_2D, textures[currentBuffer]);
    gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA, gl.canvas.width, gl.canvas.height, 0, gl.RGBA, gl.UNSIGNED_BYTE, data);
    
    // Clear the other buffer
    gl.bindTexture(gl.TEXTURE_2D, textures[(currentBuffer + 1) % 2]);
    gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA, gl.canvas.width, gl.canvas.height, 0, gl.RGBA, gl.UNSIGNED_BYTE, data);
}

// Main render loop
function render() {
    if (!isRunning) {
        requestAnimationFrame(render);
        return;
    }
    
    const now = performance.now();
    if (now - lastUpdate < 1000 / speed) {
        requestAnimationFrame(render);
        return;
    }
    lastUpdate = now;
    
    // Update reaction-diffusion
    gl.useProgram(reactionProgram);
    
    // Set uniforms for reaction-diffusion
    gl.uniform2f(gl.getUniformLocation(reactionProgram, 'u_resolution'), gl.canvas.width, gl.canvas.height);
    gl.uniform1f(gl.getUniformLocation(reactionProgram, 'u_dt'), params.dt);
    gl.uniform1f(gl.getUniformLocation(reactionProgram, 'u_feed'), params.feed);
    gl.uniform1f(gl.getUniformLocation(reactionProgram, 'u_kill'), params.kill);
    gl.uniform1f(gl.getUniformLocation(reactionProgram, 'u_dA'), params.dA);
    gl.uniform1f(gl.getUniformLocation(reactionProgram, 'u_dB'), params.dB);
    
    // Set up attributes
    const positionLocation = gl.getAttribLocation(reactionProgram, 'aVertexPosition');
    const textureCoordLocation = gl.getAttribLocation(reactionProgram, 'aTextureCoord');
    
    gl.bindBuffer(gl.ARRAY_BUFFER, positionBuffer);
    gl.vertexAttribPointer(positionLocation, 2, gl.FLOAT, false, 0, 0);
    gl.enableVertexAttribArray(positionLocation);
    
    gl.bindBuffer(gl.ARRAY_BUFFER, textureCoordBuffer);
    gl.vertexAttribPointer(textureCoordLocation, 2, gl.FLOAT, false, 0, 0);
    gl.enableVertexAttribArray(textureCoordLocation);
    
    // Ping-pong between buffers
    const nextBuffer = (currentBuffer + 1) % 2;
    
    // Bind the current texture as input
    gl.activeTexture(gl.TEXTURE0);
    gl.bindTexture(gl.TEXTURE_2D, textures[currentBuffer]);
    gl.uniform1i(gl.getUniformLocation(reactionProgram, 'u_texture'), 0);
    
    // Render to the next buffer
    gl.bindFramebuffer(gl.FRAMEBUFFER, framebuffers[nextBuffer]);
    gl.drawArrays(gl.TRIANGLE_STRIP, 0, 4);
    
    // Update current buffer before display
    currentBuffer = nextBuffer;
    
    // Display result
    gl.bindFramebuffer(gl.FRAMEBUFFER, null);
    gl.useProgram(displayProgram);
    
    // Set up display shader attributes
    const displayPositionLocation = gl.getAttribLocation(displayProgram, 'aVertexPosition');
    const displayTexCoordLocation = gl.getAttribLocation(displayProgram, 'aTextureCoord');
    
    gl.bindBuffer(gl.ARRAY_BUFFER, positionBuffer);
    gl.vertexAttribPointer(displayPositionLocation, 2, gl.FLOAT, false, 0, 0);
    gl.enableVertexAttribArray(displayPositionLocation);
    
    gl.bindBuffer(gl.ARRAY_BUFFER, textureCoordBuffer);
    gl.vertexAttribPointer(displayTexCoordLocation, 2, gl.FLOAT, false, 0, 0);
    gl.enableVertexAttribArray(displayTexCoordLocation);
    
    // Bind the current texture for display
    gl.activeTexture(gl.TEXTURE0);
    gl.bindTexture(gl.TEXTURE_2D, textures[nextBuffer]);
    gl.uniform1i(gl.getUniformLocation(displayProgram, 'u_texture'), 0);
    
    gl.drawArrays(gl.TRIANGLE_STRIP, 0, 4);
    
    requestAnimationFrame(render);
}

// Start the application
initGL().then(() => {
    requestAnimationFrame(render);
}); 