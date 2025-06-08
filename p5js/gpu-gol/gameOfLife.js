class GameOfLife {
    constructor(canvas) {
        this.canvas = canvas;
        
        // Create WebGL context with anti-aliasing disabled
        const contextAttributes = {
            alpha: false,
            depth: false,
            stencil: false,
            antialias: false,
            preserveDrawingBuffer: true
        };
        
        this.gl = canvas.getContext('webgl2', contextAttributes) || 
                 canvas.getContext('webgl', contextAttributes);
        
        if (!this.gl) {
            throw new Error('WebGL not supported');
        }
        
        // Initialize properties
        this.width = 0;
        this.height = 0;
        this.isRunning = false;
        this.frameRate = 30;
        this.frameInterval = 1000 / this.frameRate;
        this.lastFrameTime = 0;
        this.originalImageData = null;
        this.initialized = false;
        
        // Initialize WebGL
        this.initWebGL().then(() => {
            console.log('WebGL initialized successfully');
            this.initialized = true;
        }).catch(error => {
            console.error('WebGL initialization failed:', error);
        });
    }
    
    async initWebGL() {
        const gl = this.gl;
        
        // Create shader programs
        this.gameProgram = await this.createProgram('vertex.glsl', 'fragment.glsl');
        this.renderProgram = await this.createProgram('vertex.glsl', 'render.glsl');
        this.ditherProgram = await this.createProgram('vertex.glsl', 'dither.glsl');
        
        // Create a buffer for the vertices
        this.vertexBuffer = gl.createBuffer();
        gl.bindBuffer(gl.ARRAY_BUFFER, this.vertexBuffer);
        
        // Define a full-screen quad (two triangles)
        const vertices = [
            0, 0,
            1, 0,
            0, 1,
            0, 1,
            1, 0,
            1, 1
        ];
        
        gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(vertices), gl.STATIC_DRAW);
        
        // Create framebuffers and textures for ping-pong rendering
        this.createFramebuffers();
    }
    
    async createProgram(vertexShaderPath, fragmentShaderPath) {
        const gl = this.gl;
        
        // Load shader sources
        const vertexShaderSource = await this.loadShader(`shaders/${vertexShaderPath}`);
        const fragmentShaderSource = await this.loadShader(`shaders/${fragmentShaderPath}`);
        
        // Create and compile shaders
        const vertexShader = this.compileShader(gl.VERTEX_SHADER, vertexShaderSource);
        const fragmentShader = this.compileShader(gl.FRAGMENT_SHADER, fragmentShaderSource);
        
        // Create program
        const program = gl.createProgram();
        gl.attachShader(program, vertexShader);
        gl.attachShader(program, fragmentShader);
        gl.linkProgram(program);
        
        if (!gl.getProgramParameter(program, gl.LINK_STATUS)) {
            throw new Error(`Could not link program: ${gl.getProgramInfoLog(program)}`);
        }
        
        // Get attribute and uniform locations
        const positionAttribLocation = gl.getAttribLocation(program, 'a_position');
        const stateUniformLocation = gl.getUniformLocation(program, 'u_state');
        const resolutionUniformLocation = gl.getUniformLocation(program, 'u_resolution');
        const imageUniformLocation = gl.getUniformLocation(program, 'u_image');
        
        return {
            program,
            positionAttribLocation,
            stateUniformLocation,
            resolutionUniformLocation,
            imageUniformLocation
        };
    }
    
    async loadShader(path) {
        const response = await fetch(path);
        if (!response.ok) {
            throw new Error(`Failed to load shader: ${path}`);
        }
        return await response.text();
    }
    
    compileShader(type, source) {
        const gl = this.gl;
        const shader = gl.createShader(type);
        
        gl.shaderSource(shader, source);
        gl.compileShader(shader);
        
        if (!gl.getShaderParameter(shader, gl.COMPILE_STATUS)) {
            const info = gl.getShaderInfoLog(shader);
            gl.deleteShader(shader);
            throw new Error(`Could not compile shader: ${info}`);
        }
        
        return shader;
    }
    
    createFramebuffers() {
        const gl = this.gl;
        
        // Create textures and framebuffers for ping-pong rendering
        this.textures = [
            this.createTexture(),
            this.createTexture()
        ];
        
        this.framebuffers = [
            this.createFramebuffer(this.textures[0]),
            this.createFramebuffer(this.textures[1])
        ];
        
        // Create a texture for the original image
        this.imageTexture = this.createTexture();
    }
    
    createTexture() {
        const gl = this.gl;
        const texture = gl.createTexture();
        
        gl.bindTexture(gl.TEXTURE_2D, texture);
        gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.REPEAT);
        gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.REPEAT);
        gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.NEAREST);
        gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.NEAREST);
        
        return texture;
    }
    
    createFramebuffer(texture) {
        const gl = this.gl;
        const framebuffer = gl.createFramebuffer();
        
        gl.bindFramebuffer(gl.FRAMEBUFFER, framebuffer);
        gl.framebufferTexture2D(gl.FRAMEBUFFER, gl.COLOR_ATTACHMENT0, gl.TEXTURE_2D, texture, 0);
        
        return framebuffer;
    }
    
    resizeCanvas(width, height) {
        this.width = width;
        this.height = height;
        
        // Set the actual canvas dimensions to match the image dimensions
        // This ensures pixel-perfect rendering without anti-aliasing
        this.canvas.width = width;
        this.canvas.height = height;
        
        const gl = this.gl;
        
        // Set the WebGL viewport to match the canvas dimensions
        gl.viewport(0, 0, width, height);
        
        // Disable anti-aliasing
        gl.disable(gl.BLEND);
        gl.disable(gl.DITHER);
        
        // Resize textures
        for (const texture of this.textures) {
            gl.bindTexture(gl.TEXTURE_2D, texture);
            gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.REPEAT);
            gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.REPEAT);
            gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.NEAREST);
            gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.NEAREST);
            gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA, width, height, 0, gl.RGBA, gl.UNSIGNED_BYTE, null);
        }
        
        gl.bindTexture(gl.TEXTURE_2D, this.imageTexture);
        gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.REPEAT);
        gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.REPEAT);
        gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.NEAREST);
        gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.NEAREST);
        gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA, width, height, 0, gl.RGBA, gl.UNSIGNED_BYTE, null);
    }
    
    async loadImage(imageData) {
        // Wait for WebGL initialization to complete
        if (!this.initialized) {
            console.log('Waiting for WebGL initialization...');
            await new Promise(resolve => {
                const checkInitialized = () => {
                    if (this.initialized) {
                        resolve();
                    } else {
                        setTimeout(checkInitialized, 100);
                    }
                };
                checkInitialized();
            });
        }
        
        console.log(`Loading image with dimensions: ${imageData.width}x${imageData.height}`);
        this.originalImageData = imageData;
        
        // Resize canvas to match image dimensions
        this.resizeCanvas(imageData.width, imageData.height);
        
        const gl = this.gl;
        
        // Reset WebGL state for the new image dimensions
        gl.viewport(0, 0, imageData.width, imageData.height);
        
        // Disable anti-aliasing
        gl.disable(gl.BLEND);
        gl.disable(gl.DITHER);
        
        // Create a temporary canvas to flip the image vertically
        const tempCanvas = document.createElement('canvas');
        tempCanvas.width = imageData.width;
        tempCanvas.height = imageData.height;
        const tempCtx = tempCanvas.getContext('2d');
        
        // Flip the image vertically
        tempCtx.translate(0, imageData.height);
        tempCtx.scale(1, -1);
        tempCtx.drawImage(imageData, 0, 0, imageData.width, imageData.height);
        
        // Recreate textures with the new dimensions
        this.textures.forEach((texture, index) => {
            gl.bindTexture(gl.TEXTURE_2D, texture);
            gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.REPEAT);
            gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.REPEAT);
            gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.NEAREST);
            gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.NEAREST);
            gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA, imageData.width, imageData.height, 0, gl.RGBA, gl.UNSIGNED_BYTE, null);
        });
        
        // Upload flipped image to texture
        gl.bindTexture(gl.TEXTURE_2D, this.imageTexture);
        gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.REPEAT);
        gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.REPEAT);
        gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.NEAREST);
        gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.NEAREST);
        gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA, gl.RGBA, gl.UNSIGNED_BYTE, tempCanvas);
        
        // Apply dithering to create initial state
        this.applyDithering();
        
        // Trigger a resize event to update the display size
        window.dispatchEvent(new Event('resize'));
    }
    
    applyDithering() {
        try {
            const gl = this.gl;
            
            // Step 1: Apply dithering to the first texture
            gl.bindFramebuffer(gl.FRAMEBUFFER, this.framebuffers[0]);
            gl.viewport(0, 0, this.width, this.height);
            
            // Use the dither program
            gl.useProgram(this.ditherProgram.program);
            
            // Set up attributes
            gl.bindBuffer(gl.ARRAY_BUFFER, this.vertexBuffer);
            gl.enableVertexAttribArray(this.ditherProgram.positionAttribLocation);
            gl.vertexAttribPointer(this.ditherProgram.positionAttribLocation, 2, gl.FLOAT, false, 0, 0);
            
            // Set uniforms
            gl.uniform2f(this.ditherProgram.resolutionUniformLocation, this.width, this.height);
            
            // Bind the image texture
            gl.activeTexture(gl.TEXTURE0);
            gl.bindTexture(gl.TEXTURE_2D, this.imageTexture);
            gl.uniform1i(this.ditherProgram.imageUniformLocation, 0);
            
            // Draw the quad
            gl.drawArrays(gl.TRIANGLES, 0, 6);
            
            // Check for WebGL errors
            let error = gl.getError();
            if (error !== gl.NO_ERROR) {
                console.error(`WebGL error during dithering (step 1): ${error}`);
            }
            
            // Step 2: Copy the result to the second texture
            // First, read the pixels from the first texture
            const pixels = new Uint8Array(this.width * this.height * 4);
            gl.readPixels(0, 0, this.width, this.height, gl.RGBA, gl.UNSIGNED_BYTE, pixels);
            
            // Then, write them to the second texture
            gl.bindFramebuffer(gl.FRAMEBUFFER, this.framebuffers[1]);
            gl.bindTexture(gl.TEXTURE_2D, this.textures[1]);
            gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA, this.width, this.height, 0, gl.RGBA, gl.UNSIGNED_BYTE, pixels);
            
            // Check for WebGL errors
            error = gl.getError();
            if (error !== gl.NO_ERROR) {
                console.error(`WebGL error during dithering (step 2): ${error}`);
            }
            
            // Reset framebuffer
            gl.bindFramebuffer(gl.FRAMEBUFFER, null);
            
            // Initialize frame count
            this.frameCount = 0;
            
            console.log('Dithering applied successfully');
            
            // Render the initial state
            this.render();
        } catch (error) {
            console.error('Error applying dithering:', error);
        }
    }
    
    start() {
        if (!this.initialized) {
            console.error('WebGL not yet initialized');
            return;
        }
        
        if (!this.originalImageData) {
            console.error('No image loaded');
            return;
        }
        
        this.isRunning = true;
        this.lastFrameTime = performance.now();
        this.animate();
    }
    
    pause() {
        this.isRunning = false;
    }
    
    reset() {
        if (!this.originalImageData) {
            return;
        }
        
        // Reapply dithering to reset the state
        this.applyDithering();
        
        // Render the current state
        this.render();
    }
    
    setFrameRate(fps) {
        this.frameRate = fps;
        this.frameInterval = 1000 / fps;
    }
    
    animate(timestamp) {
        if (!this.isRunning) {
            return;
        }
        
        // Calculate time since last frame
        const elapsed = timestamp - this.lastFrameTime;
        
        // If enough time has passed, update and render
        if (elapsed >= this.frameInterval) {
            this.lastFrameTime = timestamp;
            this.update();
            this.render();
        }
        
        // Request next frame
        requestAnimationFrame(this.animate.bind(this));
    }
    
    update() {
        try {
            const gl = this.gl;
            
            // Ping-pong between framebuffers
            const srcIndex = this.frameCount % 2;
            const destIndex = (this.frameCount + 1) % 2;
            
            console.log(`Update: Frame ${this.frameCount}, Source: ${srcIndex}, Dest: ${destIndex}`);
            
            // Bind the destination framebuffer
            gl.bindFramebuffer(gl.FRAMEBUFFER, this.framebuffers[destIndex]);
            
            // Use the game program
            gl.useProgram(this.gameProgram.program);
            
            // Set up attributes
            gl.bindBuffer(gl.ARRAY_BUFFER, this.vertexBuffer);
            gl.enableVertexAttribArray(this.gameProgram.positionAttribLocation);
            gl.vertexAttribPointer(this.gameProgram.positionAttribLocation, 2, gl.FLOAT, false, 0, 0);
            
            // Set uniforms
            gl.uniform2f(this.gameProgram.resolutionUniformLocation, this.width, this.height);
            
            // Bind the source texture
            gl.activeTexture(gl.TEXTURE0);
            gl.bindTexture(gl.TEXTURE_2D, this.textures[srcIndex]);
            gl.uniform1i(this.gameProgram.stateUniformLocation, 0);
            
            // Draw the quad
            gl.drawArrays(gl.TRIANGLES, 0, 6);
            
            // Check for WebGL errors
            const error = gl.getError();
            if (error !== gl.NO_ERROR) {
                console.error(`WebGL error during update: ${error}`);
            }
            
            // Increment frame count
            this.frameCount++;
            
            // Reset framebuffer
            gl.bindFramebuffer(gl.FRAMEBUFFER, null);
        } catch (error) {
            console.error('Error updating game state:', error);
            this.isRunning = false;
        }
    }
    
    render() {
        try {
            const gl = this.gl;
            
            // Clear the canvas
            gl.clearColor(0, 0, 0, 1);
            gl.clear(gl.COLOR_BUFFER_BIT);
            
            // Use the render program
            gl.useProgram(this.renderProgram.program);
            
            // Set up attributes
            gl.bindBuffer(gl.ARRAY_BUFFER, this.vertexBuffer);
            gl.enableVertexAttribArray(this.renderProgram.positionAttribLocation);
            gl.vertexAttribPointer(this.renderProgram.positionAttribLocation, 2, gl.FLOAT, false, 0, 0);
            
            // Bind the current state texture
            const currentIndex = this.frameCount % 2;
            console.log(`Render: Frame ${this.frameCount}, Current: ${currentIndex}`);
            
            gl.activeTexture(gl.TEXTURE0);
            gl.bindTexture(gl.TEXTURE_2D, this.textures[currentIndex]);
            gl.uniform1i(this.renderProgram.stateUniformLocation, 0);
            
            // Draw the quad
            gl.drawArrays(gl.TRIANGLES, 0, 6);
            
            // Check for WebGL errors
            const error = gl.getError();
            if (error !== gl.NO_ERROR) {
                console.error(`WebGL error during render: ${error}`);
            }
        } catch (error) {
            console.error('Error rendering:', error);
        }
    }
}
