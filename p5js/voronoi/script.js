class VoronoiVisualizer {
    constructor() {
        this.canvas = document.getElementById('canvas');
        this.gl = this.canvas.getContext('webgl');
        
        if (!this.gl) {
            alert('WebGL not supported');
            return;
        }
        
        this.program = null;
        this.points = [];
        this.velocities = [];
        this.pointCount = 10;
        this.distanceMetric = 0; // Default to Euclidean
        this.isPaused = false;
        this.showColors = true;
        this.showGradient = false;
        this.showEdges = false;
        this.showDots = true;
        this.generateRandomPoints();
        
        this.init();
    }
    
    generateRandomPoints() {
        this.points = [];
        this.velocities = [];
        for (let i = 0; i < this.pointCount; i++) {
            this.points.push(Math.random(), Math.random());
            // Random velocity between -0.002 and 0.002 for smooth movement
            this.velocities.push(
                (Math.random() - 0.5) * 0.004,
                (Math.random() - 0.5) * 0.004
            );
        }
    }
    
    addPoint() {
        if (this.pointCount < 50) {
            this.pointCount++;
            this.points.push(Math.random(), Math.random());
            this.velocities.push(
                (Math.random() - 0.5) * 0.004,
                (Math.random() - 0.5) * 0.004
            );
            this.updatePointCountDisplay();
        }
    }
    
    removePoint() {
        if (this.pointCount > 1) {
            this.pointCount--;
            this.points.pop();
            this.points.pop();
            this.velocities.pop();
            this.velocities.pop();
            this.updatePointCountDisplay();
        }
    }
    
    updatePointCountDisplay() {
        document.getElementById('pointCountDisplay').textContent = this.pointCount;
    }
    
    async loadShader(url) {
        const response = await fetch(url);
        return await response.text();
    }
    
    createShader(source, type) {
        const shader = this.gl.createShader(type);
        this.gl.shaderSource(shader, source);
        this.gl.compileShader(shader);
        
        if (!this.gl.getShaderParameter(shader, this.gl.COMPILE_STATUS)) {
            console.error('Shader compilation error:', this.gl.getShaderInfoLog(shader));
            this.gl.deleteShader(shader);
            return null;
        }
        
        return shader;
    }
    
    createProgram(vertexShader, fragmentShader) {
        const program = this.gl.createProgram();
        this.gl.attachShader(program, vertexShader);
        this.gl.attachShader(program, fragmentShader);
        this.gl.linkProgram(program);
        
        if (!this.gl.getProgramParameter(program, this.gl.LINK_STATUS)) {
            console.error('Program linking error:', this.gl.getProgramInfoLog(program));
            this.gl.deleteProgram(program);
            return null;
        }
        
        return program;
    }
    
    async init() {
        // Load shaders
        const vertexShaderSource = await this.loadShader('vertex.glsl');
        const fragmentShaderSource = await this.loadShader('fragment.glsl');
        
        // Create shaders
        const vertexShader = this.createShader(vertexShaderSource, this.gl.VERTEX_SHADER);
        const fragmentShader = this.createShader(fragmentShaderSource, this.gl.FRAGMENT_SHADER);
        
        // Create program
        this.program = this.createProgram(vertexShader, fragmentShader);
        this.gl.useProgram(this.program);
        
        // Create full-screen quad
        const positions = new Float32Array([
            -1, -1,
             1, -1,
            -1,  1,
             1,  1
        ]);
        
        const positionBuffer = this.gl.createBuffer();
        this.gl.bindBuffer(this.gl.ARRAY_BUFFER, positionBuffer);
        this.gl.bufferData(this.gl.ARRAY_BUFFER, positions, this.gl.STATIC_DRAW);
        
        // Set up attributes
        const positionLocation = this.gl.getAttribLocation(this.program, 'a_position');
        this.gl.enableVertexAttribArray(positionLocation);
        this.gl.vertexAttribPointer(positionLocation, 2, this.gl.FLOAT, false, 0, 0);
        
        // Get uniform locations
        this.resolutionLocation = this.gl.getUniformLocation(this.program, 'u_resolution');
        this.pointsLocation = this.gl.getUniformLocation(this.program, 'u_points');
        this.velocitiesLocation = this.gl.getUniformLocation(this.program, 'u_velocities');
        this.pointCountLocation = this.gl.getUniformLocation(this.program, 'u_pointCount');
        this.distanceMetricLocation = this.gl.getUniformLocation(this.program, 'u_distanceMetric');
        this.showColorsLocation = this.gl.getUniformLocation(this.program, 'u_showColors');
        this.showGradientLocation = this.gl.getUniformLocation(this.program, 'u_showGradient');
        this.showEdgesLocation = this.gl.getUniformLocation(this.program, 'u_showEdges');
        this.showDotsLocation = this.gl.getUniformLocation(this.program, 'u_showDots');
        
        // Set up resize handler
        window.addEventListener('resize', () => this.resize());
        this.resize();
        
        // Start render loop
        this.render();
        
        // Set up control panel event listeners
        document.getElementById('increasePoints').addEventListener('click', () => {
            this.addPoint();
        });
        
        document.getElementById('decreasePoints').addEventListener('click', () => {
            this.removePoint();
        });
        
        // Set up distance metric dropdown
        document.getElementById('distanceMetric').addEventListener('change', (e) => {
            this.distanceMetric = parseInt(e.target.value);
        });
        
        // Set up pause/play button
        document.getElementById('pausePlay').addEventListener('click', () => {
            this.isPaused = !this.isPaused;
            document.getElementById('pausePlay').textContent = this.isPaused ? 'Play' : 'Pause';
        });
        
        // Set up visual toggle checkboxes
        document.getElementById('showColors').addEventListener('change', (e) => {
            this.showColors = e.target.checked;
        });
        
        document.getElementById('showGradient').addEventListener('change', (e) => {
            this.showGradient = e.target.checked;
        });
        
        document.getElementById('showEdges').addEventListener('change', (e) => {
            this.showEdges = e.target.checked;
        });
        
        document.getElementById('showDots').addEventListener('change', (e) => {
            this.showDots = e.target.checked;
        });
        
        // Set up About modal
        const aboutButton = document.getElementById('aboutButton');
        const aboutModal = document.getElementById('aboutModal');
        const closeModal = document.querySelector('.close');
        
        aboutButton.addEventListener('click', () => {
            aboutModal.style.display = 'block';
        });
        
        closeModal.addEventListener('click', () => {
            aboutModal.style.display = 'none';
        });
        
        window.addEventListener('click', (e) => {
            if (e.target === aboutModal) {
                aboutModal.style.display = 'none';
            }
        });
    }
    
    resize() {
        this.canvas.width = window.innerWidth;
        this.canvas.height = window.innerHeight;
        this.gl.viewport(0, 0, this.canvas.width, this.canvas.height);
    }
    
    updatePoints() {
        for (let i = 0; i < this.pointCount; i++) {
            const xIndex = i * 2;
            const yIndex = i * 2 + 1;
            
            // Update positions
            this.points[xIndex] += this.velocities[xIndex];
            this.points[yIndex] += this.velocities[yIndex];
            
            // Bounce off left and right edges
            if (this.points[xIndex] <= 0 || this.points[xIndex] >= 1) {
                this.velocities[xIndex] *= -1;
                this.points[xIndex] = Math.max(0, Math.min(1, this.points[xIndex]));
            }
            
            // Bounce off top and bottom edges
            if (this.points[yIndex] <= 0 || this.points[yIndex] >= 1) {
                this.velocities[yIndex] *= -1;
                this.points[yIndex] = Math.max(0, Math.min(1, this.points[yIndex]));
            }
        }
    }
    
    render() {
        // Update point positions only if not paused
        if (!this.isPaused) {
            this.updatePoints();
        }
        
        // Clear canvas
        this.gl.clearColor(0, 0, 0, 1);
        this.gl.clear(this.gl.COLOR_BUFFER_BIT);
        
        // Set uniforms
        this.gl.uniform2f(this.resolutionLocation, this.canvas.width, this.canvas.height);
        this.gl.uniform1i(this.pointCountLocation, this.pointCount);
        this.gl.uniform1i(this.distanceMetricLocation, this.distanceMetric);
        this.gl.uniform1i(this.showColorsLocation, this.showColors ? 1 : 0);
        this.gl.uniform1i(this.showGradientLocation, this.showGradient ? 1 : 0);
        this.gl.uniform1i(this.showEdgesLocation, this.showEdges ? 1 : 0);
        this.gl.uniform1i(this.showDotsLocation, this.showDots ? 1 : 0);
        
        // Pad points array to full size for uniform
        const paddedPoints = new Float32Array(100); // 50 points * 2 coordinates
        for (let i = 0; i < this.points.length; i++) {
            paddedPoints[i] = this.points[i];
        }
        this.gl.uniform2fv(this.pointsLocation, paddedPoints);
        
        // Pad velocities array to full size for uniform
        const paddedVelocities = new Float32Array(100); // 50 velocities * 2 coordinates
        for (let i = 0; i < this.velocities.length; i++) {
            paddedVelocities[i] = this.velocities[i];
        }
        this.gl.uniform2fv(this.velocitiesLocation, paddedVelocities);
        
        // Draw
        this.gl.drawArrays(this.gl.TRIANGLE_STRIP, 0, 4);
        
        requestAnimationFrame(() => this.render());
    }
}

// Start the visualizer when page loads
window.addEventListener('load', () => {
    new VoronoiVisualizer();
}); 