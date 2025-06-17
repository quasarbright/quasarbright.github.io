document.addEventListener('DOMContentLoaded', () => {
    // Get canvas and WebGL context
    const canvas = document.getElementById('canvas');
    const gl = canvas.getContext('webgl');
    
    if (!gl) {
        alert('WebGL not supported in your browser!');
        return;
    }
    
    // Create a 2D canvas for drawing root indicators
    const overlayCanvas = document.createElement('canvas');
    overlayCanvas.style.position = 'absolute';
    overlayCanvas.style.top = '0';
    overlayCanvas.style.left = '0';
    overlayCanvas.style.width = '100vw';
    overlayCanvas.style.height = '100vh';
    overlayCanvas.style.pointerEvents = 'none';
    overlayCanvas.style.zIndex = '1';
    document.body.appendChild(overlayCanvas);
    const ctx = overlayCanvas.getContext('2d');
    
    // Polynomial roots in the complex plane
    const roots = [
        { real: 1, imag: 0 },  // 1
        { real: -1, imag: 0 }, // -1
        { real: 0, imag: 1 },  // i
        { real: 0, imag: -1 }  // -i
    ];
    
    // Get UI elements
    const controlsPanel = document.getElementById('controls');
    const toggleControlsBtn = document.getElementById('toggle-controls');
    const hideControlsBtn = document.getElementById('hide-controls');
    const resetViewBtn = document.getElementById('reset-view');
    const maxIterationsSlider = document.getElementById('max-iterations');
    const iterationsValueSpan = document.getElementById('iterations-value');
    const escapeRadiusSlider = document.getElementById('escape-radius');
    const escapeValueSpan = document.getElementById('escape-value');
    const showRootsCheckbox = document.getElementById('show-roots');
    
    // Resize canvas to fill the screen
    function resizeCanvas() {
        canvas.width = window.innerWidth;
        canvas.height = window.innerHeight;
        gl.viewport(0, 0, canvas.width, canvas.height);
        
        overlayCanvas.width = window.innerWidth;
        overlayCanvas.height = window.innerHeight;
    }
    
    window.addEventListener('resize', () => {
        resizeCanvas();
        render();
    });
    resizeCanvas();
    
    // Get shader sources
    const fragmentShaderSource = document.getElementById('fragment-shader').textContent;
    
    // Vertex shader source - just pass through the position
    const vertexShaderSource = `
        attribute vec2 a_position;
        void main() {
            gl_Position = vec4(a_position, 0.0, 1.0);
        }
    `;
    
    // Create shaders
    function createShader(gl, type, source) {
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
    
    const vertexShader = createShader(gl, gl.VERTEX_SHADER, vertexShaderSource);
    const fragmentShader = createShader(gl, gl.FRAGMENT_SHADER, fragmentShaderSource);
    
    // Create program
    const program = gl.createProgram();
    gl.attachShader(program, vertexShader);
    gl.attachShader(program, fragmentShader);
    gl.linkProgram(program);
    
    if (!gl.getProgramParameter(program, gl.LINK_STATUS)) {
        console.error('Program linking error:', gl.getProgramInfoLog(program));
        return;
    }
    
    // Use program
    gl.useProgram(program);
    
    // Create a buffer for the vertices
    const positionBuffer = gl.createBuffer();
    gl.bindBuffer(gl.ARRAY_BUFFER, positionBuffer);
    
    // Two triangles that cover the entire clip space
    const positions = [
        -1.0, -1.0,
         1.0, -1.0,
        -1.0,  1.0,
        -1.0,  1.0,
         1.0, -1.0,
         1.0,  1.0
    ];
    gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(positions), gl.STATIC_DRAW);
    
    // Set up attribute
    const positionLocation = gl.getAttribLocation(program, 'a_position');
    gl.enableVertexAttribArray(positionLocation);
    gl.vertexAttribPointer(positionLocation, 2, gl.FLOAT, false, 0, 0);
    
    // Set up uniforms
    const resolutionLocation = gl.getUniformLocation(program, 'u_resolution');
    const zoomLocation = gl.getUniformLocation(program, 'u_zoom');
    const offsetLocation = gl.getUniformLocation(program, 'u_offset');
    const maxIterationsLocation = gl.getUniformLocation(program, 'u_max_iterations');
    const convergenceThresholdLocation = gl.getUniformLocation(program, 'u_convergence_threshold');
    
    // Initial values
    let zoom = 1.0;
    let offset = { x: 0.0, y: 0.0 };
    let maxIterations = 100;
    let convergenceThreshold = 5.0;
    let showRoots = true;
    
    // Convert complex plane coordinates to canvas pixel coordinates
    function complexToPixel(z) {
        const aspect = canvas.width / canvas.height;
        
        // In the shader, the mapping from normalized device coordinates to complex plane is:
        // z.x = (uv.x - 0.5) * 4.0 * aspect / zoom + offset.x
        // z.y = (uv.y - 0.5) * 4.0 / zoom + offset.y
        
        // To reverse this mapping:
        // uv.x = (z.x - offset.x) * zoom / (4.0 * aspect) + 0.5
        // uv.y = (z.y - offset.y) * zoom / 4.0 + 0.5
        
        // And then convert to pixel coordinates:
        // pixel.x = uv.x * canvas.width
        // pixel.y = (1 - uv.y) * canvas.height  // Flip y because canvas has y=0 at top
        
        const x = ((z.real - offset.x) * zoom / (4.0 * aspect) + 0.5) * canvas.width;
        // In WebGL, y=0 is at the bottom, but in canvas, y=0 is at the top
        // So we need to flip the y coordinate
        const y = (1.0 - ((z.imag - offset.y) * zoom / 4.0 + 0.5)) * canvas.height;
        
        return { x, y };
    }
    
    // Get color from angle in complex plane
    function getColorFromAngle(z) {
        // In the shader, the coloring is:
        // float angle = atan(z.y, z.x);
        // In GLSL, atan(y, x) is the same as Math.atan2(y, x) in JavaScript
        const angle = Math.atan2(z.imag, z.real);
        const normalizedAngle = (angle + Math.PI) / (2 * Math.PI);
        return hsvToRgb(normalizedAngle, 0.8, 1.0);
    }
    
    // HSV to RGB conversion
    function hsvToRgb(h, s, v) {
        let r, g, b;
        const i = Math.floor(h * 6);
        const f = h * 6 - i;
        const p = v * (1 - s);
        const q = v * (1 - f * s);
        const t = v * (1 - (1 - f) * s);
        
        switch (i % 6) {
            case 0: r = v; g = t; b = p; break;
            case 1: r = q; g = v; b = p; break;
            case 2: r = p; g = v; b = t; break;
            case 3: r = p; g = q; b = v; break;
            case 4: r = t; g = p; b = v; break;
            case 5: r = v; g = p; b = q; break;
        }
        
        return `rgb(${Math.round(r * 255)}, ${Math.round(g * 255)}, ${Math.round(b * 255)})`;
    }
    
    // Draw root indicators on the overlay canvas
    function drawRootIndicators() {
        ctx.clearRect(0, 0, overlayCanvas.width, overlayCanvas.height);
        
        if (!showRoots) return;
        
        const circleRadius = 10; // Constant radius in pixels
        const strokeWidth = 3;   // Constant stroke width in pixels
        
        roots.forEach(root => {
            const pixelPos = complexToPixel(root);
            
            // Check if root is visible on screen
            if (pixelPos.x >= -circleRadius && pixelPos.x <= canvas.width + circleRadius &&
                pixelPos.y >= -circleRadius && pixelPos.y <= canvas.height + circleRadius) {
                
                // Draw white stroke
                ctx.beginPath();
                ctx.arc(pixelPos.x, pixelPos.y, circleRadius + strokeWidth, 0, 2 * Math.PI);
                ctx.fillStyle = 'white';
                ctx.fill();
                
                // Draw colored fill
                ctx.beginPath();
                ctx.arc(pixelPos.x, pixelPos.y, circleRadius, 0, 2 * Math.PI);
                ctx.fillStyle = getColorFromAngle(root);
                ctx.fill();
            }
        });
    }
    
    // Handle UI controls
    maxIterationsSlider.addEventListener('input', () => {
        maxIterations = parseInt(maxIterationsSlider.value);
        iterationsValueSpan.textContent = maxIterations;
        render();
    });
    
    escapeRadiusSlider.addEventListener('input', () => {
        convergenceThreshold = parseFloat(escapeRadiusSlider.value);
        escapeValueSpan.textContent = convergenceThreshold.toFixed(1);
        render();
    });
    
    showRootsCheckbox.addEventListener('change', () => {
        showRoots = showRootsCheckbox.checked;
        render();
    });
    
    resetViewBtn.addEventListener('click', () => {
        zoom = 1.0;
        offset = { x: 0.0, y: 0.0 };
        render();
    });
    
    // Toggle controls visibility
    hideControlsBtn.addEventListener('click', () => {
        controlsPanel.classList.add('hidden');
        toggleControlsBtn.style.display = 'block';
    });
    
    toggleControlsBtn.addEventListener('click', () => {
        controlsPanel.classList.remove('hidden');
        toggleControlsBtn.style.display = 'none';
    });
    
    // Handle mouse interactions
    let isDragging = false;
    let lastMousePos = { x: 0, y: 0 };
    
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
            
            // Calculate aspect ratio
            const aspect = canvas.width / canvas.height;
            
            // Convert screen space to complex plane space with proper aspect ratio handling
            offset.x -= (dx / canvas.width) * (4.0 * aspect) / zoom;
            offset.y += (dy / canvas.height) * 4.0 / zoom;
            
            lastMousePos = { x: e.clientX, y: e.clientY };
            render();
        }
    });
    
    canvas.addEventListener('wheel', (e) => {
        e.preventDefault();
        
        // Get mouse position in normalized device coordinates
        const rect = canvas.getBoundingClientRect();
        const mouseX = e.clientX - rect.left;
        const mouseY = e.clientY - rect.top;
        const ndcX = (mouseX / canvas.width) * 2 - 1;
        const ndcY = (mouseY / canvas.height) * -2 + 1;
        
        // Convert to complex plane coordinates
        const aspect = canvas.width / canvas.height;
        const complexX = ndcX * 2.0 * aspect / zoom + offset.x;
        const complexY = ndcY * 2.0 / zoom + offset.y;
        
        // Zoom factor
        const zoomFactor = e.deltaY > 0 ? 0.9 : 1.1;
        zoom *= zoomFactor;
        
        // Adjust offset to zoom around mouse position
        offset.x = complexX - (ndcX * 2.0 * aspect / zoom);
        offset.y = complexY - (ndcY * 2.0 / zoom);
        
        render();
    });
    
    // Touch support
    let lastTouchDistance = 0;
    
    canvas.addEventListener('touchstart', (e) => {
        if (e.touches.length === 1) {
            isDragging = true;
            lastMousePos = { x: e.touches[0].clientX, y: e.touches[0].clientY };
        } else if (e.touches.length === 2) {
            // Calculate distance between two touch points
            const dx = e.touches[0].clientX - e.touches[1].clientX;
            const dy = e.touches[0].clientY - e.touches[1].clientY;
            lastTouchDistance = Math.sqrt(dx * dx + dy * dy);
        }
        e.preventDefault();
    });
    
    canvas.addEventListener('touchmove', (e) => {
        e.preventDefault();
        
        if (e.touches.length === 1 && isDragging) {
            const dx = e.touches[0].clientX - lastMousePos.x;
            const dy = e.touches[0].clientY - lastMousePos.y;
            
            // Calculate aspect ratio
            const aspect = canvas.width / canvas.height;
            
            // Convert screen space to complex plane space with proper aspect ratio handling
            offset.x -= (dx / canvas.width) * (4.0 * aspect) / zoom;
            offset.y += (dy / canvas.height) * 4.0 / zoom;
            
            lastMousePos = { x: e.touches[0].clientX, y: e.touches[0].clientY };
            render();
        } else if (e.touches.length === 2) {
            // Calculate new distance
            const dx = e.touches[0].clientX - e.touches[1].clientX;
            const dy = e.touches[0].clientY - e.touches[1].clientY;
            const distance = Math.sqrt(dx * dx + dy * dy);
            
            // Calculate pinch center
            const centerX = (e.touches[0].clientX + e.touches[1].clientX) / 2;
            const centerY = (e.touches[0].clientY + e.touches[1].clientY) / 2;
            
            // Convert to normalized device coordinates
            const rect = canvas.getBoundingClientRect();
            const ndcX = ((centerX - rect.left) / canvas.width) * 2 - 1;
            const ndcY = ((centerY - rect.top) / canvas.height) * -2 + 1;
            
            // Convert to complex plane coordinates
            const aspect = canvas.width / canvas.height;
            const complexX = ndcX * 2.0 * aspect / zoom + offset.x;
            const complexY = ndcY * 2.0 / zoom + offset.y;
            
            // Zoom factor based on pinch
            if (lastTouchDistance > 0) {
                const zoomFactor = distance / lastTouchDistance;
                zoom *= zoomFactor;
                
                // Adjust offset to zoom around pinch center
                offset.x = complexX - (ndcX * 2.0 * aspect / zoom);
                offset.y = complexY - (ndcY * 2.0 / zoom);
                
                render();
            }
            
            lastTouchDistance = distance;
        }
    });
    
    canvas.addEventListener('touchend', () => {
        isDragging = false;
        lastTouchDistance = 0;
    });
    
    // Render function
    function render() {
        gl.uniform2f(resolutionLocation, canvas.width, canvas.height);
        gl.uniform1f(zoomLocation, zoom);
        gl.uniform2f(offsetLocation, offset.x, offset.y);
        gl.uniform1i(maxIterationsLocation, maxIterations);
        gl.uniform1f(convergenceThresholdLocation, convergenceThreshold);
        
        gl.drawArrays(gl.TRIANGLES, 0, 6);
        
        // Draw root indicators on overlay canvas
        drawRootIndicators();
    }
    
    // Initial render
    render();
}); 