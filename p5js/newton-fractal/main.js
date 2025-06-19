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
    overlayCanvas.style.zIndex = '1';
    document.body.appendChild(overlayCanvas);
    const ctx = overlayCanvas.getContext('2d');
    
    // Function to check if a point is inside the controls panel
    function isPointInControls(x, y) {
        const rect = controlsPanel.getBoundingClientRect();
        return x >= rect.left && x <= rect.right && y >= rect.top && y <= rect.bottom;
    }
    
    // Pastel color palette - same as in the shader
    const PASTEL_COLORS = [
        'rgb(255, 51, 51)',    // Intense Red
        'rgb(51, 255, 51)',    // Intense Green
        'rgb(51, 102, 255)',   // Intense Blue
        'rgb(255, 230, 26)',   // Intense Yellow
        'rgb(230, 51, 255)',   // Intense Purple
        'rgb(26, 204, 255)',   // Intense Cyan
        'rgb(255, 51, 179)',   // Intense Magenta
        'rgb(153, 255, 26)',   // Intense Lime
        'rgb(255, 128, 26)',   // Intense Orange
        'rgb(128, 51, 255)',   // Intense Violet
        'rgb(26, 179, 153)',   // Intense Teal
        'rgb(230, 153, 26)'    // Intense Gold
    ];
    
    // Initial polynomial roots in the complex plane
    const defaultRoots = [
        { real: 1, imag: 0 },  // 1
        { real: -1, imag: 0 }, // -1
        { real: 0, imag: 1 },  // i
        { real: 0, imag: -1 }  // -i
    ];
    
    // Polynomial roots that can be modified
    let roots = JSON.parse(JSON.stringify(defaultRoots));
    
    // Get UI elements
    const controlsPanel = document.getElementById('controls');
    const resetViewBtn = document.getElementById('reset-view');
    const resetRootsBtn = document.getElementById('reset-roots');
    const addRootBtn = document.getElementById('add-root');
    const removeRootBtn = document.getElementById('remove-root');
    const rootCountDisplay = document.getElementById('root-count');
    const shadeIntensitySlider = document.getElementById('shade-intensity');
    const intensityValueDisplay = document.getElementById('intensity-value');
    
    // About modal elements
    const aboutBtn = document.getElementById('about-btn');
    const modalOverlay = document.getElementById('modal-overlay');
    const closeModalBtn = document.getElementById('close-modal');
    
    // Function to update the root count display
    function updateRootCountDisplay() {
        rootCountDisplay.textContent = `${roots.length} root${roots.length !== 1 ? 's' : ''}`;
    }
    
    // Prevent overlay canvas from capturing events when interacting with controls
    controlsPanel.addEventListener('mousedown', (e) => {
        e.stopPropagation();
    });
    
    controlsPanel.addEventListener('touchstart', (e) => {
        e.stopPropagation();
    });
    
    controlsPanel.addEventListener('wheel', (e) => {
        e.stopPropagation();
    });
    
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
    
    // Check if shaders were successfully compiled before attaching
    if (!vertexShader || !fragmentShader) {
        console.error('Failed to compile shaders. Cannot create program.');
        return;
    }
    
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
    const rootCountLocation = gl.getUniformLocation(program, 'u_root_count');
    const shadeIntensityLocation = gl.getUniformLocation(program, 'u_shade_intensity');
    
    // Root location uniforms - support up to 10 roots
    const rootLocations = [
        gl.getUniformLocation(program, 'u_root1'),
        gl.getUniformLocation(program, 'u_root2'),
        gl.getUniformLocation(program, 'u_root3'),
        gl.getUniformLocation(program, 'u_root4'),
        gl.getUniformLocation(program, 'u_root5'),
        gl.getUniformLocation(program, 'u_root6'),
        gl.getUniformLocation(program, 'u_root7'),
        gl.getUniformLocation(program, 'u_root8'),
        gl.getUniformLocation(program, 'u_root9'),
        gl.getUniformLocation(program, 'u_root10')
    ];
    
    // Initial values
    let zoom = 1.0;
    let offset = { x: 0.0, y: 0.0 };
    let maxIterations = 100;
    let convergenceThreshold = 5.0;
    let showRoots = true;
    let shadeIntensity = 1.5;
    let mandelbrotMode = false;
    let showTrail = true;
    let trailPoints = [];
    
    // Root dragging state
    let isDraggingRoot = false;
    let draggedRootIndex = -1;
    let hoveredRootIndex = -1;
    const circleRadius = 10; // Constant radius in pixels
    const strokeWidth = 3;   // Constant stroke width in pixels
    const hoverStrokeColor = '#00AAFF';
    const normalStrokeColor = '#FFFFFF';
    
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
    
    // Convert canvas pixel coordinates to complex plane coordinates
    function pixelToComplex(pixel) {
        const aspect = canvas.width / canvas.height;
        
        // Normalized device coordinates (WebGL convention: y=0 at bottom)
        const uvX = pixel.x / canvas.width;
        const uvY = 1.0 - (pixel.y / canvas.height); // Flip y because canvas has y=0 at top
        
        // Convert to complex plane
        const real = (uvX - 0.5) * 4.0 * aspect / zoom + offset.x;
        const imag = (uvY - 0.5) * 4.0 / zoom + offset.y;
        
        return { real, imag };
    }
    
    // Get root color from palette
    function getRootColor(rootIndex) {
        return PASTEL_COLORS[rootIndex % PASTEL_COLORS.length];
    }
    
    // Check if a point is near a root
    function isNearRoot(point, rootIndex) {
        if (rootIndex < 0 || rootIndex >= roots.length) return false;
        
        const rootPixel = complexToPixel(roots[rootIndex]);
        const dx = point.x - rootPixel.x;
        const dy = point.y - rootPixel.y;
        const distance = Math.sqrt(dx * dx + dy * dy);
        
        return distance <= circleRadius + strokeWidth;
    }
    
    // Find the index of the root near a point
    function findRootNearPoint(point) {
        for (let i = 0; i < roots.length; i++) {
            if (isNearRoot(point, i)) {
                return i;
            }
        }
        return -1;
    }
    
    // Draw root indicators on the overlay canvas
    function drawRootIndicators() {
        ctx.clearRect(0, 0, overlayCanvas.width, overlayCanvas.height);
        
        // Draw the Newton's method trail
        drawTrail();
        
        if (!showRoots) return;
        
        roots.forEach((root, index) => {
            const pixelPos = complexToPixel(root);
            
            // Check if root is visible on screen
            if (pixelPos.x >= -circleRadius && pixelPos.x <= canvas.width + circleRadius &&
                pixelPos.y >= -circleRadius && pixelPos.y <= canvas.height + circleRadius) {
                
                // Draw stroke (white normally, blue when hovered/dragged)
                ctx.beginPath();
                ctx.arc(pixelPos.x, pixelPos.y, circleRadius + strokeWidth, 0, 2 * Math.PI);
                ctx.fillStyle = (index === hoveredRootIndex || index === draggedRootIndex) ? 
                    hoverStrokeColor : normalStrokeColor;
                ctx.fill();
                
                // Draw colored fill from palette
                ctx.beginPath();
                ctx.arc(pixelPos.x, pixelPos.y, circleRadius, 0, 2 * Math.PI);
                ctx.fillStyle = getRootColor(index);
                ctx.fill();
            }
        });
    }
    
    // Generate a random point on the unit circle
    function randomPointOnUnitCircle() {
        const angle = Math.random() * 2 * Math.PI;
        return {
            real: Math.cos(angle),
            imag: Math.sin(angle)
        };
    }
    
    // Complex number operations for trail calculations
    function complexMul(a, b) {
        return {
            real: a.real * b.real - a.imag * b.imag,
            imag: a.real * b.imag + a.imag * b.real
        };
    }
    
    function complexDiv(a, b) {
        const denom = b.real * b.real + b.imag * b.imag;
        return {
            real: (a.real * b.real + a.imag * b.imag) / denom,
            imag: (a.imag * b.real - a.real * b.imag) / denom
        };
    }
    
    // Calculate f(z) = product of (z-root) for all roots
    function f(z) {
        let result = { real: 1.0, imag: 0.0 }; // Start with 1+0i
        
        for (let i = 0; i < roots.length; i++) {
            const factor = {
                real: z.real - roots[i].real,
                imag: z.imag - roots[i].imag
            };
            result = complexMul(result, factor);
        }
        
        return result;
    }
    
    // Calculate derivative f'(z) using the product rule
    function df(z) {
        // If we have no roots or just one root, the derivative is simple
        if (roots.length <= 0) return { real: 0.0, imag: 0.0 };
        if (roots.length == 1) return { real: 1.0, imag: 0.0 };
        
        // For multiple roots, we compute the sum of products
        let result = { real: 0.0, imag: 0.0 };
        
        // For each root, we compute the product of (z-root) for all other roots
        for (let i = 0; i < roots.length; i++) {
            let term = { real: 1.0, imag: 0.0 };
            
            for (let j = 0; j < roots.length; j++) {
                if (i === j) continue; // Skip the current root
                
                const factor = {
                    real: z.real - roots[j].real,
                    imag: z.imag - roots[j].imag
                };
                term = complexMul(term, factor);
            }
            
            result.real += term.real;
            result.imag += term.imag;
        }
        
        return result;
    }
    
    // Newton's method: z = z - f(z) / f'(z)
    function newtonStep(z) {
        const fz = f(z);
        const dfz = df(z);
        const quotient = complexDiv(fz, dfz);
        
        return {
            real: z.real - quotient.real,
            imag: z.imag - quotient.imag
        };
    }
    
    // Calculate Newton's method trail for a given starting point
    function calculateTrail(startPoint) {
        if (mandelbrotMode) return []; // No trail in mandelbrot mode
        
        const trail = [startPoint];
        let z = { ...startPoint };
        const epsilon = 0.00001;
        const maxSteps = 20; // Limit the number of steps to avoid infinite loops
        
        for (let i = 0; i < maxSteps; i++) {
            const prevZ = { ...z };
            z = newtonStep(z);
            trail.push(z);
            
            // Check for convergence
            const dx = z.real - prevZ.real;
            const dy = z.imag - prevZ.imag;
            if (Math.sqrt(dx * dx + dy * dy) < epsilon) {
                break;
            }
        }
        
        return trail;
    }
    
    // Draw the Newton's method trail on the overlay canvas
    function drawTrail() {
        if (!showTrail || mandelbrotMode || trailPoints.length < 2) return;
        
        ctx.beginPath();
        ctx.strokeStyle = 'white';
        ctx.lineWidth = 2;
        
        const startPixel = complexToPixel(trailPoints[0]);
        ctx.moveTo(startPixel.x, startPixel.y);
        
        for (let i = 1; i < trailPoints.length; i++) {
            const pixel = complexToPixel(trailPoints[i]);
            ctx.lineTo(pixel.x, pixel.y);
        }
        
        ctx.stroke();
    }
    
    // Handle UI controls
    resetViewBtn.addEventListener('click', (e) => {
        e.stopPropagation();
        zoom = 1.0;
        offset = { x: 0.0, y: 0.0 };
        render();
    });
    
    resetRootsBtn.addEventListener('click', (e) => {
        e.stopPropagation();
        roots = JSON.parse(JSON.stringify(defaultRoots));
        updateRootCountDisplay();
        render();
    });
    
    addRootBtn.addEventListener('click', (e) => {
        e.stopPropagation();
        if (roots.length < 10) {
            const newRoot = randomPointOnUnitCircle();
            roots.push(newRoot);
            console.log(`Added root #${roots.length}: (${newRoot.real.toFixed(2)}, ${newRoot.imag.toFixed(2)})`);
            updateRootCountDisplay();
            render();
        }
    });
    
    removeRootBtn.addEventListener('click', (e) => {
        e.stopPropagation();
        if (roots.length > 1) {
            const removedRoot = roots.pop();
            console.log(`Removed root: (${removedRoot.real.toFixed(2)}, ${removedRoot.imag.toFixed(2)}), ${roots.length} roots remaining`);
            updateRootCountDisplay();
            render();
        }
    });
    
    // Shade intensity slider
    shadeIntensitySlider.addEventListener('input', (e) => {
        e.stopPropagation();
        shadeIntensity = parseFloat(shadeIntensitySlider.value);
        intensityValueDisplay.textContent = shadeIntensity.toFixed(1);
        render();
    });
    
    // Mandelbrot mode checkbox
    document.getElementById('mandelbrot-mode').addEventListener('change', function(e) {
        e.stopPropagation();
        mandelbrotMode = this.checked;
        render();
    });
    
    // Show trail checkbox
    document.getElementById('show-trail').addEventListener('change', function(e) {
        e.stopPropagation();
        showTrail = this.checked;
        render();
    });
    
    // Handle mouse interactions for panning and root dragging
    let isPanning = false;
    let lastMousePos = { x: 0, y: 0 };
    
    overlayCanvas.addEventListener('mousedown', (e) => {
        // Ignore if the event originated from a control element or if the point is inside the controls
        if (controlsPanel.contains(e.target) || isPointInControls(e.clientX, e.clientY)) {
            return;
        }
        
        const mousePos = { x: e.clientX, y: e.clientY };
        const rootIndex = findRootNearPoint(mousePos);
        
        if (rootIndex >= 0 && showRoots) {
            // Start dragging a root
            isDraggingRoot = true;
            draggedRootIndex = rootIndex;
            
            // Update the root position immediately
            const complexPos = pixelToComplex(mousePos);
            roots[rootIndex] = complexPos;
        } else {
            // Start panning
            isPanning = true;
            lastMousePos = mousePos;
            trailPoints = []; // Clear the trail when panning starts
        }
        
        render();
    });
    
    overlayCanvas.addEventListener('mousemove', (e) => {
        // Ignore if the event originated from a control element or if the point is inside the controls
        if (controlsPanel.contains(e.target) || isPointInControls(e.clientX, e.clientY)) {
            overlayCanvas.style.cursor = 'default';
            return;
        }
        
        const mousePos = { x: e.clientX, y: e.clientY };
        
        if (isDraggingRoot) {
            // Update the dragged root position
            const complexPos = pixelToComplex(mousePos);
            roots[draggedRootIndex] = complexPos;
            render();
        } else if (isPanning) {
            // Pan the view
            const dx = e.clientX - lastMousePos.x;
            const dy = e.clientY - lastMousePos.y;
            
            // Calculate aspect ratio
            const aspect = canvas.width / canvas.height;
            
            // Convert screen space to complex plane space with proper aspect ratio handling
            offset.x -= (dx / canvas.width) * (4.0 * aspect) / zoom;
            offset.y += (dy / canvas.height) * 4.0 / zoom;
            
            lastMousePos = mousePos;
            render();
        } else if (showRoots) {
            // Check for hover state
            const rootIndex = findRootNearPoint(mousePos);
            if (rootIndex !== hoveredRootIndex) {
                hoveredRootIndex = rootIndex;
                // Change cursor to pointer when over a root
                overlayCanvas.style.cursor = rootIndex >= 0 ? 'pointer' : 'default';
                render();
            }
        }
        
        // Calculate and update trail for the current mouse position
        if (showTrail && !mandelbrotMode && !isPanning && !isDraggingRoot) {
            const complexPos = pixelToComplex(mousePos);
            trailPoints = calculateTrail(complexPos);
            render();
        }
    });
    
    overlayCanvas.addEventListener('mouseup', () => {
        isPanning = false;
        isDraggingRoot = false;
        draggedRootIndex = -1;
    });
    
    overlayCanvas.addEventListener('mouseleave', () => {
        isPanning = false;
        isDraggingRoot = false;
        draggedRootIndex = -1;
        hoveredRootIndex = -1;
        overlayCanvas.style.cursor = 'default';
        trailPoints = []; // Clear the trail when mouse leaves
        render();
    });
    
    // Wheel event for zooming
    overlayCanvas.addEventListener('wheel', (e) => {
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
        
        // Clear the trail when zooming
        trailPoints = [];
        
        render();
    });
    
    // Touch support
    let lastTouchDistance = 0;
    let touchStartPos = null;
    
    overlayCanvas.addEventListener('touchstart', (e) => {
        e.preventDefault();
        
        // Ignore if the event originated from a control element or if the point is inside the controls
        if (controlsPanel.contains(e.target) || isPointInControls(e.touches[0].clientX, e.touches[0].clientY)) {
            return;
        }
        
        if (e.touches.length === 1) {
            const touchPos = { x: e.touches[0].clientX, y: e.touches[0].clientY };
            const rootIndex = findRootNearPoint(touchPos);
            
            if (rootIndex >= 0 && showRoots) {
                // Start dragging a root
                isDraggingRoot = true;
                draggedRootIndex = rootIndex;
                
                // Update the root position immediately
                const complexPos = pixelToComplex(touchPos);
                roots[rootIndex] = complexPos;
            } else {
                // Start panning
                isPanning = true;
                lastMousePos = touchPos;
            }
            
            touchStartPos = touchPos;
            
            // Clear trail on touch interaction
            trailPoints = [];
        } else if (e.touches.length === 2) {
            // Calculate distance between two touch points
            const dx = e.touches[0].clientX - e.touches[1].clientX;
            const dy = e.touches[0].clientY - e.touches[1].clientY;
            lastTouchDistance = Math.sqrt(dx * dx + dy * dy);
            
            // Cancel any dragging or panning
            isPanning = false;
            isDraggingRoot = false;
            draggedRootIndex = -1;
            
            // Clear trail on pinch
            trailPoints = [];
        }
        
        render();
    });
    
    overlayCanvas.addEventListener('touchmove', (e) => {
        e.preventDefault();
        
        if (e.touches.length === 1) {
            const touchPos = { x: e.touches[0].clientX, y: e.touches[0].clientY };
            
            // Only process if we've moved more than a tiny amount (helps with accidental moves)
            if (touchStartPos && 
                Math.abs(touchPos.x - touchStartPos.x) < 3 && 
                Math.abs(touchPos.y - touchStartPos.y) < 3) {
                return;
            }
            
            if (isDraggingRoot) {
                // Update the dragged root position
                const complexPos = pixelToComplex(touchPos);
                roots[draggedRootIndex] = complexPos;
                render();
            } else if (isPanning) {
                // Pan the view
                const dx = touchPos.x - lastMousePos.x;
                const dy = touchPos.y - lastMousePos.y;
                
                // Calculate aspect ratio
                const aspect = canvas.width / canvas.height;
                
                // Convert screen space to complex plane space with proper aspect ratio handling
                offset.x -= (dx / canvas.width) * (4.0 * aspect) / zoom;
                offset.y += (dy / canvas.height) * 4.0 / zoom;
                
                lastMousePos = touchPos;
                render();
            } else if (showTrail && !mandelbrotMode) {
                // Calculate and update trail for the current touch position
                const complexPos = pixelToComplex(touchPos);
                trailPoints = calculateTrail(complexPos);
                render();
            }
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
    
    overlayCanvas.addEventListener('touchend', () => {
        isPanning = false;
        isDraggingRoot = false;
        draggedRootIndex = -1;
        lastTouchDistance = 0;
        touchStartPos = null;
        trailPoints = []; // Clear the trail when touch ends
    });
    
    // Render function
    function render() {
        // Make sure the program is being used
        gl.useProgram(program);
        
        gl.uniform2f(resolutionLocation, canvas.width, canvas.height);
        gl.uniform1f(zoomLocation, zoom);
        gl.uniform2f(offsetLocation, offset.x, offset.y);
        gl.uniform1i(maxIterationsLocation, maxIterations);
        gl.uniform1f(convergenceThresholdLocation, convergenceThreshold);
        gl.uniform1f(shadeIntensityLocation, shadeIntensity);
        gl.uniform1i(gl.getUniformLocation(program, 'u_mandelbrot_mode'), mandelbrotMode ? 1 : 0);
        
        // Pass the actual root count to the shader
        gl.uniform1i(rootCountLocation, roots.length);
        console.log(`Rendering with ${roots.length} roots${mandelbrotMode ? ' in Mandelbrot mode' : ''}`);
        
        // Pass all roots to the shader
        for (let i = 0; i < roots.length; i++) {
            gl.uniform2f(rootLocations[i], roots[i].real, roots[i].imag);
        }
        
        // Fill any unused root slots with default values
        for (let i = roots.length; i < 10; i++) {
            gl.uniform2f(rootLocations[i], 0.0, 0.0);
        }
        
        gl.drawArrays(gl.TRIANGLES, 0, 6);
        
        // Draw root indicators on overlay canvas
        drawRootIndicators();
    }
    
    // Initial render
    updateRootCountDisplay();
    render();
    
    // About modal functionality
    aboutBtn.addEventListener('click', (e) => {
        e.stopPropagation();
        modalOverlay.style.display = 'flex';
    });
    
    closeModalBtn.addEventListener('click', () => {
        modalOverlay.style.display = 'none';
    });
    
    modalOverlay.addEventListener('click', (e) => {
        if (e.target === modalOverlay) {
            modalOverlay.style.display = 'none';
        }
    });
}); 