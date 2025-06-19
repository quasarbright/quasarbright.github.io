// WebGL variables
let gl, program;
let positionBuffer;

// Shader uniforms
let u_resolution, u_zoom, u_offset, u_max_iterations, u_convergence_threshold, u_shade_intensity, u_mandelbrot_mode;
let u_root1, u_root2, u_root3, u_root4, u_root5, u_root6, u_root7, u_root8, u_root9, u_root10, u_root_count;

// Initialize WebGL and set up the scene
function init() {
    // ... existing code ...
    
    // Get uniform locations
    u_resolution = gl.getUniformLocation(program, "u_resolution");
    u_zoom = gl.getUniformLocation(program, "u_zoom");
    u_offset = gl.getUniformLocation(program, "u_offset");
    u_max_iterations = gl.getUniformLocation(program, "u_max_iterations");
    u_convergence_threshold = gl.getUniformLocation(program, "u_convergence_threshold");
    u_shade_intensity = gl.getUniformLocation(program, "u_shade_intensity");
    u_mandelbrot_mode = gl.getUniformLocation(program, "u_mandelbrot_mode");
    
    u_root1 = gl.getUniformLocation(program, "u_root1");
    u_root2 = gl.getUniformLocation(program, "u_root2");
    u_root3 = gl.getUniformLocation(program, "u_root3");
    u_root4 = gl.getUniformLocation(program, "u_root4");
    u_root5 = gl.getUniformLocation(program, "u_root5");
    u_root6 = gl.getUniformLocation(program, "u_root6");
    u_root7 = gl.getUniformLocation(program, "u_root7");
    u_root8 = gl.getUniformLocation(program, "u_root8");
    u_root9 = gl.getUniformLocation(program, "u_root9");
    u_root10 = gl.getUniformLocation(program, "u_root10");
    u_root_count = gl.getUniformLocation(program, "u_root_count");
    // ... existing code ...
}

// Update uniforms and redraw
function updateUniforms() {
    gl.uniform2f(u_resolution, canvas.width, canvas.height);
    gl.uniform1f(u_zoom, zoom);
    gl.uniform2f(u_offset, offset.x, offset.y);
    gl.uniform1i(u_max_iterations, 100);
    gl.uniform1f(u_convergence_threshold, 0.00001);
    gl.uniform1f(u_shade_intensity, parseFloat(document.getElementById('shade-intensity').value));
    gl.uniform1i(u_mandelbrot_mode, document.getElementById('mandelbrot-mode').checked ? 1 : 0);
    
    // Set root uniforms
    for (let i = 0; i < roots.length && i < 10; i++) {
        const rootUniform = eval(`u_root${i+1}`);
        gl.uniform2f(rootUniform, roots[i].x, roots[i].y);
    }
    
    gl.uniform1i(u_root_count, roots.length);
    
    // Draw the scene
    gl.drawArrays(gl.TRIANGLE_STRIP, 0, 4);
}

// ... existing code ...

// Set up event listeners
function setupEventListeners() {
    // ... existing code ...
    
    // Shade intensity slider
    document.getElementById('shade-intensity').addEventListener('input', function() {
        const value = parseFloat(this.value);
        document.getElementById('intensity-value').textContent = value.toFixed(1);
        updateUniforms();
    });
    
    // Mandelbrot mode checkbox
    document.getElementById('mandelbrot-mode').addEventListener('change', function() {
        updateUniforms();
    });
    
    // ... existing code ...
}
// ... existing code ... 