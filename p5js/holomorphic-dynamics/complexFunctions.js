// Complex function implementations for WebGL visualization

// This file contains implementations of different complex functions
// that can be used in the fragment shader.

// To use these functions, you'll need to modify the fragment shader
// to import and use the desired function.

// Example of how to include a function in the fragment shader:
/*
// In fragment.glsl:

// Complex function: z^2 + c (Mandelbrot)
vec2 complexFunction(vec2 z, vec2 c) {
    return vec2(
        z.x * z.x - z.y * z.y,
        2.0 * z.x * z.y
    ) + c;
}

// Or for Julia sets:
vec2 complexFunction(vec2 z, vec2 c) {
    // Fixed c value for Julia set
    vec2 juliaC = vec2(-0.8, 0.156);
    return vec2(
        z.x * z.x - z.y * z.y,
        2.0 * z.x * z.y
    ) + juliaC;
}
*/

// List of interesting functions to try:

// 1. Mandelbrot function: z^2 + c
function mandelbrot(z, c) {
    return {
        x: z.x * z.x - z.y * z.y + c.x,
        y: 2 * z.x * z.y + c.y
    };
}

// 2. Julia set with fixed c value
function julia(z, fixedC) {
    return {
        x: z.x * z.x - z.y * z.y + fixedC.x,
        y: 2 * z.x * z.y + fixedC.y
    };
}

// 3. Burning Ship fractal: |Re(z)|^2 + i|Im(z)|^2 + c
function burningShip(z, c) {
    return {
        x: Math.abs(z.x) * Math.abs(z.x) - Math.abs(z.y) * Math.abs(z.y) + c.x,
        y: 2 * Math.abs(z.x) * Math.abs(z.y) + c.y
    };
}

// 4. Tricorn (Mandelbar): z̄^2 + c
function tricorn(z, c) {
    return {
        x: z.x * z.x - z.y * z.y + c.x,
        y: -2 * z.x * z.y + c.y
    };
}

// 5. Sine function: sin(z) + c
function sineFractal(z, c) {
    // sin(x + iy) = sin(x)cosh(y) + i cos(x)sinh(y)
    return {
        x: Math.sin(z.x) * Math.cosh(z.y) + c.x,
        y: Math.cos(z.x) * Math.sinh(z.y) + c.y
    };
}

// 6. Cubic Mandelbrot: z^3 + c
function cubicMandelbrot(z, c) {
    // (x + iy)^3 = x^3 - 3xy^2 + i(3x^2y - y^3)
    return {
        x: z.x * z.x * z.x - 3 * z.x * z.y * z.y + c.x,
        y: 3 * z.x * z.x * z.y - z.y * z.y * z.y + c.y
    };
}

// 7. Quartic Mandelbrot: z^4 + c
function quarticMandelbrot(z, c) {
    // (x + iy)^4 = (x^2 - y^2)^2 + i(2xy)^2
    const x2 = z.x * z.x;
    const y2 = z.y * z.y;
    const x2y2 = x2 - y2;
    const xy2 = 2 * z.x * z.y;
    
    return {
        x: x2y2 * x2y2 - xy2 * xy2 + c.x,
        y: 2 * x2y2 * xy2 + c.y
    };
}

// 8. Newton fractal for p(z) = z^3 - 1
function newtonFractal(z) {
    // z - p(z)/p'(z) = z - (z^3 - 1)/(3z^2) = z - (z^2 - 1/z)/3 = (2z^3 + 1)/(3z^2)
    const x2 = z.x * z.x;
    const y2 = z.y * z.y;
    const x2y2 = x2 + y2;
    
    if (x2y2 < 1e-10) {
        return { x: 1, y: 0 }; // Avoid division by zero
    }
    
    const x3 = z.x * x2 - 3 * z.x * y2;
    const y3 = 3 * x2 * z.y - z.y * y2;
    
    const denom = 3 * (x2y2 * x2y2);
    
    return {
        x: (2 * x3 + z.x) / denom,
        y: (2 * y3 + z.y) / denom
    };
}

// Export functions for use in the main script
// (Note: This is for JavaScript usage, not for the shader)
export {
    mandelbrot,
    julia,
    burningShip,
    tricorn,
    sineFractal,
    cubicMandelbrot,
    quarticMandelbrot,
    newtonFractal
}; 