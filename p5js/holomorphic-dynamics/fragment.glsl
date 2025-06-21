precision highp float;

uniform vec2 u_resolution;
uniform int u_maxIterations;
uniform vec2 u_center;
uniform float u_zoom;

// Complex number operations
vec2 complex_mul(vec2 a, vec2 b) {
    return vec2(a.x * b.x - a.y * b.y, a.x * b.y + a.y * b.x);
}

vec2 complex_square(vec2 z) {
    return vec2(z.x * z.x - z.y * z.y, 2.0 * z.x * z.y);
}

// Complex function to iterate (can be replaced with other functions)
vec2 complex_function(vec2 z, vec2 c) {
    // Mandelbrot: f(z) = z^2 + c
    return complex_square(z) + c;
    
    // For other functions, uncomment and modify as needed:
    // Example - Burning Ship:
    // return vec2(abs(z.x) * abs(z.x) - abs(z.y) * abs(z.y), 2.0 * abs(z.x) * abs(z.y)) + c;
    
    // Example - Cubic Mandelbrot:
    // vec2 z2 = complex_square(z);
    // return vec2(z.x * z2.x - z.y * z2.y, z.x * z2.y + z.y * z2.x) + c;
}

vec3 hsv2rgb(vec3 c) {
    vec4 K = vec4(1.0, 2.0 / 3.0, 1.0 / 3.0, 3.0);
    vec3 p = abs(fract(c.xxx + K.xyz) * 6.0 - K.www);
    return c.z * mix(K.xxx, clamp(p - K.xxx, 0.0, 1.0), c.y);
}

void main() {
    vec2 st = gl_FragCoord.xy / u_resolution.xy;
    
    // Map to complex plane
    float aspectRatio = u_resolution.x / u_resolution.y;
    vec2 c = u_center + (st * 2.0 - 1.0) * vec2(aspectRatio, 1.0) / u_zoom;
    
    // Initialize
    vec2 z = vec2(0.0);
    float zMagnitude = 0.0;
    int iterations = 0;
    bool escaped = false;
    
    // Iterate and track if the point escapes
    // We'll still check for escape to determine coloring, but we won't use a fixed radius
    for (int i = 0; i < 10000; i++) {
        if (i >= u_maxIterations) break;
        
        // Apply the complex function
        z = complex_function(z, c);
        
        // Track magnitude for coloring
        zMagnitude = length(z);
        
        // Check if the magnitude is getting very large
        // This helps distinguish between points that are definitely escaping
        // and those that might be in the set
        if (zMagnitude > 1000.0) {
            escaped = true;
            break;
        }
        
        iterations = i;
    }
    
    // Normalize iteration count for coloring
    float normalizedIter = float(iterations) / float(u_maxIterations);
    
    // Color based on whether the point escaped and how many iterations it took
    if (!escaped && iterations >= u_maxIterations - 1) {
        // Points that reached max iterations without escaping are likely in the set
        gl_FragColor = vec4(0.0, 0.0, 0.0, 1.0); // Black
    } else {
        // Color based on iteration count
        float hue = fract(normalizedIter * 10.0); // Multiply for more color bands
        float saturation = 0.8;
        float value = escaped ? 1.0 : 0.7; // Slightly dimmer for non-escaping points
        
        vec3 color = hsv2rgb(vec3(hue, saturation, value));
        gl_FragColor = vec4(color, 1.0);
    }
} 