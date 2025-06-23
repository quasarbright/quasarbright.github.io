precision highp float;

uniform vec2 u_resolution;
uniform int u_maxIterations;
uniform vec2 u_center;
uniform float u_zoom;
uniform vec2 u_initialZ; // Initial value of z (z_0)
uniform vec2 u_paramC;   // Parameter c for Julia sets

// CUSTOM_UNIFORMS_PLACEHOLDER

// Implement hyperbolic functions that aren't built into GLSL
float sinh(float x) {
    return (exp(x) - exp(-x)) / 2.0;
}

float cosh(float x) {
    return (exp(x) + exp(-x)) / 2.0;
}

// Complex number operations
vec2 cmul(vec2 a, vec2 b) {
    return vec2(a.x * b.x - a.y * b.y, a.x * b.y + a.y * b.x);
}

vec2 cdiv(vec2 a, vec2 b) {
    float denominator = b.x * b.x + b.y * b.y;
    return vec2(
        (a.x * b.x + a.y * b.y) / denominator,
        (a.y * b.x - a.x * b.y) / denominator
    );
}

vec2 csquare(vec2 z) {
    return vec2(z.x * z.x - z.y * z.y, 2.0 * z.x * z.y);
}

vec2 ccube(vec2 z) {
    // z^3 = (x + yi)^3 = x^3 - 3xy^2 + i(3x^2y - y^3)
    float x2 = z.x * z.x;
    float y2 = z.y * z.y;
    return vec2(
        z.x * (x2 - 3.0 * y2),
        z.y * (3.0 * x2 - y2)
    );
}

vec2 cpow(vec2 z, float n) {
    // Only works for integer powers, approximation for other powers
    if (n == 2.0) return csquare(z);
    if (n == 3.0) return ccube(z);
    if (n == 1.0) return z;
    if (n == 0.0) return vec2(1.0, 0.0);
    
    // For higher integer powers, use repeated multiplication
    vec2 result = vec2(1.0, 0.0);
    for (int i = 0; i < 10; i++) {
        if (float(i) >= n) break;
        result = cmul(result, z);
    }
    return result;
}

vec2 csin(vec2 z) {
    // sin(x + yi) = sin(x)cosh(y) + i cos(x)sinh(y)
    return vec2(
        sin(z.x) * cosh(z.y),
        cos(z.x) * sinh(z.y)
    );
}

vec2 ccos(vec2 z) {
    // cos(x + yi) = cos(x)cosh(y) - i sin(x)sinh(y)
    return vec2(
        cos(z.x) * cosh(z.y),
        -sin(z.x) * sinh(z.y)
    );
}

vec2 cexp(vec2 z) {
    // exp(x + yi) = e^x * (cos(y) + i sin(y))
    float expx = exp(z.x);
    return vec2(
        expx * cos(z.y),
        expx * sin(z.y)
    );
}

vec2 clog(vec2 z) {
    // log(z) = log|z| + i*arg(z)
    float magnitude = length(z);
    return vec2(
        log(magnitude),
        atan(z.y, z.x)
    );
}

// USER_FUNCTION_PLACEHOLDER
vec2 complex_function(vec2 z, vec2 c) {
    return csquare(z) + c; // Default Mandelbrot function
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
    vec2 pixelPos = u_center + (st * 2.0 - 1.0) * vec2(aspectRatio, 1.0) / u_zoom;
    
    // Determine z and c based on the current mode
    // This will be controlled by JavaScript
    vec2 z, c;
    
    // We're using the same shader for both Mandelbrot and Julia sets
    // The JavaScript code will pass the appropriate values based on the UI controls
    z = u_initialZ;  // If z is in "pixel" mode, JS will set this to vec2(0,0)
    c = u_paramC;    // If c is in "pixel" mode, JS will set this to vec2(0,0)
    
    // If z is in "pixel" mode, use the pixel position
    if (z == vec2(0.0, 0.0)) {
        z = pixelPos;
    }
    
    // If c is in "pixel" mode, use the pixel position
    if (c == vec2(0.0, 0.0)) {
        c = pixelPos;
    }
    
    // Handle custom parameters - if a uniform is set to vec2(0,0), use pixel position
    // This is handled automatically because any custom parameter uniforms
    // that should use pixel position will be set to vec2(0,0) by JavaScript
    
    float zMagnitude = 0.0;
    int iterations = 0;
    bool escaped = false;
    
    // Iterate and track if the point escapes
    for (int i = 0; i < 10000; i++) {
        if (i >= u_maxIterations) break;
        
        // Apply the complex function
        z = complex_function(z, c);
        
        // Track magnitude for coloring
        zMagnitude = length(z);
        
        // Check if the magnitude is getting very large
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
        float hue = fract(normalizedIter * 3.0); // Multiply for more color bands
        float saturation = 0.8;
        float value = escaped ? 1.0 : 0.7; // Slightly dimmer for non-escaping points
        
        vec3 color = hsv2rgb(vec3(hue, saturation, value));
        gl_FragColor = vec4(color, 1.0);
    }
} 