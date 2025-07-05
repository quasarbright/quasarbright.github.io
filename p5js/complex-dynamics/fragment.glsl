precision highp float;

uniform vec2 u_resolution;
uniform int u_maxIterations;
uniform vec2 u_center;
uniform float u_zoom;
uniform vec2 u_initialZ; // Initial value of z (z_0)
uniform float u_time; // Time for animated hue shift
uniform int u_coloringMode; // 0 = escape, 1 = convergence

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

// Function to get the actual value for a parameter, handling pixel mode
vec2 getParameterValue(vec2 param, vec2 pixelPos) {
    // If param is set to the special value (-999,-999), use pixel position
    if (param.x == -999.0 && param.y == -999.0) {
        return pixelPos;
    }
    return param;
}

// USER_FUNCTION_PLACEHOLDER
vec2 complex_function(vec2 z, vec2 pixelPos) {
    // Default function: z² + c (where c will be provided as a custom parameter)
    // This is just a placeholder and will be replaced by the user's function
    return z;
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
    
    // Determine z based on the current mode
    vec2 z = getParameterValue(u_initialZ, pixelPos);
    vec2 originalZ = z; // Store the original z for convergence coloring
    
    float zMagnitude = 0.0;
    int iterations = 0;
    bool escaped = false;
    bool converged = false;
    vec2 prevZ = z;
    
    // Iterate and track if the point escapes or converges
    for (int i = 0; i < 10000; i++) {
        if (i >= u_maxIterations) break;
        
        prevZ = z; // Store previous z value to check for convergence
        
        // Apply the complex function
        z = complex_function(z, pixelPos);
        
        // Track magnitude for coloring
        zMagnitude = length(z);
        
        // Check if the magnitude is getting very large (escape)
        if (zMagnitude > 1000.0) {
            escaped = true;
            iterations = i;
            break;
        }
        
        // Check for convergence (when the change between iterations becomes very small)
        if (i > 0) {
            float delta = length(z - prevZ);
            if (delta < 0.0001) {
                converged = true;
                iterations = i;
                break;
            }
        }
        
        iterations = i;
    }
    
    // Color based on the selected coloring mode
    if (u_coloringMode == 0) {
        // Escape coloring mode (original)
        if (!escaped && iterations >= u_maxIterations - 1) {
            // Points that reached max iterations without escaping are likely in the set
            gl_FragColor = vec4(0.0, 0.0, 0.0, 1.0); // Black
        } else {
            // Improved coloring using logarithmic smoothing
            float log_zn = log(zMagnitude);
            float nu = log(log_zn / log(1000.0)) / log(2.0);
            
            // Smooth iteration count
            float smoothed = float(iterations) + 1.0 - nu;
            
            // Scale by a constant factor instead of max iterations
            float colorIndex = smoothed * 0.01;
            
            // Add time-based hue shift
            float hue = fract(colorIndex - u_time * 0.05);
            float saturation = 1.0;
            float value = escaped ? 1.0 : 0.7;
            
            vec3 color = hsv2rgb(vec3(hue, saturation, value));
            gl_FragColor = vec4(color, 1.0);
        }
    } else {
        // Convergence coloring mode
        if (converged) {
            // For Newton's method, we want to color based on which root it converged to
            
            // Use angle for hue
            float angle = atan(z.y, z.x); // Range: -PI to PI
            float hue = (angle + 3.14159) / 6.28318; // Normalize to 0-1 range
            
            // Add time-based animation
            hue = fract(hue - u_time * 0.05);
            
            // Use convergence speed (iterations) for brightness
            // Apply a gentler mapping to make differences noticeable but not too stark
            // For Newton's method, most points converge in very few iterations
            float iterationRatio = float(iterations) / 15.0; // Normalize to first 15 iterations
            iterationRatio = min(iterationRatio, 1.0); // Cap at 1.0
            
            // Apply a milder non-linear curve for more subtle shading
            float brightness = 1.0 - pow(iterationRatio, 0.7); // Using 0.7 instead of 0.5 for gentler curve
            brightness = 0.4 + 0.6 * brightness; // Scale to range [0.4, 1.0] for less darkness
            
            // Fixed high saturation
            float saturation = 1.0;
            
            vec3 color = hsv2rgb(vec3(hue, saturation, brightness));
            gl_FragColor = vec4(color, 1.0);
        } else {
            // Non-convergent points are black
            gl_FragColor = vec4(0.0, 0.0, 0.0, 1.0);
        }
    }
} 