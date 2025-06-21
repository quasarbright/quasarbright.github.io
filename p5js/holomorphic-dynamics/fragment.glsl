precision highp float;

uniform vec2 u_resolution;
uniform int u_maxIterations;
uniform vec2 u_center;
uniform float u_zoom;

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
    int iterations = 0;
    
    // Iterate without using radius-2 optimization
    for (int i = 0; i < 10000; i++) {
        if (i >= u_maxIterations) break;
        
        // z = z^2 + c
        vec2 zSquared = vec2(
            z.x * z.x - z.y * z.y,
            2.0 * z.x * z.y
        );
        z = zSquared + c;
        
        iterations = i;
        
        // // Check if point escapes
        if (dot(z, z) > 4.0) break;
    }
    
    if (iterations == u_maxIterations - 1) {
        // Points in the set are black
        gl_FragColor = vec4(0.0, 0.0, 0.0, 1.0);
    } else {
        // Color based on escape iterations
        float smoothColor = float(iterations) + 1.0 - log(log(length(z))) / log(2.0);
        float hue = smoothColor / float(u_maxIterations);
        vec3 color = hsv2rgb(vec3(hue, 0.8, 1.0));
        gl_FragColor = vec4(color, 1.0);
    }
} 