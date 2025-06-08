precision mediump float;

uniform sampler2D u_texture;
uniform vec2 u_resolution;
uniform float u_dt;
uniform float u_feed;
uniform float u_kill;
uniform float u_dA;
uniform float u_dB;

varying vec2 vTextureCoord;

void main() {
    vec2 uv = vTextureCoord;
    vec2 pixel = 1.0 / u_resolution;
    
    // Get current cell values
    vec4 current = texture2D(u_texture, uv);
    float a = current.r; // A channel is in red
    float b = current.g; // B channel is in green
    
    // Get neighboring cell values
    vec4 up = texture2D(u_texture, uv + vec2(0.0, pixel.y));
    vec4 down = texture2D(u_texture, uv - vec2(0.0, pixel.y));
    vec4 left = texture2D(u_texture, uv - vec2(pixel.x, 0.0));
    vec4 right = texture2D(u_texture, uv + vec2(pixel.x, 0.0));
    
    // Laplacian calculation
    float laplaceA = (up.r + down.r + left.r + right.r - 4.0 * a);
    float laplaceB = (up.g + down.g + left.g + right.g - 4.0 * b);
    
    // Gray-Scott model equations
    float reaction = a * b * b;
    float newA = a + (u_dA * laplaceA - reaction + u_feed * (1.0 - a)) * u_dt;
    float newB = b + (u_dB * laplaceB + reaction - (u_kill + u_feed) * b) * u_dt;
    
    // Clamp values between 0 and 1
    newA = clamp(newA, 0.0, 1.0);
    newB = clamp(newB, 0.0, 1.0);
    
    // Store A in red channel, B in green channel
    gl_FragColor = vec4(newA, newB, 0.0, 1.0);
} 