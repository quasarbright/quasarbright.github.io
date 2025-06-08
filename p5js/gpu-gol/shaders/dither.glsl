precision highp float;

uniform sampler2D u_image;
uniform vec2 u_resolution;
varying vec2 v_texCoord;

// Convert RGB to grayscale
float toGrayscale(vec3 color) {
    return dot(color, vec3(0.299, 0.587, 0.114));
}

// 4x4 Bayer matrix for ordered dithering
float bayerMatrix(vec2 position) {
    int x = int(mod(position.x, 4.0));
    int y = int(mod(position.y, 4.0));
    
    // 4x4 Bayer matrix values divided by 16
    float value = 0.0;
    
    // Row 0
    if (y == 0 && x == 0) value = 0.0/16.0;
    if (y == 0 && x == 1) value = 8.0/16.0;
    if (y == 0 && x == 2) value = 2.0/16.0;
    if (y == 0 && x == 3) value = 10.0/16.0;
    
    // Row 1
    if (y == 1 && x == 0) value = 12.0/16.0;
    if (y == 1 && x == 1) value = 4.0/16.0;
    if (y == 1 && x == 2) value = 14.0/16.0;
    if (y == 1 && x == 3) value = 6.0/16.0;
    
    // Row 2
    if (y == 2 && x == 0) value = 3.0/16.0;
    if (y == 2 && x == 1) value = 11.0/16.0;
    if (y == 2 && x == 2) value = 1.0/16.0;
    if (y == 2 && x == 3) value = 9.0/16.0;
    
    // Row 3
    if (y == 3 && x == 0) value = 15.0/16.0;
    if (y == 3 && x == 1) value = 7.0/16.0;
    if (y == 3 && x == 2) value = 13.0/16.0;
    if (y == 3 && x == 3) value = 5.0/16.0;
    
    return value;
}

void main() {
    // Get the pixel coordinates
    vec2 pixelCoord = v_texCoord * u_resolution;
    
    // Sample the image
    vec4 color = texture2D(u_image, v_texCoord);
    
    // Convert to grayscale
    float gray = toGrayscale(color.rgb);
    
    // Apply ordered dithering - strict black and white
    float threshold = bayerMatrix(pixelCoord);
    float dithered = gray > threshold ? 1.0 : 0.0;
    
    // Output the dithered result - dark pixels (0.0) represent alive cells
    // to match the expected input for the Game of Life logic
    gl_FragColor = vec4(vec3(1.0 - dithered), 1.0);
}
