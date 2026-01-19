precision mediump float;

uniform sampler2D u_image;
uniform vec2 u_resolution;
uniform float u_paletteSize;
uniform int u_ditherMode; // 0 = ordered dithering, 1 = nearest-neighbor

varying vec2 v_texCoord;

// 8x8 Bayer matrix for ordered dithering
const mat4 bayerMatrix0 = mat4(
     0.0,  32.0,   8.0,  40.0,
    48.0,  16.0,  56.0,  24.0,
    12.0,  44.0,   4.0,  36.0,
    60.0,  28.0,  52.0,  20.0
);

const mat4 bayerMatrix1 = mat4(
     2.0,  34.0,  10.0,  42.0,
    50.0,  18.0,  58.0,  26.0,
    14.0,  46.0,   6.0,  38.0,
    62.0,  30.0,  54.0,  22.0
);

// Get Bayer matrix value at position (x, y)
float getBayerValue(vec2 pos) {
    // Use mod function instead of % operator for GLSL ES 1.00 compatibility
    int x = int(mod(pos.x, 8.0));
    int y = int(mod(pos.y, 8.0));
    
    int row = int(mod(float(y), 4.0));
    int col = int(mod(float(x), 4.0));
    int matrixIndex = int(mod(floor(float(x) / 4.0), 2.0));
    
    mat4 matrix = (matrixIndex == 0) ? bayerMatrix0 : bayerMatrix1;
    
    if (row == 0) {
        if (col == 0) return matrix[0][0];
        if (col == 1) return matrix[0][1];
        if (col == 2) return matrix[0][2];
        return matrix[0][3];
    } else if (row == 1) {
        if (col == 0) return matrix[1][0];
        if (col == 1) return matrix[1][1];
        if (col == 2) return matrix[1][2];
        return matrix[1][3];
    } else if (row == 2) {
        if (col == 0) return matrix[2][0];
        if (col == 1) return matrix[2][1];
        if (col == 2) return matrix[2][2];
        return matrix[2][3];
    } else {
        if (col == 0) return matrix[3][0];
        if (col == 1) return matrix[3][1];
        if (col == 2) return matrix[3][2];
        return matrix[3][3];
    }
}

void main() {
    // Sample the current pixel color
    vec4 color = texture2D(u_image, v_texCoord);
    
    vec3 dithered;
    
    if (u_ditherMode == 1) {
        // Naive nearest-neighbor dithering
        // Simply quantize each pixel to the nearest palette color
        
        if (u_paletteSize <= 8.0) {
            // Grayscale quantization for small palettes
            float gray = dot(color.rgb, vec3(0.299, 0.587, 0.114));
            float levels = u_paletteSize - 1.0;
            float quantized = floor(gray * levels + 0.5) / levels;
            dithered = vec3(clamp(quantized, 0.0, 1.0));
        } else {
            // Per-channel quantization for larger palettes
            float levelsPerChannel = ceil(pow(u_paletteSize, 1.0 / 3.0));
            float steps = levelsPerChannel - 1.0;
            
            dithered.r = floor(color.r * steps + 0.5) / steps;
            dithered.g = floor(color.g * steps + 0.5) / steps;
            dithered.b = floor(color.b * steps + 0.5) / steps;
            dithered = clamp(dithered, 0.0, 1.0);
        }
    } else {
        // Ordered dithering (Bayer matrix)
        
        // Get pixel coordinates
        vec2 pixelCoord = v_texCoord * u_resolution;
        
        // Get Bayer threshold value (normalized to 0-1)
        float threshold = getBayerValue(pixelCoord) / 64.0;
        
        if (u_paletteSize <= 8.0) {
            // Convert to grayscale using luminance formula
            float gray = dot(color.rgb, vec3(0.299, 0.587, 0.114));
            
            // Calculate number of gray levels
            float levels = u_paletteSize - 1.0;
            
            // Apply dithering
            float ditheredGray = gray + (threshold - 0.5) / levels;
            
            // Quantize
            ditheredGray = floor(ditheredGray * levels + 0.5) / levels;
            ditheredGray = clamp(ditheredGray, 0.0, 1.0);
            
            dithered = vec3(ditheredGray);
        } else {
            // For larger palettes, use per-channel quantization
            // Calculate quantization levels per channel
            float levelsPerChannel = ceil(pow(u_paletteSize, 1.0 / 3.0));
            float steps = levelsPerChannel - 1.0;
            
            // Apply ordered dithering to each color channel
            dithered.r = color.r + (threshold - 0.5) / steps;
            dithered.g = color.g + (threshold - 0.5) / steps;
            dithered.b = color.b + (threshold - 0.5) / steps;
            
            // Quantize to palette (round to nearest level)
            dithered.r = floor(dithered.r * steps + 0.5) / steps;
            dithered.g = floor(dithered.g * steps + 0.5) / steps;
            dithered.b = floor(dithered.b * steps + 0.5) / steps;
            
            // Clamp to valid range
            dithered = clamp(dithered, 0.0, 1.0);
        }
    }
    
    gl_FragColor = vec4(dithered, color.a);
}
