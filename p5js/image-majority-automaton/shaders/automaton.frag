precision mediump float;

uniform sampler2D u_state;
uniform vec2 u_resolution;
uniform float u_paletteSize;
uniform float u_random;

varying vec2 v_texCoord;

// Get the color at the current position plus offset
vec3 getColorOffset(float dx, float dy) {
    vec2 offset = vec2(dx, dy) / u_resolution;
    vec2 coord = clamp(v_texCoord + offset, vec2(0.0), vec2(1.0));
    return texture2D(u_state, coord).rgb;
}

// Pseudo-random number generator
float rand(vec2 co) {
    return fract(sin(dot(co, vec2(12.9898, 78.233)) + u_random) * 43758.5453);
}

// Compare two colors for equality (with small tolerance for floating point)
bool colorsEqual(vec3 a, vec3 b) {
    return distance(a, b) < 0.01;
}

void main() {
    // Collect all 8 neighbor colors (Moore neighborhood)
    // We need to manually unroll this to avoid dynamic indexing
    vec3 n0 = getColorOffset(-1.0, -1.0);
    vec3 n1 = getColorOffset( 0.0, -1.0);
    vec3 n2 = getColorOffset( 1.0, -1.0);
    vec3 n3 = getColorOffset(-1.0,  0.0);
    vec3 n4 = getColorOffset( 1.0,  0.0);
    vec3 n5 = getColorOffset(-1.0,  1.0);
    vec3 n6 = getColorOffset( 0.0,  1.0);
    vec3 n7 = getColorOffset( 1.0,  1.0);
    
    // Count occurrences of each unique color
    // We'll track up to 8 unique colors (one per neighbor)
    vec3 uniqueColors[8];
    float counts[8];
    
    // Initialize
    for (int i = 0; i < 8; i++) {
        counts[i] = 0.0;
        uniqueColors[i] = vec3(0.0);
    }
    
    float numUniqueColors = 0.0;
    
    // Process each neighbor - manually unrolled to avoid dynamic indexing
    vec3 neighbors[8];
    neighbors[0] = n0;
    neighbors[1] = n1;
    neighbors[2] = n2;
    neighbors[3] = n3;
    neighbors[4] = n4;
    neighbors[5] = n5;
    neighbors[6] = n6;
    neighbors[7] = n7;
    
    // Count each neighbor color
    for (int i = 0; i < 8; i++) {
        vec3 neighborColor = neighbors[i];
        
        // Check if this color is already in our unique list
        bool found = false;
        
        // Manually unroll the search to avoid dynamic indexing
        for (int j = 0; j < 8; j++) {
            if (float(j) < numUniqueColors && colorsEqual(neighborColor, uniqueColors[j])) {
                counts[j] += 1.0;
                found = true;
                break;
            }
        }
        
        // If not found, add it as a new unique color
        if (!found && numUniqueColors < 8.0) {
            int idx = int(numUniqueColors);
            if (idx == 0) { uniqueColors[0] = neighborColor; counts[0] = 1.0; }
            else if (idx == 1) { uniqueColors[1] = neighborColor; counts[1] = 1.0; }
            else if (idx == 2) { uniqueColors[2] = neighborColor; counts[2] = 1.0; }
            else if (idx == 3) { uniqueColors[3] = neighborColor; counts[3] = 1.0; }
            else if (idx == 4) { uniqueColors[4] = neighborColor; counts[4] = 1.0; }
            else if (idx == 5) { uniqueColors[5] = neighborColor; counts[5] = 1.0; }
            else if (idx == 6) { uniqueColors[6] = neighborColor; counts[6] = 1.0; }
            else if (idx == 7) { uniqueColors[7] = neighborColor; counts[7] = 1.0; }
            numUniqueColors += 1.0;
        }
    }
    
    // Find the maximum count
    float maxCount = 0.0;
    for (int i = 0; i < 8; i++) {
        if (float(i) < numUniqueColors) {
            maxCount = max(maxCount, counts[i]);
        }
    }
    
    // Find all colors with the maximum count
    vec3 maxColors[8];
    float numMaxColors = 0.0;
    
    for (int i = 0; i < 8; i++) {
        maxColors[i] = vec3(0.0);
    }
    
    for (int i = 0; i < 8; i++) {
        if (float(i) < numUniqueColors && counts[i] == maxCount) {
            int idx = int(numMaxColors);
            if (idx == 0) maxColors[0] = uniqueColors[i];
            else if (idx == 1) maxColors[1] = uniqueColors[i];
            else if (idx == 2) maxColors[2] = uniqueColors[i];
            else if (idx == 3) maxColors[3] = uniqueColors[i];
            else if (idx == 4) maxColors[4] = uniqueColors[i];
            else if (idx == 5) maxColors[5] = uniqueColors[i];
            else if (idx == 6) maxColors[6] = uniqueColors[i];
            else if (idx == 7) maxColors[7] = uniqueColors[i];
            numMaxColors += 1.0;
        }
    }
    
    // Choose a color from the ones with maximum count
    vec3 selectedColor = maxColors[0];
    
    if (numMaxColors > 1.0) {
        // Multiple colors tied - use deterministic random selection
        float randomValue = rand(v_texCoord);
        float selectedIndexFloat = floor(randomValue * numMaxColors);
        int selectedIndex = int(selectedIndexFloat);
        
        // Manually select based on index to avoid dynamic indexing
        if (selectedIndex == 0) selectedColor = maxColors[0];
        else if (selectedIndex == 1) selectedColor = maxColors[1];
        else if (selectedIndex == 2) selectedColor = maxColors[2];
        else if (selectedIndex == 3) selectedColor = maxColors[3];
        else if (selectedIndex == 4) selectedColor = maxColors[4];
        else if (selectedIndex == 5) selectedColor = maxColors[5];
        else if (selectedIndex == 6) selectedColor = maxColors[6];
        else if (selectedIndex == 7) selectedColor = maxColors[7];
    }
    
    // Output the selected color
    gl_FragColor = vec4(selectedColor, 1.0);
}
