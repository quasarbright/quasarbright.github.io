precision highp float;

uniform sampler2D u_state;
uniform vec2 u_resolution;
varying vec2 v_texCoord;

void main() {
    // Get the current cell state (1.0 for alive, 0.0 for dead)
    // With our inverted color scheme, we need to invert the input
    vec4 currentState = texture2D(u_state, v_texCoord);
    float isAlive = currentState.r < 0.5 ? 1.0 : 0.0;  // Inverted: dark pixels are alive
    
    // Calculate the size of one pixel in texture coordinates
    vec2 pixelSize = 1.0 / u_resolution;
    
    // Count alive neighbors
    float aliveNeighbors = 0.0;
    
    // Check all 8 neighbors
    for (int y = -1; y <= 1; y++) {
        for (int x = -1; x <= 1; x++) {
            // Skip the current cell
            if (x == 0 && y == 0) continue;
            
            // Calculate neighbor coordinates with wrapping
            vec2 neighborCoord = fract(v_texCoord + vec2(float(x), float(y)) * pixelSize);
            
            // Get neighbor state (inverted: dark pixels are alive)
            vec4 neighborState = texture2D(u_state, neighborCoord);
            aliveNeighbors += neighborState.r < 0.5 ? 1.0 : 0.0;
        }
    }
    
    // Apply Conway's Game of Life rules
    float nextState = 0.0;
    
    // Rule 1: Any live cell with fewer than two live neighbors dies (underpopulation)
    // Rule 2: Any live cell with two or three live neighbors lives on
    // Rule 3: Any live cell with more than three live neighbors dies (overpopulation)
    // Rule 4: Any dead cell with exactly three live neighbors becomes a live cell (reproduction)
    
    if (isAlive > 0.5) {
        // Cell is currently alive
        nextState = (aliveNeighbors < 2.0 || aliveNeighbors > 3.0) ? 0.0 : 1.0;
    } else {
        // Cell is currently dead
        nextState = (aliveNeighbors == 3.0) ? 1.0 : 0.0;
    }
    
    // Output the next state (inverted: 0.0 for alive cells to appear white after inversion in render shader)
    gl_FragColor = vec4(vec3(1.0 - nextState), 1.0);
}
