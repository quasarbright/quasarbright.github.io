precision highp float;

uniform sampler2D u_state;
varying vec2 v_texCoord;

void main() {
    // Sample the state texture
    vec4 state = texture2D(u_state, v_texCoord);
    
    // Invert the colors so that white (1.0) represents alive cells
    // and black (0.0) represents dead cells
    gl_FragColor = vec4(vec3(1.0 - state.r), 1.0);
}
