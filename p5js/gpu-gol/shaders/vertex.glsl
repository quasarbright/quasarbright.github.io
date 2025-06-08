attribute vec2 a_position;
varying vec2 v_texCoord;

void main() {
    // Convert from 0->1 to -1->1 coordinate space for WebGL
    gl_Position = vec4(a_position * 2.0 - 1.0, 0, 1);
    
    // Pass the texture coordinates to the fragment shader
    v_texCoord = a_position;
}
