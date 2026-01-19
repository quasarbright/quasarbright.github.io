attribute vec2 a_position;
varying vec2 v_texCoord;

void main() {
  // Transform from clip space [-1, 1] to texture coordinates [0, 1]
  v_texCoord = a_position * 0.5 + 0.5;
  
  // Pass through position for clip space rendering
  gl_Position = vec4(a_position, 0.0, 1.0);
}
