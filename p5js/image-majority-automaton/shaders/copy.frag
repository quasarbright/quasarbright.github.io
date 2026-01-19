precision mediump float;

uniform sampler2D u_image;
uniform vec2 u_resolution;

varying vec2 v_texCoord;

void main() {
    // Simple texture copy - sample and output directly
    vec4 color = texture2D(u_image, v_texCoord);
    gl_FragColor = color;
}
