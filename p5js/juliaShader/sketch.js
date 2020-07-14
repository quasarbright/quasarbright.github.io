var vert = `
attribute vec4 a_position;

void main() {
  gl_Position = vec4(a_position.xy, 0.0, 1.0);
}
`;

let mouseX = 0;
let mouseY = 0;
let mousePressed = false;

function loadTextFile(url, callback) {
  var request = new XMLHttpRequest();
  request.open('GET', url, true);
  request.addEventListener('load', function () {
    callback(request.responseText);
  });
  request.send();
}

loadTextFile("shader.frag", function (text) {

  var frag = text;

  let canvas = document.createElement("canvas")
  canvas.addEventListener('mousedown', (e) => {
    if(e.button === 0) {
      mousePressed = true
      rect = canvas.getBoundingClientRect();
      mouseX = e.clientX - rect.left
      mouseY = e.clientY - rect.top
    }
  })
  canvas.addEventListener('mouseup', (e) => {
    if(e.button === 0) {
      mousePressed = false
    }
  })
  canvas.addEventListener('mousemove', (e) => {
    rect = canvas.getBoundingClientRect();
    if(mousePressed){
      mouseX = e.clientX - rect.left
      mouseY = e.clientY - rect.top
    }
  })
  canvas.width = window.innerWidth
  canvas.height = window.innerHeight
  document.body.appendChild(canvas)
  var gl = canvas.getContext('webgl');
  var vertexShader = createShader(gl, gl.VERTEX_SHADER, vert);
  var fragmentShader = createShader(gl, gl.FRAGMENT_SHADER, frag);
  var program = createProgram(gl, vertexShader, fragmentShader);
  const timeLocation = gl.getUniformLocation(program, "u_time");
  const resolutionLocation = gl.getUniformLocation(program, "u_resolution");
  const mouseLocation = gl.getUniformLocation(program, "u_mouse");
  var positionAttributeLocation = gl.getAttribLocation(program, "a_position");
  var positionBuffer = gl.createBuffer();
  gl.bindBuffer(gl.ARRAY_BUFFER, positionBuffer);
  var positions = [
    -1, -1,
    -1, 1,
    1, 1,
    1, 1,
    1, -1,
    -1, -1,
  ];
  gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(positions), gl.STATIC_DRAW);

  function render(time) {
    resizeCanvas(gl);
    gl.viewport(0, 0, gl.canvas.width, gl.canvas.height);
    gl.clearColor(0, 0, 0, 0);
    gl.clear(gl.COLOR_BUFFER_BIT);
    gl.useProgram(program);
    gl.enableVertexAttribArray(positionAttributeLocation);
    gl.bindBuffer(gl.ARRAY_BUFFER, positionBuffer);
    // // Tell the attribute how to get data out of positionBuffer (ARRAY_BUFFER)
    var size = 2;          // 2 components per iteration
    var type = gl.FLOAT;   // the data is 32bit floats
    var normalize = false; // don't normalize the data
    var stride = 0;        // 0 = move forward size * sizeof(type) each iteration to get the next position
    var offset = 0;        // start at the beginning of the buffer
    gl.vertexAttribPointer(
      positionAttributeLocation, size, type, normalize, stride, offset)

    gl.uniform1f(timeLocation, time * 0.001);
    gl.uniform2fv(resolutionLocation, [gl.canvas.width, gl.canvas.height])
    gl.uniform2fv(mouseLocation, [mouseX, mouseY])
    // // draw  
    var primitiveType = gl.TRIANGLES;
    var offset = 0;
    var count = 6;
    gl.drawArrays(primitiveType, offset, count);

    requestAnimationFrame(render);
  }
  requestAnimationFrame(render);

  function resizeCanvas(gl) {
    gl.canvas.width = window.innerWidth;
    gl.canvas.height = window.innerHeight;
  }

  function createProgram(gl, vs, fs) {
    const p = gl.createProgram();
    gl.attachShader(p, vs);
    gl.attachShader(p, fs);
    gl.linkProgram(p);
    // should check for error here!
    return p;
  }

  function createShader(gl, type, src) {
    const s = gl.createShader(type);
    gl.shaderSource(s, src);
    gl.compileShader(s);
    let compiled = gl.getShaderParameter(s, gl.COMPILE_STATUS);
    if (!compiled) {
      // Something went wrong during compilation; get the error
      console.error(gl.getShaderInfoLog(s));
    }
    // should check for error here
    return s;
  }
});