// vertex shader
var vert = `
attribute vec4 a_position;

void main() {
  gl_Position = vec4(a_position.xy, 0.0, 1.0);
}
`;


// set up some global variables
let program
let mouseX = 0;
let mouseY = 0;
let centerx = 0
let centery = 0
let cx = 1
let cy = 0
let zoom = 1.0;
let mousePressed = false;

let canvas = document.createElement("canvas")
// utility functions
function toComplex(x, y) {
  let width = canvas.width
  let height = canvas.height
  let size = Math.min(width, height)
  return [
    // MAKE SURE IT'S ALSO 3.0 IN THE SHADER!
    (3.0 / zoom) * (x - width / 2) / size + centerx,
    (3.0 / zoom) * (y - height / 2) / size + centery
  ]
}

function deltaToComplex(dx, dy) {
  let width = canvas.width
  let height = canvas.height
  let size = Math.min(width, height)
  return [
    // MAKE SURE IT'S ALSO 3.0 IN THE SHADER!
    (3.0 / zoom) * (dx) / size,
    (3.0 / zoom) * (dy) / size
  ]
}
function lerp(a, b, r) {
  return a + r * (b - a)
}

// uniform variables to pass to the shader
let shaderData = {
  'u_mouse': (gl, loc) => gl.uniform2fv(loc, [mouseX, mouseY]),
  'c': (gl, loc) => gl.uniform2fv(loc, [cx, cy]), // the c-value used for the current julia set (set by the user interactively)
  'zoom': (gl, loc) => gl.uniform1f(loc, zoom),
  'center': (gl, loc) => gl.uniform2fv(loc, [centerx, centery]), // the center of the display area as a complex number
}

// canvas event listeners for interactivity
let dragging = false
let dragStart
canvas.addEventListener('mousedown', (e) => {
  rect = canvas.getBoundingClientRect();
  let x = e.clientX - rect.left
  let y = canvas.height - (e.clientY - rect.top)
  if (e.button === 0) { // left mouse button
    mousePressed = true
    rect = canvas.getBoundingClientRect();
    mouseX = x
    mouseY = y
    c = toComplex(mouseX, mouseY)
    cx = c[0]
    cy = c[1]
  }
  if (e.button === 1 || e.button == 0) { // middle click
    dragging = true
    dragStart = [x, y]
  }
})
canvas.addEventListener('mouseup', (e) => {
  if (e.button === 0) {
    mousePressed = false
  }
  if (e.button === 1 || e.button == 0) {
    dragging = false
  }
})

canvas.addEventListener('mousemove', (e) => {
  rect = canvas.getBoundingClientRect();
  let x = e.clientX - rect.left
  let y = canvas.height - (e.clientY - rect.top)
  if (mousePressed) {
    mouseX = x
    mouseY = y
    c = toComplex(mouseX, mouseY)
    cx = c[0]
    cy = c[1]
  }
  if (dragging) {
    let curr = [x, y]
    let dx = curr[0] - dragStart[0]
    let dy = curr[1] - dragStart[1]
    let dcenter = deltaToComplex(dx, dy)
    centerx -= dcenter[0]
    centery -= dcenter[1]
    dragStart = curr
  }
})
canvas.addEventListener('wheel', (e) => {
  rect = canvas.getBoundingClientRect();
  let x = e.clientX - rect.left
  let y = canvas.height - (e.clientY - rect.top)
  z = toComplex(x, y)
  x = z[0]
  y = z[1]
  let sign
  if (e.deltaY > 0) {
    sign = -1
  } else {
    sign = 1
  }
  let strength = .1 * sign
  centerx = lerp(centerx, x, strength)
  centery = lerp(centery, y, strength)
  zoom /= 1 - strength

})
function resizeCanvas(gl) {
  gl.canvas.width = window.innerWidth;
  gl.canvas.height = window.innerHeight;
}


function loadTextFile(url, callback) {
  var request = new XMLHttpRequest();
  request.open('GET', url, true);
  request.addEventListener('load', function () {
    callback(request.responseText);
  });
  request.send();
}

// load the shader, pass it the variables, and display it on the canvas
loadTextFile("shader.frag", function (text) {
  var frag = text;
  canvas.width = window.innerWidth
  canvas.height = window.innerHeight
  document.body.appendChild(canvas)
  var gl = canvas.getContext('webgl');
  var vertexShader = createShader(gl, gl.VERTEX_SHADER, vert);
  var fragmentShader = createShader(gl, gl.FRAGMENT_SHADER, frag);
  program = createProgram(gl, vertexShader, fragmentShader);
  const timeLocation = gl.getUniformLocation(program, "u_time");
  const resolutionLocation = gl.getUniformLocation(program, "u_resolution");
  // const mouseLocation = gl.getUniformLocation(program, "u_mouse");
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
    for (const name in shaderData) {
      const location = gl.getUniformLocation(program, name)
      shaderData[name](gl, location)
    }
    var primitiveType = gl.TRIANGLES;
    var offset = 0;
    var count = 6;
    gl.drawArrays(primitiveType, offset, count);

    requestAnimationFrame(render);
  }
  requestAnimationFrame(render);

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
    return s;
  }
});
