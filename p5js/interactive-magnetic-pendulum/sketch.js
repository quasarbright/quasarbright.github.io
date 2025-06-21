// vertex shader
var vert = `
attribute vec4 a_position;

void main() {
  gl_Position = vec4(a_position.xy, 0.0, 1.0);
}
`;

// set up some global variables
let program;
// for ensuring only 1 shader runs at a time
// acts as a shader id
window.count = 0;
let timeLocation;
let resolutionLocation;
let mouseX = 0;
let mouseY = 0;
let centerx = 0;
let centery = 0;
let zoom = 1.0;
let canvas = document.createElement("canvas");

// Variables for magnet dragging
let magnetPositions = [];
let selectedMagnetIndex = -1;
let isDraggingMagnet = false;
let magnetRadius = 0.05; // Radius for magnet hitbox in coordinate space

// utility functions
function toCoord(x, y) {
  let width = canvas.width;
  let height = canvas.height;
  let size = Math.min(width, height);
  return [
    (3.0 / zoom) * (x - width / 2) / size + centerx,
    (3.0 / zoom) * (y - height / 2) / size + centery
  ];
}

function screenToCoord(x, y) {
  let rect = canvas.getBoundingClientRect();
  let canvasX = x - rect.left;
  let canvasY = canvas.height - (y - rect.top); // Flip Y coordinate
  return toCoord(canvasX, canvasY);
}

function deltaToCoord(dx, dy) {
  let width = canvas.width;
  let height = canvas.height;
  let size = Math.min(width, height);
  return [
    (3.0 / zoom) * (dx) / size,
    (3.0 / zoom) * (dy) / size
  ];
}

function coordToScreen(coordX, coordY) {
  let width = canvas.width;
  let height = canvas.height;
  let size = Math.min(width, height);
  // Convert from coordinate space to screen space, with Y flipped to match WebGL convention
  return [
    ((coordX - centerx) * zoom / 3.0) * size + width / 2,
    ((coordY - centery) * -zoom / 3.0) * size + height / 2 // Negate zoom factor for Y to flip direction
  ];
}

function lerp(a, b, r) {
  return a + r * (b - a);
}

// Initialize magnet positions
function initMagnetPositions(numMagnets) {
  magnetPositions = [];
  for (let i = 0; i < numMagnets; i++) {
    const angle = (i / numMagnets) * 2 * Math.PI;
    const r = 1.0; // Initial radius
    magnetPositions.push([r * Math.cos(angle), r * Math.sin(angle)]);
  }
}

// Check if a point is inside a magnet
function isPointInMagnet(point, magnetPos) {
  const dx = point[0] - magnetPos[0];
  const dy = point[1] - magnetPos[1];
  const distSquared = dx * dx + dy * dy;
  return distSquared <= magnetRadius * magnetRadius;
}

// Find which magnet was clicked (if any)
function findClickedMagnet(clickPos) {
  for (let i = 0; i < magnetPositions.length; i++) {
    if (isPointInMagnet(clickPos, magnetPositions[i])) {
      return i;
    }
  }
  return -1;
}

// uniform variables to pass to the shader
let shaderData = {
  'u_mouse': (gl, loc) => gl.uniform2fv(loc, [mouseX, mouseY]),
  'center': (gl, loc) => gl.uniform2fv(loc, [centerx, centery]), // the center of the display area as a complex number
  'zoom': (gl, loc) => gl.uniform1f(loc, zoom),
  'maxIter': (gl, loc) => gl.uniform1i(loc, document.getElementById("detail").value),
  'kf': (gl, loc) => gl.uniform1f(loc, document.getElementById("kf").value),
  'kp': (gl, loc) => gl.uniform1f(loc, document.getElementById("kp").value),
  'km': (gl, loc) => gl.uniform1f(loc, document.getElementById("km").value),
  'trap_radius': (gl, loc) => gl.uniform1f(loc, document.getElementById("trap_radius").value),
  'trap_velocity': (gl, loc) => gl.uniform1f(loc, document.getElementById("trap_velocity").value),
  'max_magnetism': (gl, loc) => gl.uniform1f(loc, document.getElementById("max_magnetism").value),
  'shading_strength': (gl, loc) => gl.uniform1f(loc, document.getElementById("shading_strength").value),
  'magnet_positions': (gl, loc) => {
    // Flatten the array of positions
    const flatPositions = magnetPositions.flat();
    gl.uniform2fv(loc, flatPositions);
  }
};

// canvas event listeners for interactivity
let dragging = false;
let dragStart;

canvas.addEventListener('mousedown', (e) => {
  const rect = canvas.getBoundingClientRect();
  const x = e.clientX - rect.left;
  const y = canvas.height - (e.clientY - rect.top);
  const clickCoord = toCoord(x, y);
  
  // Check if we clicked on a magnet
  const magnetIndex = findClickedMagnet(clickCoord);
  
  if (magnetIndex !== -1) {
    // We clicked on a magnet
    selectedMagnetIndex = magnetIndex;
    isDraggingMagnet = true;
  } else if (e.button === 0) { // Left mouse button for panning
    dragging = true;
    dragStart = [x, y];
  }
});

canvas.addEventListener('mouseup', (e) => {
  if (isDraggingMagnet) {
    isDraggingMagnet = false;
    selectedMagnetIndex = -1;
  }
  if (e.button === 0) {
    dragging = false;
  }
});

canvas.addEventListener('mousemove', (e) => {
  const rect = canvas.getBoundingClientRect();
  const x = e.clientX - rect.left;
  const y = canvas.height - (e.clientY - rect.top);
  
  if (isDraggingMagnet && selectedMagnetIndex !== -1) {
    // Update the position of the selected magnet
    magnetPositions[selectedMagnetIndex] = toCoord(x, y);
  } else if (dragging) {
    const curr = [x, y];
    const dx = curr[0] - dragStart[0];
    const dy = curr[1] - dragStart[1];
    const dcenter = deltaToCoord(dx, dy);
    centerx -= dcenter[0];
    centery -= dcenter[1];
    dragStart = curr;
  }
});

canvas.addEventListener('wheel', (e) => {
  e.preventDefault();
  const rect = canvas.getBoundingClientRect();
  const x = e.clientX - rect.left;
  const y = canvas.height - (e.clientY - rect.top);
  const z = toCoord(x, y);
  const coordX = z[0];
  const coordY = z[1];
  
  let sign;
  if (e.deltaY > 0) {
    sign = -1;
  } else {
    sign = 1;
  }
  const strength = 0.1 * sign;
  centerx = lerp(centerx, coordX, strength);
  centery = lerp(centery, coordY, strength);
  zoom /= 1 - strength;
});

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

// Draw white dots for the magnets
function drawMagnetDots(gl) {
  // Save WebGL state
  gl.enable(gl.BLEND);
  gl.blendFunc(gl.SRC_ALPHA, gl.ONE_MINUS_SRC_ALPHA);
  
  // Create a simple program for drawing points
  const vertexShader = createShader(gl, gl.VERTEX_SHADER, `
    attribute vec2 a_position;
    uniform vec2 u_resolution;
    
    void main() {
      // Convert from world space to clip space
      vec2 zeroToOne = a_position / u_resolution;
      vec2 zeroToTwo = zeroToOne * 2.0;
      vec2 clipSpace = zeroToTwo - 1.0;
      // Flip Y in clip space
      clipSpace.y = -clipSpace.y;
      gl_Position = vec4(clipSpace, 0, 1);
      gl_PointSize = 10.0;
    }
  `);
  
  const fragmentShader = createShader(gl, gl.FRAGMENT_SHADER, `
    precision mediump float;
    
    void main() {
      float dist = length(gl_PointCoord - vec2(0.5));
      if (dist > 0.5) discard;
      gl_FragColor = vec4(1.0, 1.0, 1.0, 1.0);
    }
  `);
  
  const dotProgram = createProgram(gl, vertexShader, fragmentShader);
  gl.useProgram(dotProgram);
  
  // Set up attributes and uniforms
  const positionLocation = gl.getAttribLocation(dotProgram, "a_position");
  const resolutionLocation = gl.getUniformLocation(dotProgram, "u_resolution");
  
  gl.uniform2f(resolutionLocation, gl.canvas.width, gl.canvas.height);
  
  // Create and fill position buffer with screen coordinates
  const positionBuffer = gl.createBuffer();
  gl.bindBuffer(gl.ARRAY_BUFFER, positionBuffer);
  
  // Convert magnet positions from coordinate space to screen space
  const screenPositions = [];
  for (let i = 0; i < magnetPositions.length; i++) {
    const pos = magnetPositions[i];
    const screenPos = coordToScreen(pos[0], pos[1]);
    screenPositions.push(screenPos[0], screenPos[1]);
  }
  
  gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(screenPositions), gl.STATIC_DRAW);
  
  // Set up the position attribute
  gl.enableVertexAttribArray(positionLocation);
  gl.vertexAttribPointer(positionLocation, 2, gl.FLOAT, false, 0, 0);
  
  // Draw the points
  gl.drawArrays(gl.POINTS, 0, magnetPositions.length);
  
  // Restore WebGL state
  gl.disable(gl.BLEND);
}

function main() {
  // increment count for this shader's id
  count++;
  
  // Initialize magnet positions
  const numMagnets = parseInt(document.getElementById("num_mag").value);
  initMagnetPositions(numMagnets);
  
  // load the shader, pass it the variables, and display it on the canvas
  loadTextFile("shader.frag", function (text) {
    var frag = text;
    var re = /# define NUM_MAGNETS \d*/gi;
    frag = frag.replace(re, "#define NUM_MAGNETS " + numMagnets);
    
    canvas.width = window.innerWidth;
    canvas.height = window.innerHeight;
    document.body.appendChild(canvas);
    
    var gl = canvas.getContext('webgl');
    var vertexShader = createShader(gl, gl.VERTEX_SHADER, vert);
    var fragmentShader = createShader(gl, gl.FRAGMENT_SHADER, frag);
    program = createProgram(gl, vertexShader, fragmentShader);
    
    // RENDER USES THE OLD LOCATION!!!! HAVE TO MAKE IT GLOBAL
    timeLocation = gl.getUniformLocation(program, "u_time");
    resolutionLocation = gl.getUniformLocation(program, "u_resolution");
    
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

    // give this shader an id
    let localCount = count;
    function render(time) {
      // if this is the current shader
      if (localCount === window.count) {
        resizeCanvas(gl);
        gl.viewport(0, 0, gl.canvas.width, gl.canvas.height);
        gl.clearColor(0, 0, 0, 0);
        gl.clear(gl.COLOR_BUFFER_BIT);
        
        // Render the fractal
        gl.useProgram(program);
        gl.enableVertexAttribArray(positionAttributeLocation);
        gl.bindBuffer(gl.ARRAY_BUFFER, positionBuffer);
        
        // Tell the attribute how to get data out of positionBuffer (ARRAY_BUFFER)
        var size = 2;          // 2 components per iteration
        var type = gl.FLOAT;   // the data is 32bit floats
        var normalize = false; // don't normalize the data
        var stride = 0;        // 0 = move forward size * sizeof(type) each iteration to get the next position
        var offset = 0;        // start at the beginning of the buffer
        gl.vertexAttribPointer(
          positionAttributeLocation, size, type, normalize, stride, offset);

        gl.uniform1f(timeLocation, time * 0.001);
        gl.uniform2fv(resolutionLocation, [gl.canvas.width, gl.canvas.height]);
        
        for (const name in shaderData) {
          const location = gl.getUniformLocation(program, name);
          shaderData[name](gl, location);
        }
        
        var primitiveType = gl.TRIANGLES;
        var offset = 0;
        var count = 6;
        gl.drawArrays(primitiveType, offset, count);
        
        // Draw the magnet dots on top
        drawMagnetDots(gl);

        requestAnimationFrame(render);
      }
    }
    requestAnimationFrame(render);
  });
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
  return s;
}

// Initialize the application
window.onload = main; 