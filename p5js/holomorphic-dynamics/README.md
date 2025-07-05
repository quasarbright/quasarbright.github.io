# Complex Dynamics

A WebGL-based visualization of complex dynamics using vanilla JavaScript. Allows you to directly enter GLSL code to visualize arbitrary complex functions and their iterative behavior in the complex plane.

## Features

- Full-screen canvas visualization
- Interactive panning and zooming
- Coloring based on iteration count
- Adjustable iteration count
- **Direct GLSL code input** - Enter your own complex functions as GLSL code
- Runtime shader generation with no page reload required

## How It Works

This visualization uses WebGL shaders to compute complex function iterations directly on the GPU. The key innovation is that you can enter your own GLSL code directly in the interface, and the application will:

1. Validate your GLSL code
2. Generate a new shader with your function
3. Compile and run it in real-time

This allows for extremely flexible and powerful function definitions without being limited to predefined patterns.

## Available Complex Functions

The following complex number operations are available in the shader:

- `complex_square(z)` - z²
- `complex_cube(z)` - z³
- `complex_pow(z, n)` - z^n (for integer n)
- `complex_mul(a, b)` - Complex multiplication
- `complex_div(a, b)` - Complex division
- `complex_sin(z)`, `complex_cos(z)` - Complex trigonometric functions
- `complex_exp(z)`, `complex_log(z)` - Complex exponential and logarithm

## How to Run

Due to browser security restrictions, you'll need to serve the files through a local web server. Here are a few options:

### Using Python

If you have Python installed:

```bash
# Python 3
python -m http.server

# Python 2
python -m SimpleHTTPServer
```

Then open your browser and navigate to `http://localhost:8000`

### Using Node.js

If you have Node.js installed, you can use:

```bash
# Install http-server globally if you haven't already
npm install -g http-server

# Run the server
http-server
```

Then open your browser and navigate to the URL shown in the terminal.

## Controls

- **Mouse drag**: Pan the view
- **Mouse wheel**: Zoom in/out
- **+/-**: Increase/decrease maximum iterations
- **R**: Reset view
- **Function input**: Enter GLSL code and click "Apply Function"

## Example Functions

Here are some example functions you can try:

```glsl
// Mandelbrot set
return complex_square(z) + c;

// Cubic Mandelbrot
return complex_cube(z) + c;

// Burning Ship fractal
vec2 absZ = vec2(abs(z.x), abs(z.y));
return complex_square(absZ) + c;

// Complex sine
return complex_sin(z) + c;

// Complex tangent
return complex_div(complex_sin(z), complex_cos(z)) + c;
```

## Technical Details

- Uses WebGL for GPU-accelerated rendering
- Implements complex function iteration in GLSL
- Runtime shader generation and compilation
- Supports arbitrary complex functions 