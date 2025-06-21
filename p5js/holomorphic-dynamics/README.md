# Complex Function Visualization

A WebGL-based visualization of complex functions using vanilla JavaScript. Currently set up to visualize the Mandelbrot set, but designed to be easily extended to other complex functions.

## Features

- Full-screen canvas visualization
- Interactive panning and zooming
- Coloring based on iteration count
- Adjustable iteration count
- No escape magnitude check, making it suitable for arbitrary complex functions

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

## Technical Details

- Uses WebGL for GPU-accelerated rendering
- Implements complex function iteration in GLSL
- Coloring is based on iteration count
- No escape magnitude check is used, making it suitable for arbitrary complex functions

## Extending with Other Complex Functions

The visualization is designed to be easily extended to other complex functions. To change the function:

1. Edit the `complex_function` function in `fragment.glsl`
2. Uncomment or add your desired function implementation
3. Reload the page to see the new visualization

Examples of other functions you can try:
- Burning Ship fractal
- Cubic Mandelbrot
- Julia sets
- Tricorn (Mandelbar)
- Newton fractals

See `complexFunctions.js` for JavaScript implementations of these functions that can be adapted for GLSL. 