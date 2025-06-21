# Mandelbrot Set Visualization

A WebGL-based visualization of the Mandelbrot set using vanilla JavaScript.

## Features

- Full-screen canvas visualization
- Interactive panning and zooming
- Coloring based on escape iterations
- Adjustable iteration count

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
- Implements the Mandelbrot set algorithm in GLSL
- Coloring is based on escape iterations
- No radius-2 escape optimization is used as requested 