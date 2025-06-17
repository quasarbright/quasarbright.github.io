# Newton Fractal Visualization

This is a WebGL-based visualization of Newton's fractal for various polynomials in the complex plane.

## Features

- Fullscreen WebGL rendering
- Interactive panning and zooming
- Mobile touch support (pinch to zoom)
- Multiple polynomial functions to explore
- Adjustable parameters (iterations, convergence threshold)
- Colorful visualization based on convergence patterns

## Available Polynomials

- z³ - 1
- z⁴ - 1
- z⁵ - 1
- z³ - z
- z⁶ + z³ - 1
- z³ + 1

## How It Works

Newton's method is an iterative numerical method for finding the roots of a function. When applied to complex functions and visualized in the complex plane, it creates beautiful fractal patterns.

For a complex polynomial f(z), Newton's method iterates:

```
z_{n+1} = z_n - f(z_n) / f'(z_n)
```

The color of each pixel represents the convergence behavior of the corresponding complex number, with hue based on the final angle and brightness indicating how quickly it converges.

## Usage

- **Pan**: Click and drag to move around the fractal
- **Zoom**: Use mouse wheel or pinch gestures on touch devices
- **Change Polynomial**: Select from the dropdown menu
- **Adjust Parameters**: Use the sliders to change iterations and convergence threshold
- **Reset View**: Click the "Reset View" button
- **Hide/Show Controls**: Use the buttons to toggle control panel visibility

## Implementation Details

The visualization is implemented using:
- WebGL for GPU-accelerated rendering
- GLSL fragment shader for the Newton's method calculation
- JavaScript for WebGL setup and user interaction handling 