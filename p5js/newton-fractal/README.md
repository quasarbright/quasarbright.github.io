# Newton Fractal Visualization

This is a WebGL-based visualization of Newton's fractal for the polynomial (z-1)(z+1)(z-i)(z+i) in the complex plane.

## Features

- Fullscreen WebGL rendering
- Interactive panning and zooming
- Mobile touch support (pinch to zoom)
- Visualization of polynomial roots
- Adjustable parameters (iterations, convergence threshold)
- Colorful visualization based on convergence patterns

## How It Works

Newton's method is an iterative numerical method for finding the roots of a function. When applied to complex functions and visualized in the complex plane, it creates beautiful fractal patterns.

For a complex polynomial f(z), Newton's method iterates:

```
z_{n+1} = z_n - f(z_n) / f'(z_n)
```

This implementation uses the polynomial f(z) = (z-1)(z+1)(z-i)(z+i), which has four roots:
1. z = 1
2. z = -1
3. z = i
4. z = -i

The color of each pixel represents the convergence behavior of the corresponding complex number, with hue based on the final angle and brightness indicating how quickly it converges.

The roots of the polynomial are visualized as colored dots with white outlines. The color of each root corresponds to its angle in the complex plane, matching the coloring scheme used for the fractal itself.

## Usage

- **Pan**: Click and drag to move around the fractal
- **Zoom**: Use mouse wheel or pinch gestures on touch devices
- **Adjust Parameters**: Use the sliders to change iterations and convergence threshold
- **Toggle Root Visualization**: Use the checkbox to show/hide the roots
- **Reset View**: Click the "Reset View" button
- **Hide/Show Controls**: Use the buttons to toggle control panel visibility

## Implementation Details

The visualization is implemented using:
- WebGL for GPU-accelerated rendering
- GLSL fragment shader for the Newton's method calculation
- Factored form representation of the polynomial for clearer root visualization
- JavaScript for WebGL setup and user interaction handling 