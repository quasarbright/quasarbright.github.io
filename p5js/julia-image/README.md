# Julia Set Image Transformer

A WebGL-based application that transforms uploaded images using Julia set mathematics. It applies the iterative function f(z) = z² + c to each pixel and maps the result back to the original image.

## Features

- Upload any image and transform it using Julia set mathematics
- Interactive control of the Julia set parameter by clicking or dragging on the canvas
- Adjustable iteration count and tile size
- Fast performance using GPU-accelerated WebGL fragment shaders
- Save the transformed image to your device

## How It Works

The application uses WebGL fragment shaders to perform Julia set calculations on the GPU, which is much faster than CPU-based implementations. Each pixel on the canvas is processed in parallel:

1. The pixel position is mapped to a complex number z
2. The function f(z) = z² + c is applied iteratively
3. If the point doesn't diverge (escape to infinity), its final position is used to sample a color from the original image
4. If the point diverges, it's colored using a darkened version of the original image at that position

The image is tiled across the complex plane in a grid pattern, with each tile divided into four quadrants using different mapping strategies to ensure more variety in the colors used.

## Technologies Used

- HTML5
- CSS3
- JavaScript
- WebGL for GPU-accelerated rendering

## Usage

Simply open `index.html` in a web browser to use the application, or run a local server:

```
python3 -m http.server 8000
```

Then navigate to `http://localhost:8000` in your browser.

### Controls

- **Click/Drag**: Set the Julia set parameter c
- **Iterations Slider**: Adjusts the number of iterations for the Julia set calculation
- **Tile Size Slider**: Adjusts how the image is tiled in the complex plane
- **Reset Button**: Returns all parameters to default values
- **Save Button**: Downloads the current transformed image

### Keyboard Shortcuts

- **S**: Save the current image
- **+/-**: Increase/decrease iterations
- **,/.**: Decrease/increase tile size

## Browser Compatibility

This application requires WebGL support, which is available in most modern browsers. If you encounter issues, please make sure your browser supports WebGL and that it's enabled. 