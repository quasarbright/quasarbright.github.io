---
title: Shaders
---
* TOC
{:toc}
These are Fragment shaders I've made using webgl. A fragment shader uses your GPU to do pick a color for every pixel on your screen.
Since you're GPU can run a shader for many pixels at the same time, it runs very fast despite the number of computations. This is perfect for when you
want to compute something for every point on the screen.

## [Mandelbrot Set](https://quasarbright.github.io/p5js/mandelbrotShaderRenormalized/)
![mandelbrot set](https://quasarbright.github.io/p5js/mandelbrotShaderRenormalized/screenshot.png)
Instead of coloring each pixel discretely based on the iteration count upon escape,
This shader uses a continuous coloring which takes into account the iteration count
as well as the modulus of the final value. Since it's colored continuously, it can have some very pretty colors.

Zoom by scrolling and pan the camera by clicking and dragging

## [Julia Set](https://quasarbright.github.io/p5js/juliaShader/)
![julia set](https://quasarbright.github.io/p5js/juliaShader/screenshot.png)
This shader uses the same coloring method as the Mandelbrot set shader, but you can select the c-value used to
generate the julia set by clicking/dragging the mouse on the screen. You can also zoom in and out with the mouse wheel.
If you want to pan the camera, you can do so by holding the mouse wheel down and moving the mouse.

## [Magnet Pendulum](https://quasarbright.github.io/p5js/magnetShader/)
![magnet shader](https://quasarbright.github.io/p5js/magnetShader/screenshot.png)
The fractal is generated from simulating a magnetic pendulum swinging over a table with three magnets on it. Each pixel is colored based
on which magnet it ends up at and darkened based on how long it took to stop at a magnet. The three magnets oscillate radially from the center to
create an interesting visual effect. You can adjust the number of magnets and the maximum number of iterations of the simulation (detail slider).

You can move around by clicking and dragging and zoom in and out by scrolling.
### GPU Warning
This one is really heavy on the GPU. If it runs too slowly, try decreasing the detail slider or making your browser-window smaller.
