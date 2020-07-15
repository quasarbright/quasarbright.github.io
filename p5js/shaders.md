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

Zoom with the scroll wheel and pan the camera by clicking and dragging

## [Julia Set](https://quasarbright.github.io/p5js/juliaShader/)
![julia set](https://quasarbright.github.io/p5js/juliaShader/screenshot.png)
This shader uses the same coloring method as the Mandelbrot set shader, but you can select the c-value used to
generate the julia set by clicking/dragging the mouse on the screen. You can also zoom in and out with the mouse wheel.
If you want to pan the camera, you can do so by holding the mouse wheel down and moving the mouse.
