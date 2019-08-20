# [Processing](https://processing.org/)
Here are some interesting projects I've done using Processing for Java. Since Processing sketches can't be directly displayed in browser like p5.js sketches, I have included screenshots and YouTube videos of my sketches running.
## Complex Color Plotter
<iframe width="560" height="315" src="https://www.youtube.com/embed/AVB6F626P1Q" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>  

I plotted a complex function as a color map. Here, I plotted a function with two zeroes and spirals. The asymptotes' locations and spiral strengths are parameterized by perlin noise over time, making it animated.  

Here's how the color ploting works: The function is a function from one complex number to another. Since complex numbers are two-dimensional, graphing them normally would require 4 dimensions, but we can only perceive 3 dimensions. To reduce the dimensionality, I used color.  

The neutral, base graph starts out as a rainbow color wheel centered at the origin. It is black in the center and gets brighter as the distance from the origin increases. The complex numbers are plotted where they belong in the complex plane, and are colored accordingly. Now, each complex number has a color and location associated with it.  

To graph a function, we go to each point in the complex plane, call that `z`, calculate `f(z)`, calculate the color of `f(z)` according to the base color map, and color in that point in the complex plane with that color. For example, if `f(1+2i) = 0`, the point `1+2i` in the complex plane will be black.  

## Perlin Cube Terrain
<iframe width="560" height="315" src="https://www.youtube.com/embed/_ozs9ZBzMyw" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>  

I created a scalar field in 3D space parameterized by perlin noise, and translated it over time. I thresholded the scalar field such that values above a certain number were filled in as a green cube, and those below it were left empty.  

## Crystallization
<iframe width="560" height="315" src="https://www.youtube.com/embed/FRw5XWjBsms" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>  

There is a frozen particle at the center of the screen. Each particle randomly drifts around until it hits a frozen particle. At that point, it gets frozen. As this process continues, an interesting pattern forms.  
