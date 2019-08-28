# [Processing](https://processing.org/)
Here are some interesting projects I've done using Processing for Java. Since Processing sketches can't be directly displayed in browser like p5.js sketches, I have included screenshots and YouTube videos of my sketches running.

## Magnetic Pendulum
![](https://quasarbright.github.io/images/magnet%20pendulum.PNG)  

There are 3 magnets arranged in a triangle and another magnet on a pendulum. The magnet on the pendulum is attracted to the other 3 magnets, and the pendulum pulls it towards the center. The picture is colored based on which magnet the pendulum ends on when released from that location.

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

## 3D Crystallization
<iframe width="560" height="315" src="https://www.youtube.com/embed/MH7q543KP0w" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>  

This is the same as the 2D crystallization, but in 3D with spheres. The pattern formed is less interesting.

## Perlin Flow Field
<iframe width="560" height="315" src="https://www.youtube.com/embed/MUfS1xu17YU" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>  

I created a field of force vectors with perlin noise, and dropped particles in the force field, tracing their paths.

## 2D Gravity
<iframe width="560" height="315" src="https://www.youtube.com/embed/Jaf1B0yYMtU" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>  

When the mouse is pressed, the particles are attracted to the mouse cursor. I also added a few features like being able to freeze the particles on the blue circle that follows the circle and making the gravitational force stronger.

## Mandelbrot Set Zoom
<iframe width="560" height="315" src="https://www.youtube.com/embed/3SGk1oI-7N8" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>

I colored the mandelbrot set by the number of steps required to escape the convergence zone, and allowed the user to pan and zoom. You can't zoom in arbitrarily far due to the limited precision of floating point numbers.
## 2D Random Walker
<iframe width="560" height="315" src="https://www.youtube.com/embed/IPxwzn4a5uQ" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>  

I create several [random walkers](https://www.wikiwand.com/en/Random_walk) and trace their paths. I made it so the color of each new walker slowly progresses through the color wheel. Once they go out of bounds, they despawn and a new walker spawns.

## 3D Random Walker
<iframe width="560" height="315" src="https://www.youtube.com/embed/Ldaid-rPy-g" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>

3D version


## 2D Ray Casting
<iframe width="560" height="315" src="https://www.youtube.com/embed/rAVZWu2lDwQ" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>  

Rays are cast from a point and surfaces are shaded based on the distance from the source. I made a 2D environment with some shapes and walls, extended it vertically, and made a user-controlled, first person camera. There is also a top-down view.

## Square Marching Terrain
<iframe width="560" height="315" src="https://www.youtube.com/embed/xzkkaIIIuj0" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>  

I made a 2D perlin noise scalar function that changes over time, thresholded it, and used a square marching algorithm to make shapes out of it. I thought it kind of looked like terrain, so I colored it like dirt and water.

## 2D Vector Flow Field
<iframe width="560" height="315" src="https://www.youtube.com/embed/MwrhJQ6_KUk" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>  

I made an underlying vector force field and set each particle's velocity to the vector under it.
