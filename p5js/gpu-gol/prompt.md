I want to make a webpage like this https://quasarbright.github.io/p5js/image-conway/
Here is what it does:
1. User uploads an image
2. Image gets converted to black and white via dithering
3. Black and white image is fed into conway's game of life
4. User sees the conway's game of life play out for their image

I want a webpage that is basically the same thing, except I want conway's game of life to run in a webgl shader, powered by the GPU so it runs faster and can run for large images in real time.

This should be a static, standalone webpage just using raw html and js, no react or nextjs or anthing, keep it simple. it would be ok to use something like threejs though. Whatever would run the smoothest.