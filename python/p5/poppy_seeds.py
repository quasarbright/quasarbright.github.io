from p5 import *
import numpy as np
xres = 10
dx = 0
dy = 0
noiseScale = .01
yres = 10

def setup():
    global dx, dy, xres, yres
    size(200,200)
    dx = width / xres
    dy = height / yres
    color_mode('HSB')

frameCount = 0
def draw():
    global noiseScale, frameCount
    background(0)
    for x in np.linspace(0,width,xres):
        for y in np.linspace(0,height,yres):
            hu = 256*noise(x*noiseScale, y*noiseScale, noiseScale*frameCount)
            hu += frameCount*5
            hu = hu % 256
            fill(hu,255,255)
            rect((x, y), dx, dy)
    frameCount += 1

run()