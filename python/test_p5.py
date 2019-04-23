from p5 import *
import random

def setup():
    size(500,500)
    color_mode('HSB')

frameCount = 0
def draw():
    global frameCount
    background(frameCount % 255, 255, 255)
    x = random.randint(0,width)
    y = random.randint(0,height)
    point = (x, y)
    fill(random.randint(0,255), 255, 255)
    ellipse(point, 100,100)
    frameCount+=1

run()
