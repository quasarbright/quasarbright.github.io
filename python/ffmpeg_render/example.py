import numpy as np
from arr_to_mp4 import arr_to_mp4
import colorsys

def calc_hu(x, y, t):
    return (x**2 / 256**2 + y / 256 + t/100) % 1

arr = np.zeros((120, 256, 256, 3))
d, w, h, _ = arr.shape
for t in range(d):
    for x in range(w):
        for y in range(h):
            hu = calc_hu(x, y, t)
            c = (hu, 1, 1)
            c = colorsys.hsv_to_rgb(*c)
            for channel in range(3):
                arr[t,x,y,channel] = c[channel] * 255
arr_to_mp4(arr, 'out1.mp4')