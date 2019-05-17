# from mpmath import *
import numpy as np
def steps_to_escape(z, max_steps):
    c = z
    zn = c
    for i in range(max_steps):
        if np.abs(zn) > 2:
            return i
        zn = zn**2 + c
    return max_steps


def steps_to_hu(steps, max_steps):
    return 255 * steps / max_steps