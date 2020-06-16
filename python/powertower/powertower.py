'''a power tower is x^(x^(x^(...)))
for some values of x, this thing actually converges!
particularly when x in [e^-e, e^(1/e)]
repeated exponentiation is called tetration, and this is infinite tetration

Inspired by 3blue1brown's video https://www.youtube.com/watch?v=elQVZLLiod4
'''

import math
import numpy as np

def arrow(x, n):
    ans = (1.0)
    for _ in range(n):
        ans = x ** ans
    return ans

def tower(x):
    return arrow(x, int(1e6))

def inv(f, y, xmin, xmax, epsilon=1e-3):
    '''approximates the inverse of a function (assumes df/dx > 0)
    '''
    xmin = np.float128(xmin)
    xmax = np.float128(xmax)
    while True:
        x = (xmin + xmax) / 2.0
        y_ = float('inf')
        try:
            y_ = f(x)
        except:
            pass
        if abs(y - y_) < epsilon:
            return x
        elif y_ > y:
            xmax = x
        elif y_ < y:
            xmin = x
        print(x, y_)


# print(inv(tower, 4, 1.444667861, 1.5))

import matplotlib.pyplot as plt
import numpy as np

X = np.linspace(0, 1.4446678610202413, 100)
Y = np.vectorize(tower)(X)

plt.scatter(X, Y)
plt.show()
