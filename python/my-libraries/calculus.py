import math
import numpy as np
def derivative(f):
    '''returns a function that is the derivative of f with respect to x'''
    def dfdx(x):
        epsilon = 1e-3
        df = f(x+epsilon) - f(x-epsilon)
        dx = 2*epsilon
        return df/dx
    return dfdx
def integrate(f,a,b):
    '''returns definite integral of f(x) with respect to x from a to b
    uses average of left, right, and trapezoidal riemann sum.
    might not work with vertical asymptotes'''
    epsilon = 1e-3
    samples = ( max(a,b)-min(a,b) ) / epsilon
    X = np.linspace(min(a,b),max(a,b),samples)
    left = 0
    for x in X[:-1]:
        left += f(x) * epsilon
    right = 0
    for x in X[1:]:
        right += f(x) * epsilon
    trap = 0
    for i in range(len(X)-1):
        trap += .5 * ( f(X[i]) + f(X[i+1]) ) * epsilon
    ans = (left+right+trap) / 3
    if b > a:
        return ans
    return -1 * ans
def firstOrderODE(dydx,xi,yi,xf):
    '''expects dydx(x,y) and (xi,yi) which is the initial condition
    example: firstOrderODE(f(x,y),2,1)
    uses euler approximation'''
    ic = (xi,yi)
    dx = .001
    X = []
    Y = []
    x = ic[0]
    y = ic[1]
    while x < xf and y < 1e99:
        dy = dydx(x,y)*dx
        x += dx
        y += dy
    return y
if __name__ == '__main__':
    #for testing
    def f(x,y):
        return x+y
    print(firstOrderODE(f,2,1,6))
