import sys, random
import matplotlib.pyplot as plt
sys.path.append('C:/Users/mdelm\OneDrive/Documents/atom/python')
from linalg import Vector
def sign(num):
    if num>=0:
        return 1
    return 0
class Perceptron:
    def __init__(self,numInputs):
        self.lr = .1
        self.weights = Vector([random.random() for x in range(0,numInputs)])
        self.bias = random.random()
    def guess(self,inputs):
        return sign((self.weights | Vector(inputs)) + self.bias)
    def train(self,inputs,ans):
        inputs = Vector(inputs)
        error = (self.guess(inputs)-ans)**2
        self.weights = self.weights - inputs*(2*error*self.lr)
        self.bias = self.bias - 2*error*self.lr
def f(x,y):#or
    if x+y > 0:
        return 1
    return 0
def avg(l):
    total = 0
    for x in l:
        total += x
    return total*1.0/len(l)
p = Perceptron(2)
points = []
hist = []
for i in range(0,10000):
    x = random.randint(0,1)
    y = random.randint(0,1)
    s = f(x,y)
    p.train(Vector([x,y]),s)
    points.append([x,y,s])
    hist.append(p.guess([x,y])-s)
avgs = [avg(hist[:i]) for i in range(1,len(hist)+1)]
plt.plot(hist)
plt.show()
print 0,0,p.guess([0,0])
print 0,1,p.guess([0,1])
print 1,0,p.guess([1,0])
print 1,1,p.guess([1,1])
# print p.weights
# print p.bias
# print p.guess([1,4])
