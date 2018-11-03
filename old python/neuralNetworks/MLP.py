import math, random, sys
sys.path.append('C:/Users/mdelm\OneDrive/Documents/atom/python')
from linalg import *
import matplotlib.pyplot as plt
phist = []
avgs = []
def sigmoid(x):
    if type(x) == Vector:
        return Vector([sigmoid(i) for i in x])
    else:
        return 1/(1+math.exp(-x))
def avg(l):
    ans = 0
    for x in l:
        ans += x
    return ans/len(l)
class MLP:#one hidden layer only
    def __init__(self,ins,hiddens,outs):
        self.ins = ins
        self.hiddens = hiddens
        self.outs = outs
        self.lr = -.01#learning rate

        weights = [Matrix.fromShape(ins+1,hiddens), Matrix.fromShape(hiddens+1,outs)]#list of Matrices
        for i in range(0,len(weights)):
            for r in range(0,len(weights[i])):
                for c in range(0,len(weights[i][r])):
                    weights[i][r][c] = random.uniform(-.5,.5)
        self.weightsT = [M.transpose() for M in weights]
    def guess(self,x):
        if len(x) != self.ins:
            raise Exception('invalid input dimensions')
        x.insert(0,1)
        x = Vector(x)
        z = sigmoid(self.weightsT[0] * x)
        z.insert(0,1)
        o = sigmoid(self.weightsT[1] * z)
        return o
    def train(self,x,actual):
        actual = Vector(actual)
        if len(x) != self.ins:
            raise Exception('invalid input dimensions')
        x.insert(0,1)
        x = Vector(x)
        z = sigmoid(self.weightsT[0] * x)
        z.insert(0,1)
        o = sigmoid(self.weightsT[1] * z)
        performance = Vector([ (o[k]-actual[k])**2 * (-1.0/2) for k in range(0,len(o)) ])
        phist.append(performance[0])
        avgs.append(avg(phist))

        deltaWT0 = Matrix.fromShape(self.hiddens,self.ins+1)
        for k in range(0,self.outs):
            dw = Matrix([[ (o[k]-actual[k])*o[k]*(1-o[k])*self.weightsT[1][k][j]*z[j+1]*(1-z[j+1])*x[i] for i in range(0,self.ins+1)] for j in range(0,self.hiddens) ])
            # print dw
            # print deltaWT0
            # print dw.rows,dw.cols,deltaWT0.rows,deltaWT0.cols
            # print deltaWT0 + dw, 'l'
            deltaWT0 = deltaWT0 + dw
        self.weightsT[0] = self.weightsT[0] + deltaWT0*self.lr
        deltaWT1 = Matrix([[ (o[k]-actual[k])*o[k]*(1-o[k])*z[j] for j in range(0,self.hiddens+1) ] for k in range(0,self.outs)])
        self.weightsT[1] = self.weightsT[1] + deltaWT1*self.lr
N = MLP(2,2,2)
def f(x):
    if (x[0] + x[1]) % 2 == 0:
        return Vector([1,0])
    return Vector([0,1])
for i in range(0,10000):
    x = [random.randint(0,1) for j in range(0,2)]
    N.train(x,f(x))
for i in range(0,4):
    x = [random.randint(0,1) for j in range(0,2)]
    print x[0],x[1],repr(N.guess(x)),repr(f(x))

plt.plot(phist)
plt.plot(avgs)
plt.show()
