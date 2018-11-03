import math, random, sys
sys.path.append('C:/Users/mdelm\OneDrive/Documents/atom/python')
from linalg import *
import matplotlib.pyplot as plt
phist = []
def sigmoid(x):
    if type(x) == Vector:
        return Vector([sigmoid(i) for i in x])
    else:
        if x >=0:
            return 1
        return 0
class MLP:#one hidden layer only
    def __init__(self,ins,hiddens,outs):
        self.ins = ins
        self.hiddens = hiddens
        self.outs = outs
        self.lr = -.1#learning rate

        weights = [Matrix.fromShape(ins,hiddens), Matrix.fromShape(hiddens,outs)]#list of Matrices
        for i in range(0,len(weights)):
            for r in range(0,len(weights[i])):
                for c in range(0,len(weights[i][r])):
                    weights[i][r][c] = random.uniform(-.5,.5)
        self.weightsT = [M.transpose() for M in weights]

        self.biases = [Vector([random.uniform(-.5,.5) for x in range(0,self.hiddens)])]
        self.biases.append(Vector( [random.uniform(-.5,.5) for x in range(0,self.outs)] ))

    def guess(self,x):
        if len(x) != self.ins:
            raise Exception('invalid input dimensions')
        x = Vector(x)
        z = sigmoid(self.weightsT[0] * x + self.biases[0])
        o = sigmoid(self.weightsT[1] * z + self.biases[1])
        return o
    def train(self,x,actual):
        actual = Vector(actual)
        if len(x) != self.ins:
            raise Exception('invalid input dimensions')
        x = Vector(x)
        z = sigmoid(self.weightsT[0] * x + self.biases[0])
        o = sigmoid(self.weightsT[1] * z + self.biases[1])
        performance = Vector([ (actual[k]-o[k])**2 * (-1.0/2) for k in range(0,len(o)) ])
        phist.append(performance[0])
        # phist.append(self.weightsT[1][0][1])
        
        deltaWT1 = Matrix([[ (o[k]-actual[k])*z[j] for j in range(0,self.hiddens) ] for k in range(0,self.outs)])
        self.weightsT[1] = self.weightsT[1] + deltaWT1*self.lr

        deltaWT0 = Matrix.fromShape(self.hiddens,self.ins)
        for k in range(0,self.outs):
            # dw = Matrix([[ (o[k]-actual[k])*o[k]*(1-o[k])*self.weightsT[1][k][j]*z[j]*(1-z[j])*x[i] for i in range(0,self.ins)] for j in range(0,self.hiddens) ])
            dw = Matrix([[ (o[k]-actual[k])*self.weightsT[1][k][j]*x[i] for i in range(0,self.ins)] for j in range(0,self.hiddens) ])
            # print dw
            # print deltaWT0
            # print dw.rows,dw.cols,deltaWT0.rows,deltaWT0.cols
            # print deltaWT0 + dw, 'l'
            deltaWT0 = deltaWT0 + dw
        self.weightsT[0] = self.weightsT[0] + deltaWT0*self.lr
        # deltaWT1 = Matrix([[ (o[k]-actual[k])*o[k]*(1-o[k])*z[j] for j in range(0,self.hiddens) ] for k in range(0,self.outs)])


        deltaB0 = Vector([0 for x in range(0,self.hiddens)])
        for k in range(0,self.outs):
            # db = Vector([(o[k]-actual[k])*o[k]*(1-o[k])*self.weightsT[1][k][j]*z[j]*(1-z[j]) for j in range(0,self.hiddens)])
            db = Vector([(o[k]-actual[k])*self.weightsT[1][k][j] for j in range(0,self.hiddens)])
            deltaB0 = deltaB0 + db
        self.biases[0] = self.biases[0] + deltaB0*self.lr
        # deltaB1 = Vector([(o[k]-actual[k])*o[k]*(1-o[k]) for k in range(0,self.outs)])
        deltaB1 = Vector([(o[k]-actual[k]) for k in range(0,self.outs)])
        self.biases[1] = self.biases[1] + deltaB1*self.lr
XOR = MLP(2,2,1)
def xor(x):
    return Vector([(x[0]+x[1]) % 2])
for i in range(0,10):
    XOR.train([0,0],[0])
    XOR.train([1,0],[1])
    XOR.train([0,1],[1])
    XOR.train([1,1],[0])

print 0,0,XOR.guess([0,0])
print 0,1,XOR.guess([0,1])
print 1,0,XOR.guess([1,0])
print 1,1,XOR.guess([1,1])
plt.plot(phist)
plt.show()
