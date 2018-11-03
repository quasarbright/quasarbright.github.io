#! /usr/bin/python
from linalg import Vector
import math, random
import matplotlib.pyplot as plt
moves = [[1,0],[0,1],[-1,0],[0,-1]]
class Walker:
    def __init__(self):
        self.pos = Vector([0,0])
        self.history = [[self.pos[0]],[self.pos[1]]]
    def step(self):
        index = random.randint(0,len(moves)-1)
        v = moves[index]
        self.pos = self.pos + v
        for i in range(0,len(self.history)):
            self.history[i].append(self.pos[i])
w = Walker()
for x in range(0,1000000):
    w.step()
plt.plot(w.history[0],w.history[1],'-')
plt.show()
