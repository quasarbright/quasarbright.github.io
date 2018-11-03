from random import randint
class Walker(object):
    def __init__(self):
        self.pos = [0,0]
        self.history = [self.pos]
        # self.MOVES = [[1, 0], [1, 1], [0,1], [-1, 1], [-1, 0], [-1, -1], [0, -1], [1, -1]]
        self.MOVES = [[1, 0], [0,1], [-1, 0], [0, -1]]
        self.hu = randint(0,255)
    def step(self):
        move = self.MOVES[randint(0,len(self.MOVES)-1)]
        self.pos[0] = self.pos[0]+move[0]*scale
        self.pos[1] = self.pos[1]+move[1]*scale
        if self.pos[0] < -width/2 or self.pos[0] > width/2 or self.pos[1] < -height/2 or self.pos[1] > height/2:
            self.__init__()
        self.history.append([self.pos[0],self.pos[1]])
    def show(self):
        stroke(self.hu,255,255)
        strokeWeight(5)
        pushMatrix()
        translate(width/2,height/2)
        line(self.history[len(self.history)-2][0],self.history[len(self.history)-2][1],self.history[len(self.history)-1][0],self.history[len(self.history)-1][1])
        popMatrix()
walkers = []
scale = 15
popsize = 50
stepTime = 1
def setup():
    frameRate(6000)
    colorMode(HSB)
    for x in range(0,popsize):
        walkers.append(Walker())
    # size(1000,1000)
    fullScreen()
    background(0)
def draw():
    fill(0,2)
    rect(-100,-100,width*2,height*2)
    if(frameCount%stepTime == 0):
        for i in range(0,popsize):
            walkers[i].step()
            walkers[i].show()