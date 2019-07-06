import matplotlib.pyplot as plt
import numpy as np
class Vector:
    def __init__(self, x, y):
        self.x = x
        self.y = y
    
    @staticmethod
    def fromPolar(r, theta):
        return Vector(r*np.cos(theta), r*np.sin(theta))
    
    def __add__(self, other):
        return Vector(self.x + other.x, self.y + other.y)
    
    def __sub__(self, other):
        return Vector(self.x - other.x, self.y - other.y)
    
    def mult(self, k):
        return Vector(self.x * k, self.y * k)
    
    def mag(self):
        return np.sqrt(self.x**2 + self.y**2)
    
    def magSq(self):
        return self.x**2 + self.y**2

class World:
    def __init__(self, n, magnetForce=1, pendulumForce=1, frictionForce=1, finishDistance=.1, finishVelocity=.1, dt=1/60):
        '''
        n: number of magnets
        magnetForce, pendulumForce, frictionForce: force constants
        finishDistance: when the pendulum is close enough to consider it landed
        finishVelocity: when the pendulum is slow enough to consider it stopped
        dt: time step in seconds
        '''
        self.n = n
        self.magnetPositions = []
        for theta in np.linspace(0, 2*np.pi, n+1)[:-1]:
            self.magnetPositions.append(Vector.fromPolar(1, theta))
        self.magnetForce = magnetForce
        self.pendulumForce = pendulumForce
        self.frictionForce = frictionForce
        self.finishDistance = finishDistance
        self.finishVelocity = finishVelocity
        self.dt = dt
    
    def calculatePendulumForce(self, pos):
        # hooke's law
        # F = -kx where k is pendulumForce and x is pos
        return pos.mult(-self.pendulumForce)
    
    def calculateMagneticForce(self, pos):
        # inverse square law summed over all magnets
        # F = -k * r/(|r|^3) where k is magnetForce and r is displacement from magnet
        resultantForce = Vector(0,0)
        for magnetPosition in self.magnetPositions:
            displacement = pos - magnetPosition
            dispMag = displacement.mag()
            force = displacement.mult(-self.magnetForce/(dispMag**3))
            resultantForce += force
        return resultantForce
    
    def calculateFrictionForce(self, vel):
        # F = -kv
        return vel.mult(-1*self.frictionForce)
    
    def calculateTotalForce(self, pos, vel):
        totalForce = Vector(0,0)
        totalForce += self.calculatePendulumForce(pos)
        totalForce += self.calculateMagneticForce(pos)
        totalForce += self.calculateFrictionForce(vel)
        return totalForce
    
    def getClosestMagnetInfo(self, pos):
        '''
        returns index, distSquared
        '''
        closestMagnetIndex = -1
        closestDistSquared = float('inf')
        for magnetIndex, magnetPosition in enumerate(self.magnetPositions):
            displacement = pos-magnetPosition
            distSquared = displacement.magSq()
            if distSquared < closestDistSquared:
                closestMagnetIndex = magnetIndex
                closestDistSquared = distSquared
        return closestMagnetIndex, closestDistSquared
    
    def isDone(self, pos, vel):
        closestMagnetIndex, closestMagnetDistSquared = self.getClosestMagnetInfo(pos)
        closeEnough = closestMagnetDistSquared < self.finishDistance**2
        slowEnough = vel.magSq() < self.finishVelocity**2
        return closeEnough and slowEnough

    def simulate(self, startingPosition, mass, show=False, maxIterations=1000):
        '''
        simulates the system with a pendulum and n magnets
        returns the ending magnet index and the trace length
        '''
        position = startingPosition
        velocity = Vector(0,0)
        acceleration = Vector(0,0)
        traceLength = 0
        iterations = 0
        if show:
            plt.axis([-2,2,-2,2])
            for magnetPosition in self.magnetPositions:
                plt.scatter(magnetPosition.x, magnetPosition.y)
            plt.scatter(position.x, position.y)
            plt.pause(self.dt)
        while not self.isDone(position, velocity) and iterations < maxIterations:
            force = self.calculateTotalForce(position, velocity)
            acceleration = force.mult(1/mass)

            # a = dv/dt
            # v = dp/dt (where p = position, not momentum)

            dv = acceleration.mult(self.dt)
            velocity += dv

            dp = velocity.mult(self.dt)
            position += dp
            traceLength += dp.mag()

            iterations += 1

            if show:
                plt.scatter(position.x, position.y)
                plt.pause(self.dt)
        
        plt.show()

        
        closestMagnetIndex, closestMagnetDistSquared = self.getClosestMagnetInfo(position)
        return closestMagnetIndex, traceLength



        
if __name__ == '__main__':
    world = World(3, frictionForce=2)
    world.simulate(Vector(1,1), mass=1, show=True)



        
    




# plt.axis([0, 10, 0, 1])

# for i in range(10):
#     y = np.random.random()
#     plt.scatter(i, y)
#     plt.pause(0.5)

# plt.show()
