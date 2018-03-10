import random, math, sys
#setup
numDoors = 3
doors = [0 for x in range(numDoors)]
randind = math.floor(random.random()*len(doors))
doors[randind] = 1
print('There are {0} doors. Behind one is a car, but the rest have goats'.format(numDoors))
#run game
guess = input('guess a door from 1 to {0}\n>>>')
guessind = int(guess)-1
indset = set([i for i in range(len(doors))])
indset.remove(guessind)
notGuessInd = indset.pop()
print('there are goats in {0}'.format(indset))
