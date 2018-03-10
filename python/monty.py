import random, math, sys
#setup
numDoors = 3
doors = [0 for x in range(numDoors)]
randind = math.floor(random.random()*len(doors))
doors[randind] = 1
print('There are {0} doors. Behind one is a car, but the rest have goats'.format(numDoors))

#run game
# guess = int(input('guess a door from 0 to {0}\n>>>'.format(numDoors-1)))
guess = 0
inds = set([i for i in range(len(doors))])
inds.remove(guess)
inds = list(inds)
notGuess = inds.pop(math.floor(random.random()*len(inds)))
print(inds)
print(doors,guess,notGuess)
print('there are goats in {0}'.format(inds))
# choice = int(input('do you want to stick with door number {guess} (enter 0), or switch to door number {other} (enter 1)?'.format(guess=guess,other=notGuess)))
choice = 1 #auto switch
if choice == 0:
    if doors[guess] == 1:
        print('congratulations! you win a brand new car!')
    else:
        print('sorry bud, here\'s a goat')
if choice == 1:
    if doors[notGuess] == 1:
        print('congratulations! you win a brand new car!')
    else:
        print('sorry bud, here\'s a goat')
