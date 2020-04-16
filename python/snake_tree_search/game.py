import random

class Vector:
    def __init__(self, x, y):
        self.x = x
        self.y = y
        
    def __add__(self, other):
        return Vector(self.x+other.x, self.y+other.y)
        
    def __eq__(self, other):
        return self.x == other.x and self.y == other.y
        
    def __hash__(self):
        return hash((self.x, self.y))
    
    def copy(self):
        return Vector(self.x, self.y)
    
    def __str__(self):
        return "<{}, {}>".format(self.x, self.y)
    
    def __repr__(self):
        return str(self)

def rand_vec(w, h):
    x = random.randint(0, w-1)
    y = random.randint(0, h-1)
    return Vector(x, y)

# directions
UP = 1
DOWN = 2
LEFT = 3
RIGHT = 4

def vec_of_direction(direction):
    if direction == UP:
        return Vector(0, -1)
    elif direction == DOWN:
        return Vector(0, 1)
    elif direction == LEFT:
        return Vector(-1, 0)
    elif direction == RIGHT:
        return Vector(1, 0)

# players
SNAKE = 0
FRUIT = 1

class Game:
    def __init__(self, w, h):
        self.w = w
        self.h = h
        self.body = []
        self.body.append(rand_vec(w, h))
        self.fruit = rand_vec(w, h)
        self.dead = False
        # so you don't recompute on every fruit spawn
        self.all_spots = [Vector(x, y) for x in range(w) for y in range(h)]
        self.age = 0
        self.players = [SNAKE,  FRUIT]

    def copy(self):
        ans = Game(self.w, self.h)
        ans.body = self.body[:]
        ans.fruit = self.fruit
        ans.dead = self.dead
        ans.all_spots = self.all_spots[:]
        ans.age = self.age
        return ans
    
    def head(self):
        return self.body[0]
    
    def inBounds(self, v):
        return v.x >= 0 and v.x < self.w and v.y >= 0 and v.y < self.h

    def __len__(self):
        return len(self.body)
    
    def head_in_body(self):
        if len(self) == 1:
            return False
        else:
            head_pos = self.head()
            return any(map(lambda p: head_pos == p, self.body[1:]))    
    
    def head_in_wall(self):
        return not self.inBounds(self.head())
    
    def head_in_fruit(self):
        return self.head() == self.fruit
    
    def good_spots(self):
        '''places where a fruit can spawn'''
        def is_good_spot(p):
            return p not in self.body
        return list(filter(is_good_spot, self.all_spots))
    
    def respawn_fruit(self):
        self.fruit = random.choice(self.good_spots())
    
    def move_snake(self, direction, should_respawn_fruit=False):
        '''move the snake, possibly growing the tail.
        DOESN'T RESPAWN FRUIT UNLESS should_respawn_fruit=True
        '''
        if self.dead:
            raise RuntimeError("cannot move while dead")
        new_pos = self.head() + vec_of_direction(direction)
        self.body.insert(0, new_pos)
        if not self.head_in_fruit():
            self.body = self.body[:-1]
        elif should_respawn_fruit:
            self.respawn_fruit()
        if self.head_in_body() or self.head_in_wall():
            self.dead = True
        self.age += 1
    
    def is_snake_move_safe(self, direction):
        g = self.copy()
        g.move_snake(direction)
        return not g.dead
    
    def status(self):
        return {'age': self.age, 'length': len(self), 'value': self.value()}

    def get_legal_actions(self, player):
        if player == SNAKE:
            return list(filter(self.is_snake_move_safe, [UP, DOWN, LEFT, RIGHT]))
        if player == FRUIT:
            if self.head_in_fruit():
                return self.good_spots()
            else:
                return [self.fruit]
    
    def is_legal_action(self, player, action):
        return action in self.get_legal_actions(player)
    
    def value(self):
        '''Prioritize liveness positively, then length positively, then age negatvely
        A living snake is worth more than a dead snake no matter what
        A long snake is worth more than a short snake if their liveness is the same
        A young snake is worth more than an old snake if all else is equal
        '''
        liveness = 0 if self.dead else 1
        return liveness - 1.0 / (len(self) + 1.0 / self.age)

    def try_action(self, player, action):
        '''doesn't actually mutate state
        player either SNAKE or FRUIT
        action UP DOWN LEFT RIGHT for SNAKE or a position for FRUIT
        returns new state
        '''
        if not self.is_legal_action(player, action):
            raise ValueError("illegal action")
        elif self.dead:
            raise RuntimeError("can't do action when dead")

        g = self.copy()
        if player == SNAKE:
            assert action in [UP, DOWN, LEFT, RIGHT]
            g.move_snake(action)
        elif player == FRUIT:
            g.fruit = action
        return g
    
    def __str__(self):
        base = [['_' for x in range(self.w)] for y in range(self.h)]
        base[self.fruit.y][self.fruit.x] = "F"
        for pos in self.body:
            base[pos.y][pos.x] = "S"
        base[self.head().y][self.head().x] = "H"
        return '\n'.join(map(lambda l:' '.join(l), base)) + "\n"
    
    def __repr__(self):
        return str(self.status())