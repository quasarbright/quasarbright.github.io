alphabet = 'abcdefghijklmnopqrstuvwxyz'


def remove(s, i):
    if 0 <= i and i < len(s):
        return s[:i] + s[i+1:]
    return s


def add(s, i, character):
    '''puts it at i'''
    return s[:i] + character + s[i:]


def swap(s, i, j):
    if i >= len(s) or j >= len(s):
        return s
    ans = ''
    for ind in range(len(s)):
        if ind == i:
            ans += s[j]
        elif ind == j:
            ans += s[i]
        else:
            ans += s[ind]
    return ans


def generate_possible_moves(s):
    moves = []
    for i in range(len(s)):
        moves.append((remove, (s, i)))
        for j in range(i+1, len(s)):
            if i != j:
                move = (swap, (s, i, j))
                if move not in moves:
                    moves.append(move)
    for i in range(len(s)+1):
        for letter in alphabet:
            moves.append((add, (s, i, letter)))
    return moves


def generate_possible_next_states(s):
    moves = generate_possible_moves(s)
    next_states = set([])
    next_states_with_moves = []
    for move in moves:
        f, args = move
        next_state = f(*args)
        if next_state not in next_states:
            next_states.add(next_state)
            next_states_with_moves.add((next_state, move))
    return next_states_with_moves

"""
def min_path(word, target):
    '''
    return minimum length list of moves
    '''
    # start string -> best path
    # path stored as a list of moves
    mem = {}
    def help(a, maxlen):
        '''
        computes the best path from a to target
        may return none if maxlen is too short
        returns [] if it reaches the target
        '''
        if a in mem:
            return mem[a]
        if a == target:
            return []
        if maxlen == 0:
            return None
        nextMoves = generate_possible_moves(a)
        bestPath = None
        bestPathLength = float('inf')
        for move in nextMoves:
            f, args = move
            nextWord = f(*args)
            # best path from nextWord to target
            bestRestPath = help(nextWord, maxlen-1)
            if bestRestPath is not None:
                path = [move] + bestRestPath
                if len(path) < bestPathLength:
                    bestPath = path
                    bestPathLength = len(path)
        mem[a] = bestPath
        return bestPath
    # don't let any sequence of transformations get longer than removing
    # everything from a and adding b to it
    return help(word, len(word) + len(target))
"""
# breadth first
def min_path(word, target):
    if word == target:
        return []
    # list of (word, move) pairs
    queue = [(word, None)]
    seenWords = set([])
    last = None
    while len(queue) > 0:
        curr = queue.popLeft()

        if curr not in seenWords:
            seenWords.add(curr)
            a, prevMove = curr
            prevWord = prevMove[0]
            nextMoves = generate_possible_moves(a)
            for move in nextMoves:
                f, args = move
                nextWord = f(*args)
                if nextWord not in seenWords:
                    if nextWord == target:
                        last = (target, move)
                        break
                    else:
                        queue.append((nextWord, move))
    assert last is not None
    moveList = []
    while last != (word, None):
        move, word ## oof can't backtrack if you clear the queue
