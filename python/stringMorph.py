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
    for move in moves:
        f, args = move
        next_states.add(f(*args))
    return next_states


def min_dist(a, b):
    pass
