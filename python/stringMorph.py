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


def replace(s, i, letter):
    return s[:i] + letter + s[i+1:]


def generate_possible_moves(s):
    moves = []
    for i in range(len(s)):
        moves.append((remove, (s, i)))
        for letter in alphabet:
            moves.append((replace, (s, i, letter)))
        for j in range(i+1, len(s)):
            if i != j:
                move = (swap, (s, i, j))
                if move not in moves:
                    moves.append(move)
    for i in range(len(s)+1):
        for letter in alphabet:
            moves.append((add, (s, i, letter)))
    return moves


def generate_good_moves(word, target):
    s = word
    # eliminates some repetition of equivalent sequences by getting lengths equal first,
    # then manipulating.
    moves = []
    for i in range(len(s)):
        # only remove if word is bigger
        if len(word) > len(target):
            moves.append((remove, (s, i)))
        # only replace/swap if lengths equal
        elif len(word) == len(target):
            # only replace if stuff needs to be replaced
            if frozenset(list(word)) != frozenset(list(target)):
                for letter in alphabet:
                    moves.append((replace, (s, i, letter)))
            else:
                # only swap if you only need to permute
                for j in range(i+1, len(s)):
                    if i != j:
                        move = (swap, (s, i, j))
                        if move not in moves:
                            moves.append(move)
    # only add if word is smaller
    if len(word) < len(target):
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


# breadth first
def min_path(word, target):
    if word == target:
        return []
    # list of (word, [move]) pairs
    queue = []
    nextMoves = generate_good_moves(word, target)
    for move in nextMoves:
        f, args = move
        nextWord = f(*args)
        if nextWord == target:
            return [move]
        queue.append((nextWord, [move]))
    seenWords = set([])
    last = None
    while len(queue) > 0 and last is None:
        import pdb; pdb.set_trace()
        curr = queue.pop(0)
        a, prevMoves = curr
        if a not in seenWords:
            seenWords.add(a)
            prevWord = prevMoves[-1][0]
            nextMoves = generate_good_moves(a, target)
            for move in nextMoves:
                f, args = move
                nextWord = f(*args)
                if nextWord not in seenWords:
                    if nextWord == target:
                        last = (target, prevMoves+[move])
                    else:
                        queue.append((nextWord, prevMoves+[move]))
    return last[1]
print(min_path('michael','chimelb'))
### left off realizing that you should only replace "bad" characters using set arithmetic
## queue explosion due to to many options (bfs too wide) is main problem
# maybe go letter by letter and remove common letters from each word, and replace the letters that are left
# maybe give remove, add, and replace less options to avoid queue explosion. Maybe only let them happen at
# the beginning of the string, and let swap handle the rest?
