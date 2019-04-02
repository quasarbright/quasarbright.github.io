import random
alphabet = 'abcdefghijklmnopqrstuvwxyz '


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


def get_need_and_remove(word, target):
    wordlist = list(word)
    targetlist = list(target)
    wordset = set(list(word))
    targetset = set(list(target))
    intersect = wordset & targetset
    for letter in intersect:
        while letter in wordlist and letter in targetlist:
            wordlist.remove(letter)
            targetlist.remove(letter)
    lettersNeeded = targetlist
    lettersToRemove = wordlist
    assert set(lettersNeeded) & set(lettersToRemove) == set([])
    return lettersNeeded, lettersToRemove

def generate_better_moves(word, target):
    '''
    only add/remove letters that need to be added/removed
    then permute once word is a permutation of target.
    doesn't use replace
    '''
    moves = []
    lettersNeeded, lettersToRemove = get_need_and_remove(word, target)
    if lettersNeeded != []:
        moves.append((add, (word, 0, lettersNeeded[0])))
    elif lettersToRemove != []:
        moves.append((remove, (word, word.index(lettersToRemove[0]))))
    else:
        for i in range(len(word)):
            for j in range(i+1, len(word)):
                moves.append((swap, (word, i, j)))
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
def min_path(word, target, generator=generate_better_moves):
    if word == target:
        return []
    # list of (word, [move]) pairs
    queue = []
    nextMoves = generator(word, target)
    for move in nextMoves:
        f, args = move
        nextWord = f(*args)
        if nextWord == target:
            return [move]
        queue.append((nextWord, [move]))
    seenWords = set([])
    last = None
    while len(queue) > 0 and last is None:
        # import pdb; pdb.set_trace()
        curr = queue.pop(0)

        a, prevMoves = curr
        print(a, len(queue))
        if a not in seenWords:
            seenWords.add(a)
            prevWord = prevMoves[-1][0]
            nextMoves = generator(a, target)
            for move in nextMoves:
                f, args = move
                nextWord = f(*args)
                if nextWord not in seenWords:
                    if nextWord == target:
                        last = (target, prevMoves+[move])
                    else:
                        queue.append((nextWord, prevMoves+[move]))
    return last[1]


def non_bfs_min_path(word, target):
    words = [word]
    moves = []
    lettersNeeded, lettersToRemove = get_need_and_remove(word, target)

    def add_letter():
        nonlocal word, words, moves, lettersNeeded, lettersToRemove
        oldWord = word
        word = add(word, 0, lettersNeeded[int(random.random()*len(lettersNeeded))])
        words.append(word)
        moves.append((add, (oldWord, 0, lettersNeeded[-1])))
        lettersNeeded, lettersToRemove = get_need_and_remove(word, target)
    def remove_letter():
        nonlocal word, words, moves, lettersNeeded, lettersToRemove
        oldWord = word
        ind = word.index(lettersToRemove[0])
        word = remove(word, ind)
        words.append(word)
        moves.append((remove, (oldWord, ind)))
        lettersNeeded, lettersToRemove = get_need_and_remove(word, target)
    # add letters as necessary
    while lettersNeeded != [] or lettersToRemove != []:
        if lettersNeeded != [] and lettersToRemove != []:
            if random.random() < .5:
                remove_letter()
            else:
                add_letter()
        elif lettersNeeded != []:
            add_letter()
        else:
            remove_letter()
    # remove letters
    while lettersToRemove != []:
        remove_letter()
    assert lettersNeeded == [] and lettersToRemove == []
    assert len(word) == len(target)
    # basically do selection sort
    for startIndex in range(len(word)-1):
        wordLetter = word[startIndex]
        targetLetter = target[startIndex]
        if wordLetter != targetLetter:
            # index of the target letter in the rest of the string
            offset = word[startIndex:].index(targetLetter)
            targetLetterIndex = startIndex + offset

            oldWord = word
            word = swap(word, startIndex, targetLetterIndex)
            words.append(word)
            moves.append((swap, (oldWord, startIndex, targetLetterIndex)))
    assert word == target
    return words, moves


def print_path(path):
    f2s = {
        add:'add',
        remove:'remove',
        swap: 'swap',
        replace: 'replace',
    }
    for move in path:
        f, args = move
        currWord = args[0]
        nextWord = f(*args)
        fstr = f2s[f]
        print(currWord)
    print(nextWord)
words, moves = non_bfs_min_path("hey vsauce. michael here!", 'whats jablin jables?')
print(*words, sep='\n')
