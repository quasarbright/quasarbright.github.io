import random
from typing import List
from mylib.graph import DiGraph
# create building blocks for words
# these are syllables, vowels, and 'n'
vowels = 'aiueo'
# consonants that can be followed by all values
fullConsonants = 'knmrpgb'
specialSyllables = [*vowels, 'n']
syllables = []
for consonant in fullConsonants:
    for vowel in vowels:
        syllables.append(consonant+vowel)
# add other cases
syllables.extend([
    'sa', 'shi', 'su', 'se', 'so',
    'za', 'ji', 'zu', 'ze', 'zo',
    'ta', 'chi', 'tsu', 'te', 'to',
    'da', 'de', 'do',
    'ha', 'hi', 'fu', 'he', 'ho',
    'ya', 'yu', 'yo',
    'wa', 'wo',
    'cha', 'cho', 'sha', 'sho', 'shu'#, 'kya', 'kyo', 'pya', 'pyo'
                                     # would lead to trithongs
])

# words can start with everything but n
starters = syllables + [*vowels]

# make a graph representing the rules of word structure
# initialize nodes
syllableGraph = DiGraph()
syllableGraph.add_node(*syllables)
syllableGraph.add_node(*specialSyllables)

# add edges
# connect all normal syllables to each other
for a in syllables:
    for b in syllables:
        syllableGraph.set_edge(a, b)

# vowels can be followed by all syllables
for vowel in vowels:
    for syllable in syllables:
        syllableGraph.set_edge(vowel, syllable)

# n can follow anything but itself
for syllable in syllables+[*vowels]:
    syllableGraph.set_edge(syllable, 'n')

# handle special syllables
for syllable in syllables:
    # n can precede normal syllables
    syllableGraph.set_edge('n', syllable)
    # only allow dipthongs that "sound japanese"
    # this does syllable endings and standalone vowel syllables
    if syllable[-1] == 'a':
        for vowel in 'aieo':
            syllableGraph.set_edge(syllable, vowel)
            syllableGraph.set_edge('a', vowel)
    elif syllable[-1] == 'i':
        for vowel in 'ieo':
            syllableGraph.set_edge(syllable, vowel)
            syllableGraph.set_edge('i', vowel)
    elif syllable[-1] == 'u':
        for vowel in 'aiueo':
            syllableGraph.set_edge(syllable, vowel)
            syllableGraph.set_edge('u', vowel)
    elif syllable[-1] == 'e':
        for vowel in 'io':
            syllableGraph.set_edge(syllable, vowel)
            syllableGraph.set_edge('e', vowel)
    elif syllable[-1] == 'o':
        for vowel in 'aiue':
            syllableGraph.set_edge(syllable, vowel)
            syllableGraph.set_edge('o', vowel)

# now, generate using the rules
# we have to manually prevent vowel chains longer than 2. The graph won't be enough
def getNextSyllable(wordList: List[str]) -> str:
    '''
    wordList list of syllables, length > 0
    returns syllable as string
    '''
    word = ''.join(wordList)
    syllableChildren = syllableGraph.get_children(wordList[-1])
    nextSyllableCandidates = syllableChildren[:]
    if len(word) >= 2:
        # check if the last two characters are vowels
        if word[-1] in vowels and word[-2] in vowels:
            # if they are, vowels can't be next
            nextSyllableCandidates = set(syllableChildren) - set(vowels)
            nextSyllableCandidates = list(nextSyllableCandidates)
    nextSyllable = random.choice(nextSyllableCandidates)
    return nextSyllable



    
def generateWord(length: int=5) -> str:
    '''
    length in syllables, must be > 0
    returns word as string
    '''
    # start word
    # word is list of syllables
    wordList = [random.choice(starters)]
    while len(wordList) < length:
        wordList.append(getNextSyllable(wordList))
    return ''.join(wordList)

if __name__ == '__main__':
    for x in range(10):
        print(generateWord(5))
