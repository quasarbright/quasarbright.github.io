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
    'da', 'dzu', 'de', 'do',
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
# handle special syllables
for syllable in syllables:
    # n can precede normal syllables
    syllableGraph.set_edge('n', syllable)
    # only allow dipthongs that "sound japanese"
    if syllable[-1] == 'a':
        syllableGraph.set_edge(syllable, 'a')
        syllableGraph.set_edge(syllable, 'i')
        syllableGraph.set_edge(syllable, 'e')
        syllableGraph.set_edge(syllable, 'o')
    elif syllable[-1] == 'i':
        syllableGraph.set_edge(syllable, 'i')
        syllableGraph.set_edge(syllable, 'e')
    elif syllable[-1] == 'u':
        syllableGraph.set_edge(syllable, 'a')
        syllableGraph.set_edge(syllable, 'i')
        syllableGraph.set_edge(syllable, 'u')
        syllableGraph.set_edge(syllable, 'e')
        syllableGraph.set_edge(syllable, 'o')
    elif syllable[-1] == 'e':
        syllableGraph.set_edge(syllable, 'i')
        syllableGraph.set_edge(syllable, 'o')
# we have to manually prevent vowel chains longer than dipthongs. The graph won't be enough

# now, generate using the rules
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
    
    return ''.join(wordList)

if __name__ == '__main__':
    for x in range(10):
        print(getNextSyllable(['ka', 'i']))