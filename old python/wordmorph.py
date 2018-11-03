import random
def lcirc(word):
    return word[1:] + word[0]
def rcirc(word):
    return word[-1] + word[0:-1]
def reverse(word):
    return word[::-1]
def apply(word,opstr):
    ans = word
    for op in opstr:
        if op == 'L':
            ans = lcirc(ans)
        elif op == 'R':
            ans = rcirc(ans)
        elif op == 'r':
            ans = reverse(ans)
    return ans

permutations = set()
for composition in range(1000):
    word = 'abcd'
    for operation in range(50):
        word = apply(word,random.choice(['L','R','r']))
        permutations.add(word)
print(permutations, len(permutations))
