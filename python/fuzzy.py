import os
import re
import itertools

def concat(lists):
    return list(itertools.chain.from_iterable(lists))

def score(target, word):
    msd = -1*minimum_spanning_distance(target, word)
    return msd

def minimum_spanning_distance(target, word):
    target_re = ''.join((map(lambda c : f"({c}).*?", target)))
    def dist(startIndex):
        match = re.match(target_re, word[startIndex:])
        if match is None:
            return float('inf')
        else:
            return len(match.group())
            # return match.end(len(target)-1) - match.start(0)
    return min(map(dist, range(len(word))), default=float('inf'))

target = "edm"
words = ["mike delmonaco", "", "edm", "e                     d                  m", "hello", "eeeeeeeeeeeedddmedm"]

if __name__ == "__main__":
    import sys
    if len(sys.argv) < 3:
        print("usage: fuzzy.py [TARGET] [ROOTDIR]")
        sys.exit(1)
    else:
        target = sys.argv[1]
        d = sys.argv[2]
    walk = os.walk(d)
    def go(triple):
        root, _, files = triple
        return map(lambda file: os.path.join(root, file), files)
    files = concat(map(go, walk))
    files = sorted(files, key=lambda word: score(target, word))
    print("\n".join(files))
