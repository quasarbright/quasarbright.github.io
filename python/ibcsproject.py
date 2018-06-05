import re, sys
ins = sys.argv[1:]
switches = [
    [r'^MAC','MC'],
    [r'^KN','N'],
    [r'^K','C'],
    [r'^PH','F'],
    [r'^PF','F'],
    [r'^SCH','S'],
    [r'EE$','Y'],
    [r'IE$','Y'],
    [r'DT$','D'],
    [r'RT$','D'],
    [r'RD$','D'],
    [r'NT$','D'],
    [r'ND$','D'],
    [r'EV','AF'],
    [r'[AEIOU]','A'],
    [r'Q','G'],
    [r'Z','S'],
    [r'M','N'],
    [r'KN','N'],
    [r'K','C'],
    [r'SCH','S'],
    [r'PH','F'],
    [r'([^AEIOU])?H([^AEIOU])?',r'\1\2'],
    [r'([AEIOU])?W',r'\1'],
    [r'(\w)\1+',r'\1'],
    [r'S$',''],
    [r'AY$','Y'],
    [r'A$','']
]
def encode(word):
    for switch in switches:
        pat = switch[0]
        rep = switch[1]
        word = re.sub(pat, rep, word)
    return word
for i in ins:
    print(encode(i))
