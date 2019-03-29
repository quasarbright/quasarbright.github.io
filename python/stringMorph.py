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
