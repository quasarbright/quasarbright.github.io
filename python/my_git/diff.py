def prepend_lines(lines, s):
    '''prepends each line in lines (array of lines no newline) with s'''
    ans = []
    for line in lines:
        ans.append(s + line)
    return ans


def next_match(lines1, lines2, i, j):
    '''
    lines1 and lines2 are [string, ...] no newline
    i, j are int indices representing the start for each list
    returns (a index, b index) for next match
    (-1, -1) if no more matches
    '''
    # i, j are start
    # a, b are lines1[i], lines2[j]
    # ain2, bin1 are indices of a, b in the other list respectively
    # either returns (i, ain2) or (bin1, j)
    while i < len(lines1) and j < len(lines2):
        a = lines1[i]
        b = lines2[j]
        if a in lines2[j:] and b in lines1[i:]:
            ain2 = lines2.index(a, j)
            bin1 = lines1.index(b, i)
            if ain2-j < bin1-i:
                # we care about which one is sooner
                # relative to where we are now
                return i, ain2
            else:
                return bin1, j
        elif a in lines2[j:] and b not in lines1[i:]:
            ain2 = lines2.index(a, j)
            return i, ain2
        elif a not in lines2[j:] and b in lines1[i:]:
            bin1 = lines1.index(b, i)
            return bin1, j
        else:
            i += 1
            j += 1
    return -1, -1


def line_by_line_diff(lines1, lines2):
    '''
    expects array of lines (no newlines)
    returns arary of lines (no newlines)
    '''
    ans = [] # array of lines
    i = 0
    j = 0
    matchI, matchJ = next_match(lines1, lines2, i, j)
    insertions = None
    deletions = None
    while i < len(lines1) and j < len(lines2):
        if (matchI, matchJ) == (-1,-1):
            deletions = lines1[i:]
            insertions = lines2[j:]
            ans.extend(prepend_lines(deletions, "-:" ))
            ans.extend(prepend_lines(insertions, "+:"))
            return ans
        else:
            deletions = lines1[i:matchI]
            insertions = lines2[j:matchJ]
            ans.extend(prepend_lines(deletions, "-:" ))
            ans.extend(prepend_lines(insertions, "+:"))
            ans.extend(prepend_lines([lines1[matchI]], " :"))
            i = matchI + 1
            j = matchJ + 1
        matchI, matchJ = next_match(lines1, lines2, i, j)
    return ans

def character_by_character_diff(s1, s2):
    '''
    expects two strings
    returns string
    '''
    ans = ''
    i = 0
    j = 0
    matchI, matchJ = next_match(s1, s2, i, j)
    insertions = None
    deletions = None
    while i < len(s1) and j < len(s2):
        if (matchI, matchJ) == (-1, -1):
            deletions = s1[i:]
            insertions = s2[j:]
            if len(deletions) > 0:
                ans += "{-"+deletions+"-}"
            if len(insertions) > 0:
                ans += "{+"+insertions+"+}"
            return ans
        else:
            deletions = s1[i:matchI]
            insertions = s2[j:matchJ]
            if len(deletions) > 0:
                ans += "{-"+deletions+"-}"
            if len(insertions) > 0:
                ans += "{+"+insertions+"+}"
            ans += s1[matchI]
            i = matchI + 1
            j = matchJ + 1
        matchI, matchJ = next_match(s1, s2, i, j)
    return ans

def file_to_lines(path):
    with open(path, 'r') as f:
        return f.read().splitlines()
if __name__ == '__main__':
    import sys
    # file1 = sys.argv[1]
    # file2 = sys.argv[2]
    # lines1 = file_to_lines(file1)
    # lines2 = file_to_lines(file2)
    # print('\n'.join(line_by_line_diff(lines1, lines2)))
    print(character_by_character_diff('mike delmonaco', 'moke dedudelmonco'))
    print(character_by_character_diff('mike', 'moke'))
        
