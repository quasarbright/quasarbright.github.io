def prepend_lines(lines, s):
    '''prepends each line in lines (array of lines no newline) with s'''
    ans = []
    for line in lines:
        ans.append(s + line)
    return ans


def next_match(lines1, lines2, start1, start2):
    '''
    lines1 and lines2 are [string, ...] no newline
    start1, start2 are int indices representing the start for each list
    returns (a index, b index) for next match
    (-1, -1) if no more matches
    '''
    foundMatch = False
    i1Best = float('inf')
    i2Best = float('inf')
    bestDist = float('inf') # (i1best - start1) + (i2best - start2)
    for i1 in range(start1, len(lines1)):
        a = lines1[i1]
        if a in lines2[start2:]:
            # match
            foundMatch = True
            ain2 = lines2.index(a, start2)
            dist = (i1 - start1) + (ain2 - start2)
            if dist < bestDist:
                # new best match
                i1Best = i1
                i2Best = ain2
                bestDist = dist
    for i2 in range(start2, len(lines2)):
        b = lines2[i2]
        if b in lines1[start1:]:
            # match
            foundMatch = True
            bin1 = lines1.index(b, start1)
            dist = (bin1 - start1) + (i2 - start2)
            if dist < bestDist:
                # new best match
                i1Best = bin1
                i2Best = i2
                bestDist = dist
    
    if foundMatch:
        return i1Best, i2Best
    else:
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
        
