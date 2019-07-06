def prepend_lines(lines, s):
    '''prepends each line in lines (array of lines no newline) with s'''
    ans = []
    for line in lines:
        ans.append(s + line)
    return ans

def find_used_matches(lines1, lines2):
    '''
    simulates using the next shortest distance match after each match
    and returns [(i1, i2),...] as they would be used
    '''
    ans = [] # [(i1, i2),...]
    i = 0
    j = 0
    matchI, matchJ = next_match(lines1, lines2, i, j)
    while (matchI, matchJ) != (-1, -1):
        # while we havent hit the end and there's matches left
        # (if we hit the end, we get (-1, -1))
        ans.append((matchI, matchJ))
        i = matchI + 1
        j = matchJ + 1
        matchI, matchJ = next_match(lines1, lines2, i, j)
    return ans
    
def universal_diff(list1, list2):
    '''expects 2 iterables of comparables which have str functions
    returns [(indices, deletions, insertions, same),...]'''
    ans = []
    used_matches = find_used_matches(list1, list2)
    if len(used_matches) == 0:
        return [((0,0,len(list1),len(list2)), list1, list2, [],)]
    i1, i2 = (0,0)
    for match in used_matches:
        mi1, mi2 = match
        indices = (i1, i2, mi1, mi2)
        deletions = list1[i1:mi1]
        insertions = list2[i2:mi2]
        same = list1[mi1]
        i1 = mi1 + 1
        i2 = mi2 + 1
        ans.append((indices, deletions, insertions, same))
    ans.append(((i1,i2, len(list1), len(list2)),list1[i1:],list2[i2:],list1[0:0]))
    return ans

def display_line_diffs_only(diff):
    '''
    doesn't show sames
    shows line insertions and deletions
    with line numbers
    '''
    for delta in diff:
        indices, deletions, insertions, same = delta
        i1, i2, mi1, mi2 = indices
        if len(deletions) > 0 or len(insertions) > 0:
            # if there's a delta
            # print line range of difference
            print('({}, {}) - ({}, {})'.format(i1+1, i2+1, mi1+1, mi2+2))
            if len(deletions) > 0:
                print('\n'.join(prepend_lines(deletions, '- :')))
            if len(insertions) > 0:
                print('\n'.join(prepend_lines(insertions, '+ :')))

def display_character_diff(diff):
    '''
    mike delmonaco -> 
    moke dedudelmonco
    m{-i-}{+o+}ke de{+dude+}lmon{-a-}co
    '''
    ans = ''
    for delta in diff:
        indices, deletions, insertions, same = delta
        if len(deletions) > 0:
            ans += '{-'+deletions+'-}'
        if len(insertions) > 0:
            ans += '{+'+insertions+'+}'
        ans += same
    print(ans)

# next match complexity len(lines1)^2 + len(lines2)^2
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
    # import sys
    # file1 = sys.argv[1]
    # file2 = sys.argv[2]
    # lines1 = file_to_lines(file1)
    # lines2 = file_to_lines(file2)
    # print('\n'.join(line_by_line_diff(lines1, lines2)))
    # print(character_by_character_diff('mike delmonaco', 'moke dedudelmonco'))
    # print(character_by_character_diff('mike', 'moke'))
    new_diff = file_to_lines('diff.py')
    old_diff = file_to_lines('old_diff')
    mike = 'mike delmonaco'
    moke = 'moke dedudelmonco'
    print(find_used_matches(mike, moke))
    display_line_diffs_only(universal_diff(old_diff, new_diff))
