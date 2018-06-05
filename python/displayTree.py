'''
run this script from the command line like
python -m run displayTree.py
'''
import re


def getChildrenAsList(tree, nodeAddress):
    '''nodeAddress is a list of strings from root to destination'''
    current = tree
    for node in nodeAddress:
        current = current[node]
    ans = []
    for node in current:
        if node != None:
            ans.append(node)
            if current[node] != None:
                newNodeAddress = nodeAddress[:]
                newNodeAddress.append(node)
                ans += getChildrenAsList(tree, newNodeAddress)
    return ans


def indent(s, level=1):
    sarr = s.split('\n')
    for i in range(len(sarr)):
        sarr[i] = '\t' * level + sarr[i]
    return '\n'.join(sarr)


def tabTree(tree, depth=0):
    ans = ''
    if tree == None:
        return ''
    for node in tree:
        ans += '\t' * depth + '{0}\n'.format(node)
        ans += tabTree(tree[node], depth + 1)  # , depth+1), depth)
    return ans


def lineTree(tree):
    # generate base string using recursion
    def f(tree, depth=0):
        ans = ''
        if tree == None:
            return ''
        count = 0
        for node in tree:
            ans += ' ' * depth * 4
            if count == len(tree) - 1:
                # last node on branch
                ans += '└─ {0}\n'.format(node)
            else:
                ans += '├─ {0}\n'.format(node)
            ans += f(tree[node], depth + 1)
            count += 1
        return ans

    # now vertically connect ├s to ├s and └s with │s
    sarr = f(tree).split('\n')

    # rectangularize with trailing spaces to transpose
    maxVal = -1
    maxInd = -1
    for i in range(len(sarr)):
        if len(sarr[i]) > maxVal:
            maxVal = len(sarr[i])
            maxInd = i
    for i in range(len(sarr)):
        while len(sarr[i]) < maxVal:
            sarr[i] += ' '
    transposed = []
    for stri in range(maxVal):
        row = ''
        for arri in range(len(sarr)):
            row += sarr[arri][stri]
        transposed.append(row)

    # now that it's transposed, replace spaces with │ where appropriate
    for i in range(len(transposed)):
        match = re.search(r'├( +)[├└]', transposed[i])
        while match:
            matchstr = match.group()
            matchstr2 = re.sub(' ', '│', matchstr)
            transposed[i] = re.sub(matchstr, matchstr2, transposed[i])
            # print(transposed[i], '::', match.group())
            match = re.search(r'├( +)[├└]', transposed[i])

    # retranspose
    sarr = []
    for j in range(len(transposed[0])):
        row = ''
        for i in range(len(transposed)):
            row += transposed[i][j]
        sarr.append(row)
    return '\n'.join(sarr)


if __name__ == "__main__":
    tree = {
        'documents': {
            'school': {
                'math': {
                    'math paper': None
                },
                'science': {
                    'science paper': None,
                    'science research paper': None
                },
                'history': None
            },
            'finances': {
                'expenses and balances': None
            },
            'code': {
                'python': {
                    'pseudocode.py': None
                },
                'javascript': {
                    'script.js': None
                },
                'java': {
                    'strict.java': None
                }
            }
        }
    }
    print(lineTree(tree))  # , ['documents', 'school']))
