import re
'''
run this script from the command line like
python -m run displayTree.py
'''

class Node:
    def __init__(self, obj, parent, children):
        self.obj = obj
        self.parent = parent
        # parent should be a Node object
        self.children = children
        # children should be a list of Node objects

    def getAllChildren(self):
        '''recursively gets all children'''
        if self.children == None:
            return None
        ans = []
        for child in self.children:
            if child.children == None:
                ans.append(child)
            else:
                ans += getAllChildren(child)


'''
documents
    school
        math
            math paper
        science
            science paper
            science research paper
        history
    finances
        expenses and balances
    code
        python
            pseudocode.py
        javascript
            script.js
        java
            strict.java
'''
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
        sarr[i] = '\t'*level + sarr[i]
    return '\n'.join(sarr)


def tabTree(tree, depth=0):
    ans = ''
    if tree == None:
        return ''
    for node in tree:
        ans += '\t'*depth+'{0}\n'.format(node)
        ans += tabTree(tree[node], depth+1)#, depth+1), depth)
    return ans

def lineTree(tree, depth=0):
    # tabbed = tabTree(tree)
    ans = ''
    if tree == None:
        return ''
    count = 0
    for node in tree:
        # print(count, len(tree), node)
        ans += ' '*depth*4
        if count == len(tree)-1:
            #last node on branch
            ans += '└─ {0}\n'.format(node)
        else:
            ans += '├─ {0}\n'.format(node)
        ans += lineTree(tree[node], depth+1)
        count += 1
    return ans
sarr = lineTree(tree).split('\n')

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
# transposed = re.sub(r'(├)( +)([├└])', r'\1│\3', transposed)
print('\n'.join(transposed))
for i in range(len(transposed)):
    match = re.search(r'├( +)[├└]', transposed[i])
    while match:
        matchstr = match.group()
        matchstr2 = re.sub(' ', '│', matchstr)
        transposed[i] = re.sub(matchstr, matchstr2, transposed[i])
        # print(transposed[i], '::', match.group())
        match = re.search(r'├( +)[├└]', transposed[i])
    # while match:
    #     print('match: ', match.group())
    #     startInd = transposed[i].index(match.group())
    #     endInd = startInd + len(match.group())
    #     for j in range(startInd+1, endInd):
    #         transposed[i] = transposed[i][:j]+'│'+transposed[i][j+1:]
    #         # should replace spaces with │
    #     match = re.search(r'├( +)[├└]', transposed[i])


# retranspose
sarr = []
for j in range(len(transposed[0])):
    row = ''
    for i in range(len(transposed)):
        row += transposed[i][j]
    sarr.append(row)

print('\n'.join(sarr))

# add trailing spaces to achieve uniform length to make list rectangular



# print(re.search(r'(\t+)├─ (.|[\n\t])+\1└─ ', s).group())

print(lineTree(tree))#, ['documents', 'school']))
