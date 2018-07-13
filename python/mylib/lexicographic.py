def numCompareTo(a,b):
    """compares two numbers by their numerical value"""
    if a>b: return 1
    elif a==b: return 0
    elif a<b: return -1
def nextPerm(arr_,compareTo=numCompareTo):
    """generate the next permutation of arr given a compareTo function
    arr_ is a list
    compareTo returns 0, 1, or -1 for ==, >, and < respectively"""
    arr = list(arr_[:])#don't change arr
    i = len(arr)-1
    while compareTo(arr[i],arr[i-1]) <= 0 and i > 0: #a <= b
        i = i - 1
    if i == 0:
        return None
    else:
        pivot = arr[i-1]
        j = len(arr)-1
        while compareTo(pivot, arr[j]) >= 0: #a >= b
            assert j>=i
            j = j - 1
        successor = arr[j]
        arr[i-1] = successor
        arr[j] = pivot
        suffix = arr[i:]
        suffix = suffix[::-1]
        arr = arr[:i] + suffix
        return arr
def allPerms(arr):
    indices = tuple(range(len(arr)))
    """generates all permutations of a list
    in lexicographic order (according to index)
    """
    def myCompareTo(a,b):
        """compares two elements from indices by their numerical value"""
        if a>b: return 1
        elif a==b: return 0
        elif a<b: return -1
    yield tuple(arr)
    new = nextPerm(indices,myCompareTo)
    while new:
#         print('new:',[arr[i] for i in new])
#         if tuple([arr[i] for i in new]) in perms:print(tuple([arr[i] for i in indices]),'dup')#investigating why last perm not in set
        yield tuple([arr[i] for i in new])
        indices = new
        new = nextPerm(indices,myCompareTo)
if __name__=='__main__':
    print(list(allPerms('abc')))
