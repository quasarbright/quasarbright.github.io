'''
1. use a full binary heap.
    This is a binary (not search) tree where children
    of a node are less than that node. Fullness means all rows except possibly the
    last are full. Any row that isn't full has all of its nodes on the left and all
    of its empty spots are on the right. If you do a breadth first traversal and
    index all nodes (starting at zero) you'll find that the index of the i'th node's
    left child is 2*i+1 and the right child's is 2*i+2. This means the heap can be
    nicely represented by an array with the breadth first traversal indices.
2. build it by inserting each array element. Do this by first inserting at the
    "next" point in the heap. This means looking at the bottom row, and if there's
    an empty space, put it in the leftmost one. Otherwise, if the bottom row is full,
    put it in the leftmost space of the next row. Now, if it is greater than its parent,
    swap its place with its parent and continue until it is <= its parent or becomes
    the root. Since the heap will be represented by the list, keep track of where
    the heap starts and the "rest" of the list begins. But how do we find that "next"
    spot? The key feature of the breadth first traversal array representation is
    that the first index after the ending of the heap part of the array corresponds
    to exactly where the "next" position in the tree is where you'd insert it.
    So you can just treat the first element not in the heap as in the heap, swap
    that new element up like we just talked about, and it's inserted into the heap.
    repreat for all elements in the list. note that this is all in place and uses
    constant memory independent of the list size
3. remove each node.
    After building your heap, the root is the greatest element in the heap. Remove
    it by swapping it with the last element in the heap array. Visually, what's happening
    here is the same as swapping it with the rightmost element in the bottom row.
    Now consider that element as out of the heap. Now the heap is broken because
    a small value is the root. To fix this, swap the root with its largest child
    and repeat until its children are <= to itself or it has no children. Now you
    have a good heap, with one element in the array no longer in the heap. Repeat,
    but this time, swap the root with the second to last element in the whole array,
    which is the last element in the heap part of the array. Swap the bad root down
    until the heap is fixed, and keep going until the whole heap has been removed.
    Since you kept removing the largest element and putting it right after the
    end of the heap part of the array, the entire array is now sorted.
'''
def heapSort(l):
    def leftChildIndex(index):
        return 2 * index + 1
    def rightChildIndex(index):
        return 2 * index + 2
    def parentIndex(index):
        return (index - 1) // 2
    def swap(i, j):
        temp = l[i]
        l[i] = l[j]
        l[j] = temp
    def insert(endIndex):
        '''insert the element at endIndex into the heap
        endIndex represents the index of the first element in the list which
        is not in the heap yet'''
        assert endIndex < len(l)
        ind = endIndex
        x = l[ind]
        pind = parentIndex(ind)
        parent = l[pind]
        while parent < x: # < is important because when x is root, it'll terminate bc x !< x
            swap(ind, pind)
            ind = pind
            # x unchanged
            pind = parentIndex(ind) # left off here
            parent = l[pind]
    def remove(lastIndex):
        '''lastIndex represents the index of the last element in the heap'''
        if lastIndex > 0:
            swap(0, lastIndex)
            # setup for swapping down
            ind = 0
            x = l[ind]
            lind = leftChildIndex(ind)
            rind = rightChildIndex(ind)
            rchild = float('-inf')
            lchild = float('-inf')
            if lind < len(l):
                lchild = l[lind]
            if rind < len(l):
                rchild = l[rind]

            while (x < lchild or x < rchild) and (max(ind, lind, rind) < lastIndex):
                maxInd = lind
                maxChild = lchild
                if rchild >= lchild:
                    maxInd = rind
                    maxChild = rchild
                if x >= maxChild:
                    break
                else:
                    swap(ind, maxInd)
                    ind = maxInd
                    # x unchanged

                    lind = leftChildIndex(ind)
                    rind = rightChildIndex(ind)
                    rchild = float('-inf')
                    lchild = float('-inf')
                    if lind < len(l):
                        lchild = l[lind]
                    if rind < len(l):
                        rchild = l[rind]

    # reorder the list into a heap
    for endIndex in range(len(l)):
        insert(endIndex)
    print(l)
    for lastIndex in range(len(l)-1, 0, -1): # excludes 0, but that's ok
        remove(lastIndex)
    return l
import random
l = [random.randint(0, 100) for x in range(20)]
print(l)
print(heapSort(l))
