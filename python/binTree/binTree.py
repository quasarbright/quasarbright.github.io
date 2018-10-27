'''
A BT is one of
BT(Number, BT, BT)
None
'''
class InvalidBinTreeError(Exception):
    pass


class BT:
    def __init__(self, num, left=None, right=None):
        #input validation
        if not (isis)
        if not (isinstance(left, BT) or left == None):
            raise InvalidBinTreeError("left tree is not a valid BT: {0}".format(left))
        if not (isinstance(right, BT) or right == None):
            raise InvalidBinTreeError("right tree is not a valid BT: {0}".format(right))

        self.left = left
        self.right = right

    def flatten(self):
        '''returns a list with all nodes, using infix'''
        if self.left == None and self.right == None:
            return [self.num]
        elif self.left == None and isinstance(self.right, BT):
            return [self.num] + self.right.flatten()
        elif isinstance(self.left, BT) and self.right == None:
            return self.left.flatten() + [self.num]
        else:
            return self.left.flatten() + self.right.flatten()


class UnorderedBSTError(Exception):
    pass


class BST(BT):
    def __init__(self, left, right):
        super().__init__(self, left, right)

        # validate tree structure
        '''if not self.isValid():
            raise UnorderedBSTError("BST is not properly ordered")'''

    def isValid(self):
        '''is the tree a valid binary search tree?'''
        flattened = self.flatten()
        ans = True
        for i in range(0, len(flattened) - 1):
            if flattened[i] < flattened[i + 1]:
                ans = False
        return ans
