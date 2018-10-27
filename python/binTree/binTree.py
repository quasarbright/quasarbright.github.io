'''
A BT is one of
BT(Number, BT, BT)
None
'''
class InvalidBinTreeError(Exception):
    pass


class BT:
    def __init__(self, num, left=None, right=None):
        # input validation
        if not (isinstance(left, BT) or left == None):
            raise InvalidBinTreeError("left tree is not a valid BT: {0}".format(left))
        if not (isinstance(right, BT) or right == None):
            raise InvalidBinTreeError("right tree is not a valid BT: {0}".format(right))

        self.num = num
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

    def __Str__(self):
        if self.left == None and self.right == None:
            return "({0})".format(self.num)
        elif self.left == None:
            return "({0}, (), {1})".format(self.num, str(self.right))
        elif self.right == None:
            return "({0}, {1}, ())".format(self.num, str(self.left))
        else:
            return "({0}, {1}, {2})".format(self.num, str(self.left), str(self.right))


class UnorderedBSTError(Exception):
    pass


class BST(BT):
    def __init__(self, num, left=None, right=None):
        super().__init__(num, left, right)
        # validate tree structure
        #if not self.isValid():
            #raise UnorderedBSTError("BST is not properly ordered")

    def isValid(self):
        '''is the tree a valid binary search tree?'''
        flattened = self.flatten()
        for i in range(0, len(flattened) - 1):
            if flattened[i] < flattened[i + 1]:
                return False
        return True
