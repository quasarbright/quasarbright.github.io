import unittest
from binTree import *

bst = BST(5,
    BST(3,
        BST(2),
        BST(4)),
    BST(8,
        BST(6,
            None,
            BST(7)),
        None))
'''
    5
 3     8
2 4  6
      7
'''
bst2 = BST(10,
    BST(5,
        BST(3,
            BST(2,
                BST(1),
                None),
            BST(4)),
        None),
    BST(11))
'''
     10
    5  11
  3
 2 4
1
'''
def tryBadBST():
    badbst = BST(5,
        BST(3,
            BST(2),
            BST(6)),
        BST(8,
            BST(7),
            None))
    return badbst
'''
   5
 3   8
2 6   7
'''
class TestBinTree(unittest.TestCase):
    def testFlatten(self):
        self.assertEqual(bst.flatten(), [2, 3, 4, 5, 6, 7, 8])
        self.assertEqual(bst2.flatten(), [1, 2, 3, 4, 5, 10, 11])
    def testBadCreation(self):
        self.assertRaises(InvalidBinTreeError, lambda x: BT(2, "i'm a tree i swear", 1337))
    def testSTR(self):
        self.assertEqual(str(bst), "(5, (3, (2, (), (),), (4, (), ())), (8, (6, (), (7, (), ())), ()))")
class TestBinarySearchTree(unittest.TestCase):
    def testBadCreation(self):
        self.assertRaises(UnorderedBSTError, tryBadBST)
