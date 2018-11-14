from mylib.graph import *
import unittest

# black widow shape isometric to square, not isometric to complete-ish graph
# 1 2
# 3 4
# black widow cycle
G1 = DiGraph()
G1.add_node(1, 2, 3, 4)
G1.set_edge(1, 2)
G1.set_edge(2, 3)
G1.set_edge(3, 4)
G1.set_edge(4, 1)
# square cycle
G2 = DiGraph()
G2.add_node(1, 2, 3, 4)
G2.set_edge(1, 2)
G2.set_edge(2, 4)
G2.set_edge(4, 3)
G2.set_edge(3, 1)
# both
G3 = DiGraph()
G3.add_node(1, 2, 3, 4)
G3.set_edge(1, 2)
G3.set_edge(2, 3)
G2.set_edge(2, 4)
G3.set_edge(3, 4)
G2.set_edge(4, 3)
G3.set_edge(4, 1)
G2.set_edge(3, 1)



'''
maybe remove an edge and recurse?
'''



class Test(unittest.TestCase):
    def test(self):
        pass
unittest.main()
