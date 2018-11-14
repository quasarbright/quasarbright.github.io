from mylib.graph import *
from mylib.lexicographic import allPerms
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
G3.set_edge(2, 4)
G3.set_edge(3, 4)
G3.set_edge(4, 3)
G3.set_edge(4, 1)
G3.set_edge(3, 1)

def isCorrectMapping(g, h, gnodes, hnodes):
    nodemap = {}
    for gnode, hnode in zip(gnodes, hnodes):
        nodemap[gnode] = hnode
    for gu in gnodes:
        for gv in gnodes:
            hu = nodemap[gu]
            hv = nodemap[gv]
            guchildren = set(g.get_children(gu))
            huchildren_guess = set(nodemap[n] for n in guchildren)
            huchildren_actual = set(h.get_children(hu))
            if huchildren_guess != huchildren_actual:
                return False
    return True

def areIsomorphic(g, h):
    # check number of nodes
    if len(g.get_nodes()) != len(h.get_nodes()):
        return False
    # check number of edges
    if len(g.get_edges()) != len(h.get_edges()):
        return False
    # check total degrees
    if g.get_total_in_degree() != h.get_total_in_degree() or g.get_total_out_degree() != h.get_total_out_degree():
        return False
    # check all permutations :(
    gnodes = tuple(g.get_nodes())
    for hnodes in allPerms(tuple(h.get_nodes())):
        if isCorrectMapping(g, h, gnodes, hnodes):
            return True
    return False

'''
maybe remove an edge and recurse?
won't work. could remove two non-corresponding edges which leads to two isomorphic graphs
'''

class Test(unittest.TestCase):
    def testIsCorrectMapping(self):
        self.assertTrue(isCorrectMapping(G1, G2, [1, 2, 3, 4], [1, 2, 4, 3]))
        self.assertFalse(isCorrectMapping(G1, G2, [1, 2, 3, 4], [1, 2, 3, 4]))
        self.assertFalse(isCorrectMapping(G1, G3, [1, 2, 3, 4], [1, 2, 3, 4]))
    def test1(self):
        self.assertTrue(areIsomorphic(G1, G2))
    def test2(self):
        self.assertTrue(areIsomorphic(G2, G1))
    def test3(self):
        self.assertFalse(areIsomorphic(G1, G3))
    def test4(self):
        self.assertFalse(areIsomorphic(G3, G1))
    def test5(self):
        self.assertFalse(areIsomorphic(G3, G2))
    def test6(self):
        G = DiGraph()
        G.add_node('a')
        self.assertFalse(areIsomorphic(G1, G))

unittest.main()
