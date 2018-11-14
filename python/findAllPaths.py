from mylib.graph import *
import unittest
G = DiGraph()
G.add_node('a','b','c','d','e','f','g')
G.set_edge('a','b')
G.set_edge('a','e')
G.set_edge('b','e')
G.set_edge('b','f')
G.set_edge('c','d')
G.set_edge('e','c')
G.set_edge('e','f')
G.set_edge('e','a')
G.set_edge('f','d')
G.set_edge('f','g')

# def findAllPaths(g, u, v):
#     def findAllPathsUtil(g, u, v, visited):
#         if u == v:
#             return [[u]], visited
#         else:
#             newVisited = set(visited)
#             newVisited.add(u)
#             accumulatedVisited = set(newVisited)
#             ans = []
#             for child in g.get_children(u):
#                 if child not in accumulatedVisited:
#                     paths, newAccumulatedVisited = findAllPathsUtil(g, child, v, accumulatedVisited)
#                     accumulatedVisited = newAccumulatedVisited
#                     # the important part is that you update accumulatedVisited before checking the next child
#                     ans += [[u]+path for path in paths]
#             return ans, accumulatedVisited
#     return findAllPathsUtil(g, u, v, set([]))[0]

def findAllPaths(g, u, v):
    def findAllPathsUtil(g, u, v, pathSoFar):
        if u == v:
            return [[v]]
        else:
            newPathSoFar = set(pathSoFar)
            newPathSoFar.add(u)
            ans = []
            for child in g.get_children(u):
                if child not in newPathSoFar:
                    paths = findAllPathsUtil(g, child, v, newPathSoFar)
                    ans += [[u]+path for path in paths]
            return ans
    return findAllPathsUtil(g, u, v, set([]))

class TestFindAllPaths(unittest.TestCase):
    def seteq(self, a, b):
        for x in a:
            if x not in b:
                self.assertTrue(x in b)
        for y in b:
            if y not in a:
                self.assertTrue(y in a)
        self.assertTrue(True)
    def testSelf(self):
        self.assertEqual(findAllPaths(G, 'c', 'c'), [['c']])
    def testNoPath(self):
        self.assertEqual(findAllPaths(G, 'c', 'g'), [])
    def testGoodPaths(self):
        self.seteq(findAllPaths(G, 'a', 'b'), [['a', 'b']])
        self.seteq(findAllPaths(G, 'e', 'g'), [['e','f','g'], ['e','a','b','f','g']])
        self.seteq(findAllPaths(G, 'b', 'g'), [['b','e','f','g'], ['b','f','g']])
        self.seteq(findAllPaths(G, 'a', 'g'), [['a','b','e','f','g'], ['a','b','f','g'], ['a','e','f','g']])
unittest.main()
'''
(define G1
 (make-graph '(A B C D E F G)
             (λ (n)
               (cond [(symbol=? n 'A) '(B E)]
                     [(symbol=? n 'B) '(E F)]
                     [(symbol=? n 'C) '(D)]
                     [(symbol=? n 'D) '()]
                     [(symbol=? n 'E) '(C F A)]
                     [(symbol=? n 'F) '(D G)]
                     [(symbol=? n 'G) '()]))))
(find-all-paths G1 'C 'C) -> '((C)) ; src = dest
(find-all-paths G1 'C 'G) -> '() ; no paths from 'C to 'G
(find-all-paths G1 'A 'B) -> '((A B))
(find-all-paths G1 'E 'G) -> '((E F G)  (E A B F G))
(find-all-paths G1 'B 'G) -> '((B E F G)  (B F G))
(find-all-paths G1 'A 'G) -> '((A B E F G) (A B F G) (A E F G))
'''

'''
maybe return a pair with the paths and the visited nodes so you can keep track of visited and do a map-ish foldr
3 colors: seen, done, toVisit
I think what you want to accumulate is actually the path so far, not what you simply have visited.
Because you still want to look at what you have already visited, as long as it's not already on the path. This allows you to find paths starting with f,
finish that, then look at paths starting with b that can possibly have f in them, but not twice. It's not just a normal DFS with Visited and Not Visited,
since you work with other nodes when you visit a node, you don't just print the node or something. You want to see things twice, but no cycles.

It doesn't recurse nicely since when you look at the answer for a child, it might contain the answer for the parent, which means infinite recursion
If you do it with this accumulator, I think it's equivalent to removing the parent from the graph and passing the child call that graph.
'''
