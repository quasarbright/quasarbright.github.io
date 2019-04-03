'''
a union find is a data structure used to keep track of a universe of items,
and disjoint subsets of them. You could use this to keep track of which nodes
in a graph belong to the same connected component, and then connect two connected
components if you want. All you can do is union (connect) subsets and see if
two elements are in the same subset (they are connected to each other).

The goal is to have this functionality with linear space and constant time for
both operations.

we will keep track of a dictionary from elements to their representatives, where
each subset has a representative. To find an element's representative, you say
e = representatives[e] until you reach a cycle, which means you found the "base"
representative. for example, let's say you have subsets [(1,2,3), (4, 5, 6)] and
representatives dictionary {1:2, 2:2, 3:2, 4:5, 5:6, 6:6}. 2 is the representative of
the first subset, and 6 is the representative of the second subset, despite the fact
that it appears that 4's representative is 5. This is why we have to "go down the
chain" until we find a cycle to find the true representative.

naively, we could connect elements a and b by finding b's true representative,
finding a's true representative, and setting a's true representative's representative
to b's true representative. For example, let's say we want to connect 1 and 4:
we'd find that 1's representative is 2 and 4's is 6. we'd say representatives[2] = 4
and now when we find 1's true representative is 6. It's important to go down the chain
before modifying the dictionary because if we just said representatives[1] = 6,
2's true representative would still be 2, and we want its true representative to be
6 since it's part of the same subset as 1, and we just unioned those subsets. That's
why you have to go down the chain.

This would work, but you could get a chain with height equal to the total number of items.
for example, with items 5, 6, 7, 8, 9 you could end up with {8: 7, 9: 8, 5: 5, 6: 5, 7: 6}
if you do
connect(9, 8)
connect(8, 7)
connect(7, 6)
connect(6, 5)

then, when you try to find if 9 and 5 are in the same subset, it'll take linear time.
What we can do to improve this is a technique called path compression. Basically,
whenever we get the true representative and go down the chain, remember the true
representative we found at the end, go back up the chain, and set the direct representative
of each element to its true representative. This operation is at worst, linear and
the next time you use the union find, it will be much faster. You will only ever
have to go down a chain once. This example still creates a linked list, but if we ask
are_connected(9, 5), we will get 9's neighbor and flatten the chain and representatives will become
{8: 5, 9: 5, 5: 5, 6: 5, 7: 5}

this makes the algorithms linear time at worst, but it approaches constant time
the more you use it. Memory also stays linear, despite time complexity decreasing
'''
class UnionFind:
    def __init__(self, universe):
        self.universe = universe
        # map from an element to its representative
        # elements in the same set have the same "ending" parent if you go down
        # the chain until an element is its own representative
        self.representatives = {e:e for e in universe}


    def get_representative(self, a):
        '''
        get the representative element of a
        by "diving down the chain"
        updates chain with final representative on the way down
        '''
        chain = []
        prev = a
        chain.append(prev)
        next = self.representatives[prev]
        while prev is not next:
            prev = next
            chain.append(prev)
            next = self.representatives[next]
        # repair representative chain to make it faster next time
        for e in chain:
            self.representatives[e] = next
        return next


    def are_connected(self, a, b):
        '''
        are a and b in the same set?
        '''
        arep = self.get_representative(a)
        brep = self.get_representative(b)
        return arep is brep


    def connect(self, a, b, done=False):
        '''
        union the sets of a and b
        '''
        brep = self.get_representative(b)
        prev = a
        next = self.representatives[prev]
        self.representatives[prev] = brep
        while prev is not next:
            prev = next
            next = self.representatives[next]
            self.representatives[prev] = brep
        if not done:
            self.connect(b, a, True)


uf = UnionFind([5, 6, 7, 8, 9])
uf.connect(9, 8)
uf.connect(8, 7)
uf.connect(7, 6)
uf.connect(6, 5)
# uf.connect(9, 5, True)
# uf.connect(5, 9)
print(uf.representatives)
# linked list
print(uf.are_connected(5, 7))
# True
print(uf.representatives)
# 7 and below have been "repaired" by getting 7's representative and now point to 5
# so unioning and finding (connect and are_connected) are "kind of" constant time access
# more specifically, access gets faster the more it is used, and it worst case linear
