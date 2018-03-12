'''
data structure design:
{node : {
        'to':{
                node : {edge object},
                node : {edge object},
                ...
             },
        'from':{
                node : {edge object},
                node : {edge object},
                ...
               }
         },
 node : {...},
 ...
}
'''
import json
class NodeAlreadyExistsError(Exception):
    pass
class DiGraph(dict):
    def add_node(self,*nodes):
        '''Add a node to the graph. Can be of any hashable type.
        Can add multiple nodes like G.add_node(n1,n2,...).
        Raises error if node exists'''
        for node in nodes:
            if node in self.keys():
                raise NodeAlreadyExistsError(node)
            else:
                self[node] = {'to':{},'from':{}}
    def set_edge(self,a,b,obj={}):
        '''Create an edge between nodes a and b.
        Updates/overwrites edge if one exists.'''
        if isinstance(obj, (int, float)):
            obj = {'weight':obj}
        assert isinstance(obj,dict)
        try:
            #update edge
            edgeobj = {**self[a][b],**obj}
            self[a]['to'][b] = edgeobj
            self[b]['from'][a] = self[a]['to'][b]
        except KeyError as e:
            #no edge yet
            self[a]['to'][b] = obj
            self[b]['from'][a] = self[a]['to'][b]
    def get_nodes(self):
        '''returns the set of nodes'''
        return set(self.keys())
    def get_children(self,node):
        '''returns a tuple containing children of node'''
        return tuple(self[node]['to'].keys())
    def get_parents(self,node):
        '''Returns a tuple containing parents of node'''
        return tuple(self[node]['from'].keys())
    def pretty(self):
        '''returns json string with indentation'''
        return json.dumps(self, sort_keys=True, indent=2)
class Graph(DiGraph):
    def set_edge(self,a,b,obj={}):
        super().set_edge(a,b,obj)
        super().set_edge(b,a,obj)
if __name__ == "__main__":
    G = DiGraph()
    G.add_node(1,2,3,4)
    G.set_edge(1,2,12)
    G.set_edge(1,3,13)
    G.set_edge(1,4,14)
    G.set_edge(1,1,11)
    print(G)
