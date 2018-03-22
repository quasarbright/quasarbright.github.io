from my_libraries.lexicographicOrder import *
from my_libraries.graph_theory.MyGraph import *
import random,math

def randind(arr):
    return math.floor(random.random()*len(arr))
def getRelNeighbors(G,node,rel):
    ans = set([])
    for n in G.get_neighbors(node):
        if G[node]['to'][n]['rel'] is rel:
            ans.add(n)
    return ans

players = [
    'mike',
    'bobby',
    'eubihn',
    'feit',
    'austin',
    'kibby',
    'jonah',
    'sean'
]
numTeams = 2

G = Graph()
G.add_node(*players)
G.set_edge('mike','feit',{'rel':'enemy'})
G.set_edge('mike','bobby',{'rel':'friend'})
G.set_edge('austin','jonah',{'rel':'friend'})
G.set_edge('eubihn','jonah',{'rel':'enemy'})
G.set_edge('feit','kibby',{'rel':'enemy'})

for a in G.get_nodes():
    for b in G.get_nodes():
        if b not in G.get_neighbors(a) and a is not b:
            G.set_edge(a,b,{'rel':'neutral'})

def check(guess):
    #check enemies
    for team in guess:
        for a in team:
            for b in team:
                if a is b:
                    continue
                else:
                    if G[a]['to'][b]['rel'] is 'enemy':
                        return False
    #check friends
    for team in guess:
        for a in team:
            friends = getRelNeighbors(G,a,'friend')
            if not friends <= set(team):
                return False
    return True

def guess(perm):
    teams = []
    players = perm[:]
    while len(players) > 0:
        team = []
        while len(team)< math.floor(len(G.get_nodes())/numTeams) and len(players)>0:
            team.append(players.pop(0))
        teams.append(team)
    return teams

players = list(G.get_nodes())
for perm in allPerms(players):
    perm = list(perm)
#     print(guess(perm))
    if check(guess(perm)):
        print(perm,guess(perm))
        break
