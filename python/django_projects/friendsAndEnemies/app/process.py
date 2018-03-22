from .lexicographicOrder import *
from .MyGraph import *
import random
import math
import re


def randind(arr):
    return math.floor(random.random() * len(arr))


def getRelNeighbors(G, node, rel):
    ans = set([])
    for n in G.get_neighbors(node):
        if G[node]['to'][n]['rel'] == rel:
            ans.add(n)
    return ans


def parse(players, enemies, friends):
    players = re.sub('\r','',players)
    friends = re.sub('\r','',friends)
    enemies = re.sub('\r','',enemies)
    players = players.split(', ')
    G = Graph()
    G.add_node(*players)
    friends = friends.split('\n')
    friends = map(lambda e:e.split(', '),friends)
    for pair in friends:
        G.set_edge(pair[0],pair[1],{'rel':'friend'})
    enemies = enemies.split('\n')
    enemies = map(lambda e:e.split(', '),enemies)
    for pair in enemies:
        G.set_edge(pair[0],pair[1],{'rel':'enemy'})
    return G

def generate_teams(players, enemies, friends):
    G = parse(players, enemies, friends)
    numTeams = 2
    for a in G.get_nodes():
        for b in G.get_nodes():
            if b not in G.get_neighbors(a) and a != b:
                G.set_edge(a, b, {'rel': 'neutral'})

    def check(guess):
        # check enemies
        for team in guess:
            for a in team:
                for b in team:
                    if a == b:
                        continue
                    else:
                        if G[a]['to'][b]['rel'] == 'enemy':
                            return False
        # check friends
        for team in guess:
            for a in team:
                friends = getRelNeighbors(G, a, 'friend')
                if not friends <= set(team):
                    return False
        return True

    def guess(perm):
        teams = []
        players = perm[:]
        while len(players) > 0:
            team = []
            while len(team) < math.floor(len(G.get_nodes()) / numTeams) and len(players) > 0:
                team.append(players.pop(0))
            teams.append(team)
        return teams

    players = list(G.get_nodes())
    for perm in allPerms(players):
        perm = list(perm)
    #     print(guess(perm))
        if check(guess(perm)):
            return guess(perm)
            break


if __name__ == '__main__':
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
    G.set_edge('mike', 'feit', {'rel': 'enemy'})
    G.set_edge('mike', 'bobby', {'rel': 'friend'})
    G.set_edge('austin', 'jonah', {'rel': 'friend'})
    G.set_edge('eubihn', 'jonah', {'rel': 'enemy'})
    G.set_edge('feit', 'kibby', {'rel': 'enemy'})

    print(generate_teams(G))
