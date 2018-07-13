'''
recursively displays file tree with lineTree
'''
import sys
import os
import glob
from displayTree import lineTree

path = os.getcwd()
if len(sys.argv) > 1:
    path = sys.argv[1]
def walk(path):
    entries = os.scandir(path)
    tree = {}
    for entry in entries:
        if entry.is_file():
            tree[entry.name] = None
        else:
            tree[entry.name] = walk(path+'\\'+entry.name)
    return tree
print(lineTree(walk(path)))
