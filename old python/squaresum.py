import math
def isSquare(num):
    return math.sqrt(num) == math.floor(math.sqrt(num))
vertices = range(1,16)
graph = []
# print(' ',list(vertices))
for i in vertices:
    row = []
    for j in vertices:
        if isSquare(i+j) and i != j:
            row.append(1)
        else:
            row.append(0)
    # print(i,row)
    graph.append(row)
