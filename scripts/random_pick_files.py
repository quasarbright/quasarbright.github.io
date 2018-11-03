#!/usr/bin/python
#writes to a file specified by arg
#usage: $! percentage in_file(fullpath) out_file(fullpath)
import sys
import random
import math

if len(sys.argv) < 2:
    print('usage: {0} [percentage] [in_file] [out_file]'.format(sys.argv[0]))
    sys.exit()

percentage = float(sys.argv[1])
in_file = sys.argv[2]
out_file = sys.argv[3]

#read master file
lines = []
with open(in_file, 'r') as f:
    lines = f.readlines()
    for i in range(len(lines)):
        lines[i] = lines[i].strip('\n')
        lines[i] = lines[i].strip('\r')

num_picks = int(round(len(lines) * percentage/100)) # the number of files to be selected
selected_lines = [] # the list of paths of files which will be selected

for x in range(num_picks):
    rand_ind = int(math.floor(random.random()*len(lines)))
    selected_lines.append(lines.pop(rand_ind))

print(len(selected_lines), '/', len(lines))

outlines = []
for line in selected_lines:
    splitline = line.split('/')
    filename = splitline[-1]
    outline = '{0} {1}'.format(line, filename)
    outlines.append(outline)
with open(out_file, 'w') as f:
    f.write('\n'.join(outlines))
