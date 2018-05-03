import re
count = 0
with open('kissanime bookmark table.txt','r') as f:
    lines = f.readlines()
    lines = lines[2:]# get rid of header
    for line in lines:
        line = line.split('\t')
        if line[2] == ' Watched':
            count += 1
        print(repr(line))
    print(len(lines))
print(count)

'''
'\n'
'Anime name\tLatest\tStatus\t\t\n'
'91 Days (Sub)\tCompleted\t Unwatched\t Remove\t Folder\n'
'Ao no Exorcist (Dub)\tCompleted\t Unwatched\t Remove\t Folder\n'
'Ao no Exorcist (Sub)\tCompleted\t Unwatched\t Remove\t Folder\n'
'Attack on Titan (Sub) (1080p)\tCompleted\t Unwatched\t Remove\t Folder\n'

['91 Days (Sub)', 'Completed', ' Unwatched', ' Remove', ' Folder\n']
'''
