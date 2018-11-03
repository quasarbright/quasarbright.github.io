import os
import sys
from mutagen.easyid3 import EasyID3
import mutagen
songDir = '/Users/mdelmonaco/Music/iTunes/iTunes Media/Music/'
if len(sys.argv) > 1:
    songDir = sys.argv[1]
def removeTitle(songPath):
    try:
        audiofile = EasyID3(songPath)
        audiofile['title'] = u''
        audiofile.save()
    except mutagen.id3._util.ID3NoHeaderError:
        print(songPath, 'failed')
        pass

for root, dirs, files in os.walk(songDir):
    if files != []:
        # there are files
        for filename in files:
            if filename[-4:] == '.mp3':
                # it's an mp3 file
                songPath = os.path.join(root, filename)
                print(songPath)
                removeTitle(songPath)
