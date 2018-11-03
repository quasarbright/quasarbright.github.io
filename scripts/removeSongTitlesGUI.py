import os
import sys
import tkinter
from tkinter import filedialog
from mutagen.easyid3 import EasyID3
import mutagen

root = tkinter.Tk()
root.withdraw()
songDir = filedialog.askdirectory(parent=root,title='select song directory')
def removeTitle(songPath):
    try:
        audiofile = EasyID3(songPath)
        audiofile['title'] = u''
        audiofile.save()
    except mutagen.id3._util.ID3NoHeaderError:
        print(songPath, 'failed')
        pass
def removeTitlesInDir(songDir):
    for root, dirs, files in os.walk(songDir):
        if files != []:
            # there are files
            for filename in files:
                if filename[-4:] == '.mp3':
                    # it's an mp3 file
                    songPath = os.path.join(root, filename)
                    print(songPath)
                    removeTitle(songPath)
if songDir:
    removeTitlesInDir(songDir)
# if __name__ == "__main__":
    # removeTitlesInDir(songDir)'''
