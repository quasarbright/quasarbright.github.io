import os
from shutil import copyfile
from mutagen.mp3 import MP3

script_dir = os.path.dirname(os.path.realpath(__file__))
song_dir = os.path.join(script_dir, 'songs')

def removeAllMetadata(songPath):
    audiofile = MP3(songPath)
    audiofile.delete()
    audiofile.save()

def addSong(songPath, name):
    if name.endswith('.mp3'):
        name = name[:-4]
    newPath = os.path.join(song_dir, name) + '.mp3'
    copyfile(songPath, newPath)
    removeAllMetadata(newPath)
