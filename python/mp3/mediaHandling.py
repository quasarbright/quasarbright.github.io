import os
import random
import threading
import DBhandling as db
from playlistLogic import buildPlaylist
from pyglet.media import load, Player

script_dir = os.path.dirname(os.path.realpath(__file__))
song_dir = os.path.join(script_dir, 'songs')

player = Player()
songQueue = []
songIndex = 0
previousSource = player.source

def play():
    player.play()

def pause():
    player.pause()
def seek(time):
    player.source.seek(time)


def seekRatio(x):
    '''expects x from 0 to 1'''
    seek(x*player.source.duration)

def getQueue():
    '''returns songs that are being played or waiting to be played'''
    return songQueue[songIndex:]

def getPlaying():
    '''returns source that is playing or False if there is none'''
    if len(songQueue) != 0:
        return songQueue[songIndex]
    return False

def update():
    # check for eos
    global player, previousSource, on_eos
    if player.source != previousSource:
        on_eos()
    previousSource = player.source
def on_eos():
    print('sdfsdf')


shouldLoop = True
def loop():
    # put stuff to be executed every second here
    global shouldLoop
    global update
    update()
    if shouldLoop:
        threading.Timer(1, loop).start()

def stop():
    global shouldLoop
    shouldLoop = False

'''
you don't want queue to work like it currently does.
features you need to make:
- view the queue
- edit the queue
- previous song
'''

def songPath(song):
    song = db.getSong(song)
    return os.path.join(song_dir, song.name+'.mp3')

def queuePlaylist(playlist, shuffle=False):
    playlist = db.getPlaylist(playlist)
    songs = buildPlaylist(playlist)
    if shuffle:
        random.shuffle(songs)
    for song in songs:
        source = load(songPath(song))
        player.queue(source)


if __name__ == '__main__':
    loop()
    queuePlaylist(db.getPlaylist('test'))
    play()
    player.volume = .5
    seekRatio(.97)
# play = player.play
# pause = player.pause
# seek = player.seek
# next = player.next_source()


# ### testing eos ###
# queuePlaylist(db.getPlaylist('test'))
# player.play()
# player.seekRatio(.97)
#
# # e=input()
