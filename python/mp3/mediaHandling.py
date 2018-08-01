'''
loop() executes every second until stopLoop() is called and loop() calls
update(), which checks for song ending. If song ended, it calls on_eos(), which
handles queueing the next song.
tests to run:
- queue playlist and qutoplay works
- get to end of queue, finish song, wait, then queue another song and it should auto-play
- next and previous should work
- forcePlay should work
- playing one song by itself should work (nothing else in queue)
- one song in queue, finish, previousSong, finish, should reach end of queue
- start loop then queue and play works
- forcePlay on empty queue
'''
import os
import sys
import threading
import time
import DBhandling as db
import playlistLogic as pl
from pyglet.media import load, Player

script_dir = os.path.dirname(os.path.realpath(__file__))
song_dir = os.path.join(script_dir, 'songs')

player = Player()
songQueue = []
songIndex = 0
reachedEnd = False

def play():
    player.play()

def pause():
    player.pause()

def seek(time):
    player.source.seek(time)

def quit():
    stop()
    sys.exit()

def getTime():
    if player.source == None:
        return None
    return player.time

def getDuration():
    if player.source == None:
        return None
    return player.source.duration

def seekRatio(x):
    '''expects x from 0 to 1'''
    seek(x*player.source.duration)

def getQueue():
    '''returns songs that are being played or waiting to be played'''
    return songQueue[songIndex:]

def getPlaying():
    '''returns source that is playing or False if there is none'''
    if len(songQueue) != 0 and len(songQueue) > songIndex:
        return songQueue[songIndex]
    return False

def clearQueue():
    '''clears songs after currently playing song'''
    global songQueue, songIndex
    if len(songQueue) != 0:
        songQueue = songQueue[:songIndex + 1]

def nextSong():
    '''skips to next song and returns True, returns False and does nothing if
    there is no next song
    '''
    if songIndex != len(songQueue) - 1 and len(songQueue) != 0:
        player.next_source()
        on_eos()
        return True
    return False

def previousSong():
    '''goes to previous song or the beginning of current song if there is no
    previous song
    '''
    global songIndex, songQueue, reachedEnd
    reachedEnd = False
    if songIndex > 0:
        songIndex -= 1
    player.next_source()
    if len(songQueue) != 0:
        realQueue(songQueue[songIndex])
    play()

def queue(song):
    song = db.getSong(song)
    songQueue.append(song)
    if len(songQueue) == 1:
        realQueue(song)

def getSource(song):
    return load(songPath(song))

def realQueue(song):
    # actually queue the song on the player
    print('really queueing', song)
    player.queue(getSource(song))

def update():
    # check for eos
    global player, on_eos, reachedEnd
    if player.playing:
        reahchedEnd = False
    if player.source != None and abs(player.source.duration - player.time < .1):
        on_eos()

def on_eos():
    global songIndex, songQueue, player, load, songPath, reachedEnd
    if len(songQueue) > songIndex:
        print(songQueue[songIndex], 'ended')
    if len(songQueue) == songIndex:
        print('reached end of queue')
        reachedEnd = True
    if len(songQueue) > songIndex and not reachedEnd:
        songIndex += 1
    if len(songQueue) > songIndex:
        # if there is a current song
        reachedEnd = False
        realQueue(songQueue[songIndex])
        player.play()

def reset():
    global player, songQueue, songIndex
    player = Player()
    songQueue = []
    songIndex = 0

def getNextSong():
    if songIndex < len(songQueue) - 1:
        return songQueue[songIndex + 1]

def forcePlay(song):
    '''plays a song right now'''
    if len(songQueue) == 0:
        queue(song)
        play()
    else:
        songQueue.insert(songIndex+1, song)
        nextSong()

# looping
shouldLoop = True
def loop():
    # put stuff to be executed every second here
    global shouldLoop
    global update
    while shouldLoop and threading.main_thread().is_alive():
        update()
        time.sleep(1)

def stopLoop():
    global shouldLoop
    shouldLoop = False

def resumeLoop():
    global shouldLoop
    shouldLoop = True
    loop()

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

def queuePlaylist(playlist):
    playlist = db.getPlaylist(playlist)
    songs = pl.buildPlaylist(playlist)
    for song in songs:
        queue(song)
        # source = load(songPath(song))


def initialize():
    t = threading.Thread(target=loop)
    t.start()


if __name__ == '__main__':
    queuePlaylist(db.getPlaylist('test'))
    # queue(pl.buildPlaylist(db.getPlaylist('test')).pop())
    play()
    player.volume = .3
    # songIndex = len(songQueue) - 2
    # nextSong()
    seekRatio(.97)
    initialize()
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
