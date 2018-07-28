import os
import random
import DBhandling as db
from playlistLogic import buildPlaylist
from pyglet.media import load, Player

script_dir = os.path.dirname(os.path.realpath(__file__))
song_dir = os.path.join(script_dir, 'songs')
player = Player()

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

play = player.play
pause = player.pause
seek = player.seek
next = player.next_source()
