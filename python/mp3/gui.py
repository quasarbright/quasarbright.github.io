import os
import ntpath
from appJar import gui
import DBhandling as db
import mediaHandling as media

script_dir = os.path.dirname(os.path.realpath(__file__))
app = gui('main menu', '600x500', showIcon=True)
media.initialize()
media.queuePlaylist(db.getPlaylist('test'))# testing


def stopFunction():
    # stop the infinite loop in media
    media.stopLoop()
    return True
app.setStopFunction(stopFunction)

app.setIcon(os.path.join(script_dir, 'icon.gif'))


def songPress(button):
    if button == 'add song':
        path = app.openBox('select a song to add', fileTypes=[('Audio', '*.mp3')])
        name = ntpath.basename(path)
        app.infoBox('test', path+'\n'+name)
        bangericity = 0.0
        while True:
            bangericity = app.floatBox('bangericity input', 'enter a bangericity from 0 to 100')
            if 0 <= bangericity and bangericity <= 100:
                break
            app.errorBox('invalid bangericity box', 'Error: bangericity must be between 0 and 100')
        db.addSong(path, name, bangericity)
        updateSongTable()

with app.tabbedFrame('tabs'):
    with app.tab('songs'):
        songs = db.getAllSongs()
        tableArr = [[song.name, song.bangericity] for song in songs]
        app.addTable('song table', [['Name','Bangericity']]+tableArr, colspan=3)
        #buttons
        app.addButton('add song', songPress)
    with app.tab('playlists'):
        playlists = db.getAllPlaylists()
        tableArr = [[playlist.name] for playlist in playlists]
        app.addTable('playlist table', [['Name']]+tableArr, colspan=3)
    with app.tab('tags'):
        tags = db.getAllTags()
        tableArr = [[tag.name] for tag in tags]
        app.addTable('tag table', [['Name']]+tableArr, colspan=3)

def updateSongTable():
    songs = db.getAllSongs()
    tableArr = [[song.name, song.bangericity] for song in songs]
    app.replaceAllTableRows('song table', [['Name','Bangericity']]+tableArr)

def updatePlaylistTable():
    playlists = db.getAllPlaylists()
    tableArr = [[playlist.name] for playlist in playlists]
    app.replaceAllTableRows('playlist table', [['Name']]+tableArr)

def updateTagTable():
    tag = db.getAllTags()
    tableArr = [[tag.name] for tag in tags]
    app.replaceAllTableRows('tag table', [['Name']]+tableArr)


def mediaPress(button):
    if button == 'previous':
        media.previousSong()
    elif button == 'pause':
        media.pause()
    elif button == 'play':
        media.play()
    elif button == 'next':
        media.nextSong()

with app.frame('bottom', row=3):
    app.addScale('slider', row=1)
    app.addButtons(['previous', 'pause', 'play', 'next'], mediaPress)

app.go()
