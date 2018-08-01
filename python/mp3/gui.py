import os
import ntpath
import threading
import time
from appJar import gui
import DBhandling as db
import mediaHandling as media

script_dir = os.path.dirname(os.path.realpath(__file__))
app = gui('main menu', '600x500', showIcon=True)
media.initialize()
media.queuePlaylist(db.getPlaylist('test'))# testing


app.setIcon('icon.gif')

def stopFunction():
    # stop the infinite loop in media
    media.stopLoop()
    return True

app.setStopFunction(stopFunction)


def songPress(button):
    if button == 'add song':
        path = app.openBox('select a song to add', fileTypes=[('Audio', '*.mp3')])
        if path:
            name = ntpath.basename(path)
            app.infoBox('test', path+'\n'+name)
            bangericity = 0.0
            while True:
                bangericity = app.floatBox('bangericity input', 'enter a bangericity from 0 to 100')
                if bangericity is None:
                    # user cancelled
                    break
                if 0 <= bangericity and bangericity <= 100:
                    break
                app.errorBox('invalid bangericity box', 'Error: bangericity must be between 0 and 100')
            if bangericity is not None:
                try:
                    db.addSong(path, name, bangericity)
                except FileExistsError:
                    app.errorBox('duplicate song error box', 'Error: A song with this name already exists. The song will not be added.')
                updateSongTable()


realSlide = True
def seek(slider):
    '''slider callback'''
    global realSlide
    if slider == 'slider' and realSlide:
        value = app.getScale('slider') / 100
        duration = media.getDuration()
        if duration is not None:
            time = media.getDuration() * value
            media.seek(time)
            media.play()

def updateSlider():
    '''sets slider according to song progress'''
    global realSlide
    time = media.getTime()
    if time is not None:
        duration = media.getDuration()
        ratio = time / duration
        realSlide = False
        app.setScale('slider', ratio*100)
        realSlide = True

def loop():
    global updateSlider
    while threading.main_thread().is_alive():
        updateSlider()
        time.sleep(1)


with app.tabbedFrame('tabs'):
    with app.tab('songs'):
        songs = db.getAllSongs()
        tableArr = [[song.name, song.bangericity] for song in songs]
        tableArr.sort()
        app.addTable('song table', [['Name','Bangericity']]+tableArr, colspan=3)
        #buttons
        app.addButton('add song', songPress)
    with app.tab('playlists'):
        playlists = db.getAllPlaylists()
        tableArr = [[playlist.name] for playlist in playlists]
        tableArr.sort()
        app.addTable('playlist table', [['Name']]+tableArr, colspan=3)
    with app.tab('tags'):
        tags = db.getAllTags()
        tableArr = [[tag.name] for tag in tags]
        tableArr.sort()
        app.addTable('tag table', [['Name']]+tableArr, colspan=3)

def updateSongTable():
    songs = db.getAllSongs()
    tableArr = [[song.name, song.bangericity] for song in songs]
    tableArr.sort()
    app.replaceAllTableRows('song table', [['Name','Bangericity']]+tableArr)

def updatePlaylistTable():
    playlists = db.getAllPlaylists()
    tableArr = [[playlist.name] for playlist in playlists]
    tableArr.sort()
    app.replaceAllTableRows('playlist table', [['Name']]+tableArr)

def updateTagTable():
    tag = db.getAllTags()
    tableArr = [[tag.name] for tag in tags]
    tableArr.sort()
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
    app.setScaleIncrement('slider', 0)
    t = threading.Thread(target=loop)
    t.start()
    app.setScaleChangeFunction('slider', seek)
    app.addButtons(['previous', 'pause', 'play', 'next'], mediaPress)

app.go()
