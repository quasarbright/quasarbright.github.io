import os
import ntpath
import sys
import threading
import time
from appJar import gui
import DBhandling as db
import mediaHandling as media
import playlistLogic as pl

script_dir = os.path.dirname(os.path.realpath(__file__))
app = gui('main menu', '600x500', showIcon=True)
media.initialize()
# media.queuePlaylist(db.getPlaylist('test'))# testing


app.setIcon(os.path.join(script_dir, 'icon.ico'))
app.setIcon('icon.ico')

def stopFunction():
    # stop the infinite loop in media
    media.stopLoop()
    return True

app.setStopFunction(stopFunction)

def temporaryWindow(title, body):
    '''
    produces and opens destructible subwindow.
    - title is the title of the subwindow
    - body is a function where the contents of the window are specified (adding
    buttons, etc.)
    '''
    global isDestroying
    # prevents stopFunction - destroySubWindow infinite recursion
    isDestroying = False
    def destroy():
        global isDestroying
        if not isDestroying:
            isDestroying = True
            app.destroySubWindow(title)
            isDestroying = False
        return True
    app.startSubWindow(title, modal=True, transient=True)
    app.setStopFunction(destroy)
    body()
    app.stopSubWindow()
    app.showSubWindow(title)


def addSong():
    path = app.openBox('select a song to add', fileTypes=[('Audio', '*.mp3')])
    if path:
        name = ntpath.basename(path)
        app.infoBox('test', path+'\n'+name)
        bangericity = 0.0
        while True:
            bangericity = app.floatBox('bangericity input', 'enter a\
             bangericity from 0 to 100')
            if bangericity is None:
                # user cancelled
                break
            # validate bangericity
            if 0 <= bangericity and bangericity <= 100:
                break
            app.errorBox('invalid bangericity box', 'Error: bangericity must be\
             between 0 and 100')
        if bangericity is not None:
            try:
                db.addSong(path, name, bangericity)
            except FileExistsError:
                app.errorBox('duplicate song error box',
                    'Error: A song with this name already exists. The song will\
                     not be added.')
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

def songAction(rowNum):
    songOptionsWindow(app.getTableRow('song table', rowNum)[0])

def songOptionsWindow(song):
    song = db.getSong(song)
    title = song.name+' options'
    def optionsContent():
        def press(button):
            if button == 'play now':
                media.forcePlay(song)
            elif button == 'add to queue':
                media.queue(song)
            elif button == 'edit tags':
                # new window for editing tags
                def body1():
                    allTags = db.getAllTags()
                    songTags = song.tags
                    boolDict = {}
                    for tag in allTags:
                        if tag in songTags:
                            boolDict[tag.name] = True
                        else:
                            boolDict[tag.name] = False
                    app.properties('song tag selection', value=boolDict)
                    def editTagPress(button):
                        if button == 'submit':
                            result = app.getProperties('song tag selection')
                            newTags = []
                            for tag in result:
                                if result[tag] is True:
                                    newTags.append(db.getTag(tag))
                            db.setSongTags(song, newTags)
                            app.destroySubWindow('edit tags window')
                    app.addButton('submit', editTagPress)
                temporaryWindow('edit tags window', body1)
            elif button == 'change bangericity':
                bangericity = 0.0
                while True:
                    bangericity = app.floatBox('bangericity input', 'enter a\
                     bangericity from 0 to 100')
                    if bangericity is None:
                        # user cancelled
                        break
                    # validate bangericity
                    if 0 <= bangericity and bangericity <= 100:
                        break
                    app.errorBox('invalid bangericity box', 'Error: bangericity\
                     must be between 0 and 100')
                if bangericity is not None:
                    db.changeBangericity(song, bangericity)
                    updateSongTable()
            elif button == 'remove from library':
                confirmation = app.yesNoBox('remove song?', 'Are you sure you\
                 want to remove this song?')
                if confirmation is True:
                    db.removeSong(song)
                    updateSongTable()
                    app.destroySubWindow(title)

        app.addButtons(['play now', 'add to queue', 'edit tags',
            'change bangericity', 'remove from library'], press
        )
    temporaryWindow(title, optionsContent)

def playlistForm(playlist=None):
    '''can be used for creation and editing.
    if playlist is None, it goes to creation mode
    '''
    if playlist == 'newRow':
        playlist = None
    title = ''
    if playlist is not None:
        playlist = db.getPlaylist(playlist)
        title = 'edit playlist '+playlist.name
    else:
        title = 'create playlist'

    def formContents():
        allTags = db.getAllTags()
        includedTags = []
        excludedTags = []
        if playlist is not None:
            includedTags = playlist.includedTags
            excludedTags = playlist.excludedTags
        includedTagDict = {}
        excludedTagDict = {}
        # both going to be {tagname => boolean, ...}
        for tag in allTags:
            includedTagDict[tag.name] = tag in includedTags
            excludedTagDict[tag.name] = tag in excludedTags
        if playlist is None:
            app.addLabel('playlist name:')
            app.entry('name')

        app.properties('included tags', includedTagDict)
        app.properties('excluded tags', excludedTagDict)
        app.addLabel('minimum and maximum bangericity:')
        app.addNumericEntry('minimum bangericity')
        app.addNumericEntry('maximum bangericity')
        min = 0
        max = 100
        if playlist is not None:
            min = playlist.minBangericity
            max = playlist.maxBangericity
        app.setEntry('minimum bangericity', min)
        app.setEntry('maximum bangericity', max)

        app.addCheckBox('use AND logic on includes')
        ticked = True
        if playlist is not None:
            ticked = playlist.andLogic
        app.setCheckBox('use AND logic on includes', ticked=ticked)


        def submit(button):
            if button == 'submit':
                # bangericity
                minBangericity = float(app.getEntry('minimum bangericity'))
                maxBangericity = float(app.getEntry('maximum bangericity'))
                # validate bangericities
                if not minBangericity < maxBangericity:
                    app.errorBox('invalid bangericities', 'minimum bangericity\
                     must be less than maximum bangericity')
                    return None
                # include logic
                andLogic = app.getCheckBox('use AND logic on includes')
                # tags
                newIncludedTags = []
                newExcludedTags = []
                includedTagDict = app.getProperties('included tags')
                excludedTagDict = app.getProperties('excluded tags')
                ##### left off here investigating bug where you can't edit then add
                # also label entries
                # figure out what the new tags will be
                for tag in includedTagDict:
                    if includedTagDict[tag] is True:
                        newIncludedTags.append(db.getTag(tag))
                    if excludedTagDict[tag] is True:
                        newExcludedTags.append(db.getTag(tag))
                # name
                if playlist is None:
                    name = app.getEntry('name')
                    # validate name
                    if name == '':
                        app.errorBox('invalid name', 'playlist must have a name')
                        return None
                    existingPlaylistNames = [
                        playlist.name for playlist in db.getAllPlaylists()
                    ]
                    if name in existingPlaylistNames:
                        app.errorBox('duplicate name', 'a playlist with that\
                         name already exists')
                        return None
                    # create playlist
                    db.addPlaylist(name, newIncludedTags, newExcludedTags,
                        minBangericity, maxBangericity, andLogic
                    )
                    showPlaylistSongs(name)
                    updatePlaylistTable()
                    app.destroySubWindow(title)
                else:
                    # edit playlist
                    playlist.includedTags = newIncludedTags
                    playlist.excludedTags = newExcludedTags
                    playlist.minBangericity = minBangericity
                    playlist.maxBangericity = maxBangericity
                    playlist.andLogic = andLogic
                    showPlaylistSongs(playlist)
                    updatePlaylistTable()
                    app.destroySubWindow(title)

        app.addButton('submit', submit)

    temporaryWindow(title, formContents)
def showPlaylistSongs(playlist):
    playlist = db.getPlaylist(playlist)
    songs = list(pl.buildPlaylist(playlist))
    songNames = [song.name for song in songs]
    app.infoBox(playlist.name+' songs', '\n'.join(songNames))
def playlistAction(rowNum):
    playlistOptionsWindow(app.getTableRow('playlist table', rowNum)[0])

def playlistOptionsWindow(playlist):
    playlist = db.getPlaylist(playlist)
    title = playlist.name+' playlist options window'

    def optionsContent():
        def press(button):
            if button == 'play now':
                media.forcePlayPlaylist(playlist)
            elif button == 'add to queue':
                media.queuePlaylist(playlist)
            elif button == 'view songs':
                showPlaylistSongs(playlist)
            elif button == 'edit':
                playlistForm(playlist)
                # remember to edit bangericity and includeLogic too
            elif button == 'remove':
                confirmation = app.yesNoBox('remove playlist?',
                    'Are you sure you want to remove this playlist?')
                if confirmation is True:
                    db.removePlaylist(playlist)
                    updatePlaylistTable()
                    app.destroySubWindow(title)
        app.addButtons(['play now', 'add to queue', 'view songs', 'edit', 'remove'],
            press)
        # maybe don't do make into a tag
    temporaryWindow(title, optionsContent)

def tagAction(rowNum):
    tagOptionsWindow(app.getTableRow('tag table', rowNum)[0])

def tagOptionsWindow(tag):
    tag = db.getTag(tag)
    title = tag.name + ' tag options window'
    def press(button):
        if button == 'view songs':
            songs = tag.songs
            songNames = [song.name for song in songs]
            app.infoBox('songs', '\n'.join(songNames))
        elif button == 'remove':
            if app.yesNoBox('confirmation', 'are you sure you want to remove this tag?'):
                db.removeTag(tag)
                updateTagTable()
                app.destroySubWindow(title)
    def optionsContent():
        app.addButtons(['view songs', 'remove'], press)
    temporaryWindow(title, optionsContent)

def loop():
    global updateSlider
    while threading.main_thread().is_alive():
        updateSlider()
        updateSongLabel()
        time.sleep(1)

def addTag():
    name = app.stringBox('create a tag', 'tag name:')
    db.addTag(name)
    updateTagTable()

with app.tabbedFrame('tabs'):
    with app.tab('songs'):
        songs = db.getAllSongs()
        tableArr = [[song.name, song.bangericity] for song in songs]
        tableArr.sort()
        app.addTable('song table', [['Name','Bangericity']]+tableArr, colspan=3, addRow=addSong, showMenu=True, action=songAction)
    with app.tab('playlists'):
        playlists = db.getAllPlaylists()
        tableArr = [[playlist.name] for playlist in playlists]
        tableArr.sort()
        app.addTable('playlist table', [['Name']]+tableArr, colspan=3, showMenu=True, action=playlistAction, addRow=playlistForm)
    with app.tab('tags'):
        tags = db.getAllTags()
        tableArr = [[tag.name] for tag in tags]
        tableArr.sort()
        app.addTable('tag table', [['Name']]+tableArr, colspan=3, showMenu=True, action=tagAction, addRow=addTag)

def updateSongTable():
    songs = db.getAllSongs()
    tableArr = [[song.name, song.bangericity] for song in songs]
    tableArr.sort()
    app.replaceAllTableRows('song table', tableArr)

def updatePlaylistTable():
    playlists = db.getAllPlaylists()
    tableArr = [[playlist.name] for playlist in playlists]
    tableArr.sort()
    app.replaceAllTableRows('playlist table', tableArr)

def updateTagTable():
    tags = db.getAllTags()
    tableArr = [[tag.name] for tag in tags]
    tableArr.sort()
    app.replaceAllTableRows('tag table', tableArr)


def mediaPress(button):
    if button == 'previous':
        media.previousSong()
    elif button == 'pause':
        media.pause()
    elif button == 'play':
        media.play()
    elif button == 'next':
        media.nextSong()


def updateSongLabel():
    if media.getPlaying():
        app.setLabel('currently playing label', media.getPlaying().name)


with app.frame('bottom', row=3):
    app.addLabel('currently playing label', '')
    app.addScale('slider')
    app.setScaleIncrement('slider', 0)
    t = threading.Thread(target=loop)
    t.start()
    app.setScaleChangeFunction('slider', seek)
    app.addButtons(['previous', 'pause', 'play', 'next'], mediaPress)
    app.setButtonImage('play', os.path.join(app.icon_path, 'md-play.png'))
    app.setButtonImage('pause', os.path.join(app.icon_path, 'md-pause.png'))
    app.setButtonImage('next', os.path.join(app.icon_path, 'md-next.png'))
    app.setButtonImage('previous', os.path.join(app.icon_path, 'md-previous.png'))


app.go()
