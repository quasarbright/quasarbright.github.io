import os
from sqlalchemy import create_engine
from sqlalchemy.orm import sessionmaker
from sqlalchemy.orm.exc import NoResultFound
import mappings
from mappings import Song, Tag, Playlist


script_dir = os.path.dirname(os.path.realpath(__file__))
song_dir = os.path.join(script_dir, 'songs')

engine = create_engine('sqlite:///database.db', echo=True)
Session = sessionmaker(bind=engine)
session = Session()

def getSong(song):
    if type(song) == Song:
        return song
    return session.query(Song).filter_by(name=song).one()

def validateBangericity(bangericity):
    if not (0 <= bangericity and bangericity <= 100):
        raise ValueError('bangericity must be between 0 and 100')

def addSong(path, name, bangericity):
    '''hardlinks song to local songs folder and
    adds them to database.
    link path is ./songs/$SONG_NAME.mp3'''
    validateBangericity(bangericity)
    # don't want .mp3 at the end of the name
    if name[-4:] == '.mp3':
        name = name[:-4]
    os.link(path, os.path.join(song_dir, name+'.mp3'))
    song = mappings.Song(name=name, bangericity=bangericity)
    session.add(song)
    session.commit()

def removeSong(song):
    song = getSong(song)
    os.remove(os.path.join(song_dir, song.name+'.mp3'))
    session.delete(song)
    session.commit()


def getTag(tag):
    if type(tag) == Tag:
        return tag
    return session.query(Tag).filter_by(name=tag).one()

def addTag(name):
    tag = Tag(name=name)
    session.add(tag)
    session.commit()

def removeTag(tag):
    tag = getTag(tag)
    session.delete(tag)
    session.commit()

def getPlaylist(playlist):
    if type(playlist) == Playlist:
        return playlist
    return session.query(Playlist).filter_by(name=playlist).one()

def addPlaylist(name, includedTags=[], excludedTags=[], minBangericity=0, maxBangericity=100, andLogic=True):
    includes = [getTag(tag) for tag in includedTags]
    excludes = [getTag(tag) for tag in excludedTags]
    playlist = Playlist(
        name=name,
        includedTags=includes,
        excludedTags=excludes,
        minBangericity=minBangericity,
        maxBangericity=maxBangericity,
        andLogic=andLogic
    )
    session.add(playlist)
    session.commit()

def removePlaylist(playlist):
    playlist = getPlaylist(playlist)
    session.delete(playlist)
    session.commit()

def applyTag(song, tag):
    song = getSong(song)
    tag = getTag(tag)
    if not tag in song.tags:
        song.tags.append(tag)
    session.commit()

def applyTags(song, *tags):
    badNames = []
    for tag in tags:
        try:
            applyTag(song, tag)
        except NoResultFound:
            if type(tag) == Tag:
                badNames.append(tag.name)
            else:
                badNames.append(tag)
    if len(badNames) > 0:
        raise ValueError('these tag names don\'t exist: {0}'.format(badNames))

def removeTagFromSong(song, tag):
    song = getSong(song)
    tag = getTag(tag)
    song.tags.remove(tag)
    session.commit()

def changeBangericity(song, bangericity):
    validateBangericity(bangericity)
    song = getSong(song)
    song.bangericity = bangericity
    session.commit()

def getAllSongs():
    return session.query(Song).all()

def getAllTags():
    return session.query(Tag).all()

def getAllPlaylists():
    return session.query(Playlist).all()
