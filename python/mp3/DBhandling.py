import os
import sys
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

def getSong(name):
    return session.query(Song).filter_by(name=name).one()

def addSong(path, name, bangericity):
    '''hardlinks song to local songs folder and
    adds them to database.
    link path is ./songs/$SONG_NAME.mp3'''
    # validate bangericity
    if not (0 <= bangericity and bangericity <= 100):
        raise ValueError('bangericity must be between 0 and 100')
    # don't want .mp3 at the end of the name
    if name[-4:] == '.mp3':
        name = name[:-4]
    os.link(path, os.path.join(song_dir, name+'.mp3'))
    song = mappings.Song(name=name, bangericity=bangericity)
    session.add(song)
    session.commit()

def removeSong(name):
    song = session.query(Song).filter_by(name=name).one()
    os.remove(os.path.join(song_dir, song.name+'.mp3'))
    session.delete(song)
    session.commit()


def getTag(name):
    return session.query(Tag).filter_by(name=name).one()

def addTag(name):
    tag = Tag(name=name)
    session.add(tag)
    session.commit()

def removeTag(id):
    tag = session.query(Tag).filter_by(name=name).one()
    session.delete(tag)
    session.commit()

def getPlaylist(name):
    return session.query(Tag).filter_by(name=name).one()

def addPlaylist(name='', includedTags=[], excludedTags=[], minBangericity=0, maxBangericity=100, andLogic=True):
    playlist = Playlist(
        name=name,
        includedTags=includedTags,
        excludedTags=[],
        minBangericity=minBangericity,
        maxBangericity=maxBangericity,
        andLogic=andLogic
    )
    sessin.add(playlist)
    session.commit()

def removePlaylist(name):
    playlist = session.query(Playlist).filter_by(name=name)
    session.delete(playlist)
    session.commit()

def applyTag(songName, tagName):
    song = getSong(songName)
    song.tags.append(getTag(tagName))
    session.commit()

def applyTags(songName, *tagNames):
    badNames = []
    for tagName in tagNames:
        try:
            applyTag(songName, tagName)
        except NoResultFound:
            badNames.append(tagName)
    if len(badNames) > 0:
        raise ValueError('these tag names don\'t exist: {0}'.format(badNames))
