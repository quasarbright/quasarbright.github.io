from DBhandling import *


def buildPlaylist(playlist):
    '''returns a list of songs'''
    includedTags = playlist.includedTags
    excludedTags = playlist.excludedTags
    minBangericity = playlist.minBangericity
    maxBangericity = playlist.maxBangericity
    andLogic = playlist.andLogic
    includes = set([getTag(tag) for tag in includedTags])
    excludes = set([getTag(tag) for tag in excludedTags])
    songs = set([])

    # add wanted songs
    if includedTags == []:
        # no included tags specified
        # this means all songs are included
        songs = set(getAllSongs())
    elif andLogic is False:
        for tag in includes:
            for song in realTag.songs:
                songs.add(song)
    else:
        # andLogic is True
        firstIncludeSongs = tuple(includes)[0].songs
        # this is a list of songs containing the first included tag (arbitrary)
        # works because mathematically, firstIncludeSongs supersets wanted songs
        for song in firstIncludeSongs:
            if includes <= set(song.tags):
                # this checks that all included tags are in song.tags (subset)
                songs.add(song)

    # now remove unwanted songs
    for song in tuple(songs):
        x = song.bangericity
        if not (minBangericity <= x and x <= maxBangericity):
            songs.remove(song)
        elif not set(song.tags).isdisjoint(excludes):
            # this means there is an excluded tag in song.tags
            songs.remove(song)

    return songs
