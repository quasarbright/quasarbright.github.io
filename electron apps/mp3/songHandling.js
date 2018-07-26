const app = require('electron').remote.app
const storage = require('electron-storage')
const fs = require('fs')
let songs, playlists
let songsPath = app.getAppPath()+'/data/songs.json'
let playlistsPath = app.getAppPath()+'/data/playlists.json'


function getSongs(callback){
  if(typeof(callback) !== 'function'){
    throw 'getSongs(callback) requires a callback like f(songs)'
  }
  fs.readFile(songsPath, 'utf-8', function(err, data){
    if(err) throw err
    songs = JSON.parse(data)
    if(!songs){
      songs = []
    }
    callback(songs)
  })
}


function getPlaylists(callback){
  if(typeof(callback) !== 'function'){
    throw 'getPlaylists(callback) requires a callback like f(playlists)'
  }
  fs.readFile(playlistsPath, 'utf-8', function(err, data){
    if(err) throw err
    playlists = JSON.parse(data)
    if(!playlists){
      playlists = []
    }
    callback(playlists)
  })
}


function getTags(callback){
  let tags = new Set([])
  getSongs(function(songs){
    for(let song of songs){
      for(let tag of song.tags){
        tags.add(tag)
      }
    }
    callback(tags)
  })
}


function writeSongs(songs){
  if(songs === undefined){
    throw 'writeSongs(songs) requires an array of song objects'
  }
  fs.writeFile(songsPath, JSON.stringify(songs), err => {
    if(err) throw err
  })
}

function writePlaylists(playlists){
  if(playlists === undefined){
    throw 'writePlaylists(playlists) requires an array of playlists'
  }
  fs.writeFile(playlistsPath, JSON.stringify(playlists), err => {
    if(err) throw err
  })
}


function addSong(song){
  getSongs((songs) => {
    //check song name duplicate
    for(let s of songs){
      if(s.name == song.name){
        alert('cannot add due to duplicate song name')
        return undefined
      }
    }
    songs.push(song)
    writeSongs(songs)
  })
}

function addPlaylist(playlist){
  getPlaylists(function(playlists){
    //check for playlist name duplicate
    for(let p of playlists){
      if(playlist.name === p.name){
        alert('cannot add due to duplicate playlist name')
        return undefined
      }
    }
    playlists.push(playlist)
    writePlaylists(playlists)
  })
}


Set.prototype.intersection = function(other){
  let ans = new Set([])
  for(let e of this){
    if(other.has(e)){
      ans.add(e)
    }
  }
  return ans
}


Set.prototype.equals = function(other){
  if(this.size !== other.size){
    return false
  }
  for(let e of this){
    if(!other.has(e)){
      return false
    }
  }
  return true
}


//inclusive
function isBetween(x, a, b){
  return a <= x && x<= b
}


/*
 * songs: [{name:'',tags:[],bangericity:1}, ...]
 * key: {includedTags:[''],excludedTags:[''],minBangericity:1,maxBangericity:100,includeLogic:'and',}
 * excludes override includes, must meet tag requirements AND bangericity requirements to be in playlist
 * includeLogic is and by default
 */
function makePlaylistArr(songs, key) {
    let playlist = []

    //initialize key to defaults
    if(key.includeLogic === undefined){
      key.includeLogic = 'and'
    }
    if(key.includedTags === undefined){
      key.includedTags = []
    }
    if(key.excludedTags === undefined){
      key.excludedTags = []
    }
    if(key.minBangericity === undefined){
      key.minBangericity = 0
    }
    if(key.maxBangericity === undefined){
      key.maxBangericity = 100
    }
    //use sets
    let includedTags = new Set(key.includedTags)
    let excludedTags = new Set(key.excludedTags)
    for (let song of songs) {
      let tags = new Set(song.tags)
      if(key.includeLogic === 'and'){
        if (
            tags.intersection(excludedTags).equals(new Set([]))
            && tags.intersection(includedTags).equals(includedTags)
            && isBetween(song.bangericity, key.minBangericity, key.maxBangericity)
          ){
            playlist.push(song)
        }
      } else if(key.includeLogic === 'or'){
        console.log('or')//debug
        if (
            tags.intersection(excludedTags).equals(new Set([]))
            && !tags.intersection(includedTags).equals(new Set([]))
            && isBetween(song.bangericity, key.minBangericity, key.maxBangericity)
          ){
            playlist.push(song)
        }
      }

    }
    return playlist
}


function makePlaylist(name, songs, key){
  let playlistSongs = makePlaylistArr(songs, key)
  //this contains song objects
  let playlist = {name:name,songs:[], key:key}
  for(let song of playlistSongs){
    playlist.songs.push(song.name)
  }
  // return playlist
  addPlaylist(playlist)
}


function test(songs){
  console.log(makePlaylistArr(songs, {
      includedTags: ['banger', 'osu'],
      excludedTags: ['osu'],
      // minBangericity: 90,
      // maxBangericity: 100,
      includeLogic:'or'
  }));
}
