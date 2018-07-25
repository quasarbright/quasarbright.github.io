const storage = require('electron-storage')
const fs = require('fs')
/*
let songs = [{
        name: 'unravel',
        tags: ['banger', 'tokyo ghoul', 'anime', 'weeb', 'opening', 'driving', 'public', 'loud', 'string', 'rock'],
        bangericity: 100
    },
    {
        name: 'spice',
        tags: ['banger', 'shokugeki no soma', 'anime', 'weeb', 'ending', 'driving', 'loud', 'pop'],
        bangericity: 85
    },
    {
        name: 'azalea',
        tags: ['banger', 'citrus', 'anime', 'weeb', 'opening', 'public', 'loud', 'string', 'rock'],
        bangericity: 95
    },
    {
        name: 'black song',
        tags: ['game', 'weeb', 'drakenier', 'drakengard 3',
            'weeb', 'ost', 'classical'
        ],
        bangericity: 70
    },
    {
        name: 'mysterious destiny',
        tags: ['game', 'bayonetta', 'weeb', 'ost', 'electronic'],
        bangericity: 80
    },
    {
      name: "[A]ddiction - Giga and Reol",
      tags: ['osu', 'banger', 'weeb', 'electronic', 'reol', 'giga', 'giga p', 'loud', 'for the uncultured'],
      bangericity: 95
    }
]
*/
// storage.set('songs.json', songs, err => console.log(err))
let songs
let songsPath = __dirname+'/songs.json'


function getSongs(callback){
  if(typeof(callback) !== 'function'){
    throw 'getSongs(callback) requires a callback like f(songs)'
  }
  fs.readFile(songsPath, 'utf-8', function(err, data){
    if(err) throw err
    songs = JSON.parse(data)
    callback(songs)
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


function addSong(song){
  getSongs((songs) => {
    songs.push(song)
    writeSongs(songs)
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
function makePlaylist(songs, key) {
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

function main(songs){
  console.log(makePlaylist(songs, {
      includedTags: ['banger', 'osu'],
      excludedTags: ['osu'],
      // minBangericity: 90,
      // maxBangericity: 100,
      includeLogic:'or'
  }));
}
