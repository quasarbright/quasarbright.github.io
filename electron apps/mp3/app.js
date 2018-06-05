songs = [{
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
        bangericity: 90
    }
]
// TODO: make this a json and do fs.js stuff


function arrEqual(a, b){
  for(let x of a){
    for(let y of b){
      if(x != y){
        return false
      }
    }
  }
  return true && a.length === b.length//otherwise, arrEqual(a,[]) returns true
}


function getIntersection(a, b){
  ans = []
  for(let x of a){
    for(let y of b){
      if(x == y){
        ans.push(x)
      }
    }
  }
  return ans
}


//inclusive
function isBetween(x, a, b){
  return a <= x && x<= b
}
/*
 * songs: [{name:'',tags:[],bangericity:1}]
 * key: {includedTags:[''],excludedTags:[''],minBangericity:1,maxBangericity:1}
 * excludes override includes, must meet tag requirements AND bangericity requirements to be in playlist
 */
function makePlaylist(songs, key) {
    let playlist = []

    //initialize key to defaults
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
    for (let song of songs) {
      // console.log(song.tags, key.includedTags, getIntersection(song.tags, key.includedTags))
      // console.log(song.tags, key.excludedTags, getIntersection(song.tags, key.excludedTags))
      // console.log(arrEqual(getIntersection(song.tags, key.excludedTags), []), arrEqual(getIntersection(song.tags, key.includedTags), key.includedTags), isBetween(song.bangericity, key.minBangericity, key.maxBangericity))
      if (
        arrEqual(getIntersection(song.tags, key.excludedTags), [])
        && arrEqual(getIntersection(song.tags, key.includedTags), key.includedTags)
        && isBetween(song.bangericity, key.minBangericity, key.maxBangericity)
        ){
        playlist.push(song)
      }
    }
    return playlist
}

console.log(makePlaylist(songs, {
    includedTags: ['banger'],
    excludedTags: ['ending'],
    minBangericity: 90,
    maxBangericity: 100
}));
