function setup() {
    noCanvas();
    console.log(generate())
}

// create building blocks for words
// these are syllables, vowels, and 'n'
let vowels = 'aiueo'
// consonants that can be followed by all values
let fullConsonants = 'knmrpgb'
let specialSyllables = [...vowels, 'n']
let syllables = []
for(let consonant of fullConsonants){
    for (let vowel of vowels){
        syllables.push(consonant + vowel)
    }
}

// add other cases
syllables = syllables.concat([
    'sa', 'shi', 'su', 'se', 'so',
    'za', 'ji', 'zu', 'ze', 'zo',
    'ta', 'chi', 'tsu', 'te', 'to',
    'da', 'de', 'do',
    'ha', 'hi', 'fu', 'he', 'ho',
    'ya', 'yu', 'yo',
    'wa', 'wo',
    'cha', 'cho', 'sha', 'sho', 'shu'
])


// words can start with everything but n
let starters = syllables.concat([...vowels])

// make a graph representing word structure rules
let syllableGraph = {}

// initialize nodes
for(let syllable of syllables){
    syllableGraph[syllable] = []
}
for(let syllable of specialSyllables){
    syllableGraph[syllable] = []
}

// normal syllables can follow each other
for(let a of syllables){
    syllableGraph[a]=syllableGraph[a].concat(syllables)
}

// vowels can be followed by all normal syllables
for(let vowel of vowels){
    syllableGraph[vowel]=syllableGraph[vowel].concat(syllables)
}

// n can be followed by any normal syllable
syllableGraph['n'] = syllableGraph['n'].concat(syllables)
// n can follow anything
for(let syllable of syllables.concat([...vowels])){
    syllableGraph[syllable].push('n')
}

// only allow dipthongs that "sound japanese"
for(let syllable of syllables.concat(specialSyllables)){
    let lastCharacter = syllable[syllable.length-1]
    if(lastCharacter === 'a'){
        syllableGraph[syllable]=syllableGraph[syllable].concat([...'aieo'])
    }
    if(lastCharacter === 'i'){
        syllableGraph[syllable]=syllableGraph[syllable].concat([...'ieo'])
    }
    if(lastCharacter === 'u'){
        syllableGraph[syllable]=syllableGraph[syllable].concat([...'aiueo'])
    }
    if(lastCharacter === 'e'){
        syllableGraph[syllable]=syllableGraph[syllable].concat([...'io'])
    }
    if(lastCharacter === 'o'){
        syllableGraph[syllable]=syllableGraph[syllable].concat([...'aiue'])
    }
}

// now generate words using these rules

/**
 * get the next syllable given the current word
 * @param {Array.<String>} wordList list of syllables. length > 0
 * @return {String} next syllable
 */
function getNextSyllable(wordList){
    let word = wordList.join('')
    let lastSyllable = wordList[wordList.length - 1]
    // the syllables that can follow, according to the graph
    let syllableChildren = syllableGraph[lastSyllable]
    // debugger
    // only allow chains of two vowels max
    if(word.length >= 2){
        if (vowels.includes(word[word.length - 1]) && vowels.includes(word[word.length - 2])){
            // ending is 2 vowels, no more vowels allowed
            for(let vowel of vowels){
                if(syllableChildren.includes(vowel)){
                    let vowelIndex = syllableChildren.indexOf(vowel)
                    syllableChildren.splice(vowelIndex, 1)
                }
            }
        }
    }
    let nextSyllable = syllableChildren[Math.floor(Math.random() * syllableChildren.length)]
    return nextSyllable
}


/**
 * generates a "japanese sounding" word of given syllable length
 * @param {Number} length integer length of word (in syllables)
 * @return {String} the generated word
 */
function generateWord(length){
    // start the word
    let start = starters[Math.floor(Math.random() * starters.length)]
    let wordList = [start]
    while(wordList.length < length){
        wordList.push(getNextSyllable(wordList))
    }
    return wordList.join('')
}



/**
 * generate words on the page
 */
function generate(){
    let words = []
    for(let i = 0; i < 10; i++){
        words.push(generateWord(Math.floor(Math.random()*3+2)))
    }
    let wordsHTML = words.join('<br>')
    $('#out')
        .empty()
        .html(wordsHTML)
}
// console.log(generate())
