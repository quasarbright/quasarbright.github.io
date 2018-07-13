function setup() {
    noCanvas();
    console.log(generate())
}

function draw() {
    background(51);
}

let prefixes = [
    'b',
    'c',
    'd',
    'f',
    'g',
    'h',
    'j',
    'k',
    'l',
    'm',
    'n',
    'p',
    'qu',
    'r',
    's',
    't',
    'v',
    'w',
    'x',
    'y',
    'z',
    'bl',
    'br',
    'ch',
    'cl',
    'cr',
    'dr',
    'fl',
    'fr',
    'gl',
    'gr',
    'pl',
    'pr',
    'sc',
    'sh',
    'sk',
    'sl',
    'sm',
    'sn',
    'sp',
    'st',
    'sw',
    'th',
    'tr',
    'tw',
    'wh',
    'wr',
    'sch',
    'scr',
    'shr',
    'sph',
    'spl',
    'spr',
    'squ',
    'str',
    'thr',
]
let vowels = [
    'a',
    'e',
    'i',
    'o',
    'u',
]
let suffixes = ['nch']

function generate(){
    let ginchulate1 = random(prefixes)+random(vowels)+random(suffixes)
    let ginchulate2 = random(prefixes)+random(vowels)+random(suffixes)
    $('#out')
        .empty()
        .html(`what if you take a <b>${ginchulate1}</b> and a <b>${ginchulate2}</b>?`)
}
// console.log(generate())
