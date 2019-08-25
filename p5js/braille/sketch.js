/*
Render an image in braille characters
Each braille character is a 4x2 grid.

In each grid space, there can be a dot or nothing. This means each braille character is like a 4x2
black and white image with bit depth of 1

This means we can render an image as a grid of braille characters with dithering

The image will be broken down into little 4x2 sections, and based on what pixels are going to be on and off,
we decide which braille character to put there

The tricky part is converting from a grid of pixels to the right braille character.
Here is how unicode does it:
Each Position in the braille character's grid is numbered like this:
1 4
2 5
3 6
7 8
The reason for the bottom two being out of order is because braille used to just be 3x2, but they
added the bottom two dots and didn't want to change the encoding for 3x2 characters.
Anyway, unicode basically counts the dots like binary with the reading order described by the numbering. This means each
character can be encoded with 8 bits. For example:
hex: 41
binary:
0100 0001
8765 4321
 7      1
This means the top left and bottom left are dots
here's the actual braille character:    ⡁
here's a full braille character (FF):   ⣿

And in unicode, each braille character is prefixed with 28, so this character would be U+2841

That's how you convert from binary to braille, but how do we go from knowing which dots we want to binary?
We go in reverse, recording each bit, building a binary string. For example:
⢦
The 8th position has a dot. 1
1
The 7th position has no dot. 0
10
the 6th position has a dot. 1
101
the 5th position has no dot. 0
1010
the 4th position has no dot. 0
1010 0
the 3rd position has a dot. 1
1010 01
the 2nd position has a dot. 1
1010 011
the 1st position has no dot. 0
1010 0110
Hex: A6
*/
let img = new Image()
let width = 80
let height = 80
let canvas
let context

// triggered on image upload
function loadFile(event) {
  // update img
  img.src = URL.createObjectURL(event.target.files[0])
  updateCanvas()
}

// called in loadFile and submit
function updateCanvas() {
  // draw resized image on fake canvas
  canvas = document.createElement('canvas')
  canvas.width = width
  canvas.height = height
  context = canvas.getContext('2d')
  // draw resized image
  context.drawImage(img, 0, 0, width, height)
}

function submit() {
  if(width > 0){
    width = parseInt($("#dotWidth").val())
  }
  if(height > 0){
    height = parseInt($("#dotHeight").val())
  }
  updateCanvas();
  main()
}

function getPixel(x, y) {
  return context.getImageData(x, y, 1, 1).data;
}

function getPixelArr() {
  // returns a 2d array of grayscale values
  // in range [0, 1)
  let ans = []
  for(let r = 0; r < height; r++){
    let row = []
    for(let c = 0; c < width; c++){
      let color = getPixel(c, r);
      let R = color[0]
      let G = color[1]
      let B = color[2]
      let avg = (R+G+B) / 3.0
      avg = avg / 256.0
      row.push(avg)
    }
    ans.push(row)
  }
  return ans
}

function dither(arr) {
  // arr is 2d list of grayscale values in range [0,1)
  function findClosestColor(color) {
    if (color < 0.5) {
      return 0
    } else {
      return 1
    }
  }
  // clone arr
  let copy = []
  for(let row of arr){
    copy.push(row.slice())
  }
  arr = copy

  let height = arr.length
  let width = arr[0].length

  for(let r = 0; r < height; r++){
    for(let c = 0; c < width; c++) {
      let oldPixel = arr[r][c]
      let newPixel = findClosestColor(oldPixel)
      arr[r][c] = newPixel
      let error = oldPixel - newPixel

      let right = c < width - 1
      let down = r < height - 1
      let left = c > 0

      if(right)
        arr[r][c+1] += error * 7.0/16
      if(right && down)
        arr[r+1][c+1] += error * 1.0/16
      if(down)
        arr[r+1][c] += error * 5.0/16
      if(left && down)
        arr[r+1][c-1] += error * 1.0/16 
    }
  }
  return arr
}

function ditheredToBraille(arr) {
  // convert dithered array to braille
  let height = arr.length
  let width = arr[0].length
  let lines = []
  for(let i = 0; i < Math.floor(height / 4); i++) {
    let r = i * 4
    let line = ""
    for(let j = 0; j < Math.floor(width / 2); j++) {
      let c = j * 2

      // determine braille character
      // read the binary string in reverse braille order
      let binaryString = ""
      binaryString += arr[r+3][c+1]
      binaryString += arr[r+3][c+0]
      binaryString += arr[r+2][c+1]
      binaryString += arr[r+1][c+1]
      binaryString += arr[r+0][c+1]
      binaryString += arr[r+2][c+0]
      binaryString += arr[r+1][c+0]
      binaryString += arr[r+0][c+0]

      let base10 = parseInt(binaryString, 2)
      let hexString = base10.toString(16)
      if (hexString.length == 1)
        hexString = "0" + hexString
      hexString = "28" + hexString
      let unicodeEncoding = parseInt(hexString, 16)
      let brailleCharacter = String.fromCharCode(unicodeEncoding)
      line += brailleCharacter
    }
    lines.push(line)
  }
  let braille = lines.join("\n")
  return braille
}

function main() {
  let arr = getPixelArr()
  let dithered = dither(arr)
  let braille = ditheredToBraille(dithered)
  // debugger
  // console.log(braille);
  $("#out").val(braille)
}
