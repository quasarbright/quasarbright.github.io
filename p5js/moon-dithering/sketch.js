// holds the uploaded image
let img = new Image()
// output dimensions
let width = 80
let height = 80
// holds the resized image, not displayed on screen
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
    if (color < 0.25) {
      return 0
    } else if (color > .75) {
      return 1
    } else {
      return 0.5
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

// 2D array of 0 | 0.5 | 1 -> String
function ditheredToMoon(arr) {
  let height = arr.length
  let width = arr[0].length
  let lines = []
  // skip every other row to make the image look right
  for (let r = 0; r < height - 1; r += 2) {
    let line = ""
    for (let c = 0; c < width - 1; c += 2) {
      const left = arr[r][c]
      const right = arr[r][c+1]
      line += getEmoji(left, right)
    }
    lines.push(line)
  }
  return lines.join("\n")
}

// (0 | 0.5 | 1) (0 | 0.5 | 1) -> String
function getEmoji(left, right) {
  if (left === 0) {
    if (right === 0) {
      return "🌑"
    } else if (right === 0.5) {
      return "🌒"
    } else {
      return "🌓"
    }
  } else if (left === 0.5) {
    if (right === 0 || right === 0.5) {
      // there is no way to do 0.5 0.5
      return "🌘"
    } else {
      return "🌔"
    }
  } else if (left === 1) {
    if (right === 0) {
      return "🌗"
    } else if (right === 0.5) {
      return "🌖"
    } else {
      return "🌕"
    }
  }
}

function main() {
  let arr = getPixelArr()
  let dithered = dither(arr)
  let text = ditheredToMoon(dithered)
  $("#out").val(text)
}
