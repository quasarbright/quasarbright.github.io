// the uploaded image
let img = new Image()
// the canvas holding the original image
let uploadedCanvas
// the canvas holding the current image (resized)
let editedCanvas
// the displayed canvas (different from editedCanvas if we're in energy mode)
let displayCanvas
// INVARIANT: editedCanvas and displayCanvas should always have the same dimensions

// triggered on image upload
function loadFile(event) {
  // update img
  img.src = URL.createObjectURL(event.target.files[0])
  img.onload = () => {
    uploadedCanvas = document.createElement('canvas')
    paintImgToCanvas(img, uploadedCanvas)
    editedCanvas = document.createElement('canvas')
    paintImgToCanvas(img, editedCanvas)
    displayCanvas = document.getElementById('canvas')
    paintImgToCanvas(img, displayCanvas)
    if (isEnergyMode()) {
      setCanvasToImage(displayCanvas, getEnergyImage())
    }
  }
}

function paintImgToCanvas(img, canvas) {
  canvas.width = img.width
  canvas.height = img.height
  let context = canvas.getContext('2d')
  context.drawImage(img, 0, 0, img.width, img.height)
}

// Canvas -> [number, number, number][][]
function getCanvasImage(canvas) {
  return fromImageData(canvas.getContext('2d').getImageData(0,0,canvas.width,canvas.height))
}

// ImageData -> [number, number, number][][]
function fromImageData(imageData) {
  const image = []
  for (let r = 0; r < imageData.height; r ++) {
    const row = []
    for (let c = 0; c < imageData.width; c++) {
      const i = (r * imageData.width + c) * 4
      const red = imageData.data[i]
      const green = imageData.data[i+1]
      const blue = imageData.data[i+2]
      row.push([red,green,blue])
    }
    image.push(row)
  }
  return image
}

// Canvas [number, number, number][][] -> void
function setCanvasToImage(canvas, image) {
  canvas.height = image.length
  canvas.width = image[0].length
  const imageData = toImageData(image)
  const ctx = canvas.getContext('2d')
  ctx.clearRect(0, 0, canvas.width, canvas.height); 
  ctx.putImageData(imageData, 0, 0)
}

// [number, number, number][][] -> ImageData
function toImageData(image) {
  const width = image[0].length
  const height = image.length
  const arr = new Uint8ClampedArray(width * height * 4)
  let i = 0
  for (const row of image) {
    for (const [r,g,b] of row) {
      arr[i] = r
      arr[i+1] = g
      arr[i+2] = b
      arr[i+3] = 255
      i += 4
    }
  }
  return new ImageData(arr, width, height)
}



// -> number[][]
function getGrayscale() {
  // returns a 2d array of grayscale values
  // in range [0, 1)
  return getCanvasImage(editedCanvas).map((row) => row.map(([R,G,B]) => 0.2989 * R + 0.5870 * G + 0.1140 * B))
}

// Sobel and Scharr filter / kernel for energy
const gx = [
  [-1,0,1],
  [-2,0,2],
  [-1,0,1]
]
const gy = [
  [-1,-2,-1],
  [0,0,0],
  [1,2,1]
]

// -> [number, number, number][][]
function getEnergyImage() {
  const raw = getEnergy()
  const smoothed = raw.map(row => row.map(energy => Math.sqrt(energy)))
  let maxEnergy = -Infinity
  for (const row of smoothed) {
    maxEnergy = Math.max(maxEnergy, ...row)
  }
  const normalized = smoothed.map(row => row.map(energy => maxEnergy > 0 ? energy * 255 / maxEnergy : 0))
  const image = normalized.map(row => row.map(energy => [energy, energy, energy]))
  return image
}

// -> number[][]
function getEnergy() {
  // convolve with those kernels treating pixels beyond the image as black
  const grays = getGrayscale()
  return grays.map((row, r) => row.map((gray, c) => {
    let energyX = 0
    let energyY = 0
    for (let dr = -1; dr < 2; dr++) {
      for (let dc = -1; dc < 2; dc ++) {
        const neighborGray = grays[r + dr]?.[c + dc] ?? 0
        energyX += neighborGray * gx[1 + dr][1 + dc]
        energyY += neighborGray * gy[1 + dr][1 + dc]
      }
    }
    return Math.sqrt(energyX * energyX + energyY * energyY)
  }))
}

// -> void
// remove vertical seam of lowest energy
function removeMinimalSeam() {
  const seam = findMinimalSeam()
  return removeSeam(seam)
}

// -> {r: number, c: number}[]
function findMinimalSeam() {
  const minimalEnergyMap = findMinimalEnergyMap()
  const height = minimalEnergyMap.length
  const width = minimalEnergyMap[0].length
  const seam = []
  {
    const r = height - 1
    const children = minimalEnergyMap[r].map((_,c) => ({r,c}))
    let minimalEnergyChild = argmin(children, ({r,c}) => minimalEnergyMap[r][c])
    seam.push(minimalEnergyChild)
  }
  while (true) {
    const {r,c} = seam[seam.length - 1]
    if (r === 0) {
      break
    }
    const parents = [-1,0,1].map(dc => ({r: r-1, c: c+dc})).filter(({r,c}) => c > 0 && c < width)
    const minimalEnergyParent = argmin(parents, ({r,c}) => minimalEnergyMap[r][c])
    seam.push(minimalEnergyParent)
  }
  return seam
}

// -> number[][]
// the minimal energy of a seam from the top of the image to this pixel.
function findMinimalEnergyMap() {
  const energyMap = getEnergy()
  const height = energyMap.length
  const width = energyMap[0].length
  const minimalEnergyMap = [energyMap[0].slice()]
  for (let r = 1; r < height; r++) {
    const row = []
    for (let c = 0; c < width; c++) {
      parentEnergies = [-1,0,1].map(
        dc => minimalEnergyMap[r-1][c+dc]
      ).filter(
        energy => energy !== undefined
      )
      row.push(energyMap[r][c] + Math.min(...parentEnergies))
    }
    minimalEnergyMap.push(row)
  }
  return minimalEnergyMap
}

// any[] (any -> number) -> any | undefined
function argmin(arr, fun) {
  let min = Infinity
  let minElement = undefined
  for(const ele of arr) {
    const val = fun(ele)
    if (val < min) {
      min = val
      minElement = ele
    }
  }
  return minElement
}

// {r: number, c: number}[] -> void
function removeSeam(seam) {
  const image = getCanvasImage(editedCanvas)
  for (const {r,c} of seam) {
    image[r].splice(c,1)
  }
  setCanvasToImage(editedCanvas, image)
}

function go() {
  requestAnimationFrame(animateShowMinimalSeam)
}

function animateShowMinimalSeam(t) {
  if (getCanvasImage(editedCanvas).length > 1) {
    const seam = findMinimalSeam()
    for (const {r,c} of seam) {
      // TODO draw red pixel
      const ctx = displayCanvas.getContext('2d')
      ctx.fillStyle = 'rgba(255,0,0,1)'
      ctx.fillRect(c,r,2,2)
    }
    requestAnimationFrame(animateRemoveSeam(seam))
  }
}

function animateRemoveSeam(seam) {
  return (t) => {
    removeSeam(seam)
    setCanvasToImage(displayCanvas, isEnergyMode() ? getEnergyImage() : getCanvasImage(editedCanvas))
    requestAnimationFrame(animateShowMinimalSeam)
  }
}

function isEnergyMode() {
  return document.getElementById('energy-mode').checked
}