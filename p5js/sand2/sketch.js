// dimension of world grid in grains
const worldWidth = 100
const worldHeight = 100
let brushRadius = 3
let playing = true

let world


// a World is a JsonMap<Index, Grain>

// an Index is a {row: number, col: number}
// where {row: 0, col: 0} is the top-left of the screen

// see grain.js for grains

// -> World
function generateWorld() {
  const world = new JsonMap()
  return world
}

function isEmpty(idx) {
  return !world.get(idx)
}
function canMoveTo(idx) {
  return isEmpty(idx) && isInBounds(idx)
}

// World -> Void
// Advance the world (mutates)
function step(world) {
  // make each grain fall if possible
  // using this as a set
  const updatedGrains = new Set()
  for (let row = worldHeight - 1; row >= 0; row--) {
    for (let col = 0; col < worldWidth; col++) {
      const idx = {row, col}
      if (isEmpty(idx)) {
        continue
      }
      const grain = world.get(idx)
      if (updatedGrains.has(grain)) {
        continue
      }
      grain.update(idx)
      updatedGrains.add(grain)
    }
  }
}

// Index -> Boolean
function isInBounds({row, col}) {
  return row >= 0 && row < worldHeight && col >= 0 && col < worldWidth
}

// Index -> Index[]
function neighboringIndices({row, col}) {
  return [
    {row: row - 1, col: col - 1},
    {row: row - 1, col: col},
    {row: row - 1, col: col + 1},
    {row: row, col: col - 1},
    {row: row, col: col + 1},
    {row: row + 1, col: col - 1},
    {row: row + 1, col: col},
    {row: row + 1, col: col + 1},
  ]
}

// the materials you can paint, in picker order. make() returns null for the eraser.
const MATERIALS = [
  {id: 'SAND', rgb: COLORS.sand, name: 'Sand', make: () => new Sand(), tip: 'falls and piles up, sinks through liquids'},
  {id: 'WATER', rgb: COLORS.water, name: 'Water', make: () => new Water(), tip: 'flows and spreads, puts out fire, cools lava into stone'},
  {id: 'ACID', rgb: COLORS.acid, name: 'Acid', make: () => new Acid(), tip: 'flows like water and eats sand, wood and stone'},
  {id: 'STONE', rgb: COLORS.stone, name: 'Stone', make: () => new Stone(), tip: 'stays where you put it; lava can melt it'},
  {id: 'WOOD', rgb: COLORS.wood, name: 'Wood', make: () => new Wood(), tip: 'stays where you put it, burns slowly'},
  {id: 'FIRE', rgb: COLORS.fireCool, name: 'Fire', make: () => new Fire(), tip: 'spreads to flammable things, dies out with nothing to burn'},
  {id: 'GASOLINE', rgb: COLORS.gasoline, name: 'Gasoline', make: () => new Gasoline(), tip: 'flows like water, catches fire fast'},
  {id: 'GUNPOWDER', rgb: COLORS.gunpowder, name: 'Gunpowder', make: () => new Gunpowder(), tip: 'piles like sand, catches fire fast'},
  {id: 'LAVA', rgb: COLORS.lavaCool, name: 'Lava', make: () => new Lava(), tip: 'thick liquid that melts stone, ignites things, and hardens into stone'},
  {id: 'ERASER', name: 'Eraser', make: () => null, tip: 'removes grains'},
]
let material = MATERIALS[0]

// ---------- rendering ----------

const BOARD_RGB = [14, 13, 18]
const canvas = document.getElementById('board')
canvas.width = worldWidth
canvas.height = worldHeight
const ctx = canvas.getContext('2d')
const image = ctx.createImageData(worldWidth, worldHeight)

function drawWorld() {
  const px = image.data
  for (let row = 0; row < worldHeight; row++) {
    for (let col = 0; col < worldWidth; col++) {
      const grain = world.get({row, col})
      const clr = grain ? grain.getColor() : BOARD_RGB
      const i = 4 * (row * worldWidth + col)
      px[i] = clr[0]
      px[i + 1] = clr[1]
      px[i + 2] = clr[2]
      px[i + 3] = 255
    }
  }
  ctx.putImageData(image, 0, 0)
}

// ---------- painting ----------

// paint a disk of the current material centered at idx (mutates)
function addGrain(idx) {
  for (let dr = -brushRadius; dr <= brushRadius; dr++) {
    for (let dc = -brushRadius; dc <= brushRadius; dc++) {
      const row = idx.row + dr
      const col = idx.col + dc
      if (dr*dr + dc*dc > brushRadius * brushRadius || !isInBounds({row, col})) {
        continue
      }
      const grain = material.make()
      if (grain) {
        world.set({row, col}, grain)
      } else {
        world.delete({row, col})
      }
    }
  }
}

// pointer position in grid coordinates (fractional), or null when off the board
let pointer = null
let lastPainted = null
let pointerDown = false

function toGrid(e) {
  const r = canvas.getBoundingClientRect()
  const col = (e.clientX - r.left) / r.width * worldWidth
  const row = (e.clientY - r.top) / r.height * worldHeight
  if (col < 0 || row < 0 || col >= worldWidth || row >= worldHeight) return null
  return {row, col}
}

canvas.addEventListener('pointerdown', (e) => {
  if (e.button !== 0) return
  e.preventDefault()
  canvas.setPointerCapture(e.pointerId)
  pointerDown = true
  pointer = toGrid(e)
  lastPainted = null
  hideHint()
})
canvas.addEventListener('pointermove', (e) => {
  if (pointerDown) pointer = toGrid(e)
  moveRing(e)
})
canvas.addEventListener('pointerleave', () => { ringEl.hidden = true })

// outline of the brush under a mouse cursor, so you can see how much one dab covers
const ringEl = document.getElementById('brush-ring')
function moveRing(e) {
  const g = toGrid(e)
  if (e.pointerType === 'touch' || !g) {
    ringEl.hidden = true
    return
  }
  const r = canvas.getBoundingClientRect()
  const cell = r.width / worldWidth
  const size = (2 * brushRadius + 1) * cell
  ringEl.hidden = false
  ringEl.style.width = ringEl.style.height = size + 'px'
  ringEl.style.left = (Math.floor(g.col) + 0.5) * cell + 'px'
  ringEl.style.top = (Math.floor(g.row) + 0.5) * cell + 'px'
}
for (const type of ['pointerup', 'pointercancel']) {
  canvas.addEventListener(type, () => {
    pointerDown = false
    pointer = null
    lastPainted = null
  })
}

// paint from where the stroke was last frame to where it is now, so fast strokes don't leave gaps.
// Holding still keeps pouring, one dab per frame.
function paintStroke() {
  if (!pointerDown || !pointer) {
    lastPainted = null
    return
  }
  const from = lastPainted ?? pointer
  const dist = Math.hypot(pointer.row - from.row, pointer.col - from.col)
  const n = Math.max(1, Math.ceil(dist / Math.max(1, brushRadius * 0.75)))
  for (let i = 1; i <= n; i++) {
    const t = i / n
    addGrain({
      row: Math.floor(from.row + (pointer.row - from.row) * t),
      col: Math.floor(from.col + (pointer.col - from.col) * t),
    })
  }
  lastPainted = pointer
}

// ---------- loop ----------

// the sim runs at a fixed 60 steps per second, independent of the display's refresh rate
const STEPS_PER_SECOND = 60
let stepAccumulator = 0
let lastTime = performance.now()

function frame(now) {
  const dt = Math.min(0.1, (now - lastTime) / 1000)
  lastTime = now
  stepAccumulator += dt * STEPS_PER_SECOND
  if (stepAccumulator > 3) stepAccumulator = 3
  let steps = 0
  while (stepAccumulator >= 1) {
    stepAccumulator -= 1
    if (playing) step(world)
    paintStroke()
    steps++
  }
  if (steps > 0) drawWorld()
  requestAnimationFrame(frame)
}

world = generateWorld()
drawWorld()
requestAnimationFrame(frame)

function reset() {
  world = generateWorld()
  drawWorld()
}

function togglePlayPause() {
  playing = !playing
  syncPlayButton()
}

function next() {
  step(world)
  drawWorld()
}

// ---------- UI ----------

const pickerEl = document.getElementById('picker')
const hintEl = document.getElementById('hint')
const playBtn = document.getElementById('play')
const stepBtn = document.getElementById('step')
const brushEl = document.getElementById('brush')
const brushValueEl = document.getElementById('brush-value')

function hideHint() {
  if (hintEl) hintEl.hidden = true
}

function syncPlayButton() {
  playBtn.classList.toggle('is-paused', !playing)
  playBtn.title = playing ? 'Pause (space): freeze the simulation; you can still paint' : 'Play (space): resume the simulation'
  playBtn.setAttribute('aria-label', playing ? 'Pause' : 'Play')
  stepBtn.disabled = playing
}

// a little heap of real grains of this material, drawn on the chip
function drawSwatch(swatch, mat) {
  const w = 18, h = 11
  swatch.width = w
  swatch.height = h
  const sctx = swatch.getContext('2d')
  const img = sctx.createImageData(w, h)
  for (let y = 0; y < h; y++) {
    for (let x = 0; x < w; x++) {
      // a mound: taller in the middle
      const top = h - Math.round((h - 1) * (1 - Math.pow(Math.abs(x - (w - 1) / 2) / (w / 2), 2)))
      let clr = null
      if (y >= top) {
        const grain = mat.make()
        if (grain) {
          // burning things show a spread of ages, hottest at the bottom
          if ('remainingLifespan' in grain) {
            grain.remainingLifespan *= Math.min(1, 0.15 + (y - top) / (h - top) + Math.random() * 0.3)
          }
          clr = grain.getColor()
        }
      }
      const i = 4 * (y * w + x)
      if (clr) {
        img.data[i] = clr[0]
        img.data[i + 1] = clr[1]
        img.data[i + 2] = clr[2]
        img.data[i + 3] = 255
      }
    }
  }
  sctx.putImageData(img, 0, 0)
}

function selectMaterial(mat) {
  material = mat
  for (const chip of pickerEl.children) {
    const on = chip.dataset.id === mat.id
    chip.classList.toggle('is-active', on)
    chip.setAttribute('aria-pressed', on)
  }
}

MATERIALS.forEach((mat, i) => {
  const key = String((i + 1) % 10)
  const chip = document.createElement('button')
  chip.className = 'mat-chip'
  chip.dataset.id = mat.id
  if (mat.rgb) chip.style.setProperty('--mat', `rgb(${mat.rgb.join(',')})`)
  chip.title = `${mat.name} (${key}): ${mat.tip}`
  const swatch = document.createElement('canvas')
  swatch.className = 'mat-swatch'
  if (mat.id === 'ERASER') swatch.classList.add('is-eraser')
  else drawSwatch(swatch, mat)
  const label = document.createElement('span')
  label.textContent = mat.name
  chip.append(swatch, label)
  chip.addEventListener('click', () => selectMaterial(mat))
  pickerEl.appendChild(chip)
  mat.key = key
})
selectMaterial(material)

playBtn.addEventListener('click', togglePlayPause)
stepBtn.addEventListener('click', next)
document.getElementById('clear').addEventListener('click', reset)

function syncBrush() {
  brushRadius = Number(brushEl.value)
  brushValueEl.textContent = brushRadius
}
brushEl.addEventListener('input', syncBrush)
syncBrush()
syncPlayButton()

document.addEventListener('keydown', (e) => {
  if (e.metaKey || e.ctrlKey || e.altKey) return
  if (e.target instanceof HTMLInputElement && e.target.type !== 'range') return
  const mat = MATERIALS.find((m) => m.key === e.key)
  if (mat) {
    selectMaterial(mat)
  } else if (e.key === ' ') {
    e.preventDefault()
    togglePlayPause()
  } else if ((e.key === '.' || e.key === 'ArrowRight') && !playing) {
    next()
  } else if (e.key === '[' || e.key === ']') {
    brushEl.value = Number(brushEl.value) + (e.key === ']' ? 1 : -1)
    syncBrush()
  }
})

