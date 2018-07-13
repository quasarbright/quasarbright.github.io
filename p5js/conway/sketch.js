let world = []
let rows = 15
let cols = 15
let going = false
let w, h

function setup() {
  createCanvas(800, 800);
  frameRate(1000)
  world = blankArr()
  w = width / cols
  h = height / rows
}

function draw() {
  background(51);
  if (going) iterateWorld()
  showWorld()
}
//click management
function mousePressed() {
  r = floor(map(mouseY, 0, height, 0, rows))
  c = floor(map(mouseX, 0, width, 0, cols))
  // console.log(r,c)
  try {
    world[r][c] = !world[r][c]
  } catch(e){
    console.log('r,c',r,c)
  }
}
function mouseDragged(){
  r = floor(map(mouseY, 0, height, 0, rows))
  c = floor(map(mouseX, 0, width, 0, cols))
  // console.log(r,c)
  try {
    world[r][c] = true
  } catch(e){
    console.log('r,c',r,c)
  }
}
function start() {
  going = true
}
function stop(){
  going = false
}
function clearWorld(){
  world = blankArr()
  stop()
}
//world stuff
function neighbors(r,c){
  let ans = 0
  for(let i = -1; i <= 1; i++){
    for(let j = -1; j <= 1; j++){
      if(j || i){//to prevent counting itself as a neighbor
        try{
          // if(world[r+i][c+j]) ans++
          if(world[(rows+(r+i))%rows][((cols+c+j))%cols]) ans++//boundless
        }catch(e){
          console.log(e)
        }
      }
    }
  }
  return ans
}
function blankArr(){
  let ans = []
  for (let r = 0; r < rows; r++) {
    let row = []
    for (let c = 0; c < cols; c++) {
      row.push(false)
    }
    ans.push(row)
  }
  return ans
}
function iterateWorld() {
  let next = blankArr()
  for (let r = 0; r < rows; r++) {
    for (let c = 0; c < cols; c++) {
      let val = world[r][c]
      let nextVal = false
      let n = neighbors(r,c)
      //loneliness
      if(val && n<2) nextVal = false
      //stay alive
      else if(val && (n===2 || n===3)) nextVal = true
      //overpopulation
      else if(val && n > 3) nextVal = false
      //reproduction
      else if(!val && n===3) nextVal = true
      next[r][c] = nextVal
    }
  }
  world = next.slice()
}
function showWorld() {
  for (let r = 0; r < rows; r++) {
    for (let c = 0; c < cols; c++) {
      if (world[r][c]) fill(0, 255, 0)
      else fill(0)
      stroke(255)
      rect(c * w, r * h, (c + 1) * w, (r + 1) * h)
    }
  }
}
