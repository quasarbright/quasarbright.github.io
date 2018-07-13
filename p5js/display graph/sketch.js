// adj = [
//   [0,1,1,1],
//   [1,0,1,0],
//   [1,1,0,0],
//   [1,0,0,0]
// ]
let dtheta, len, rand2D, adj, nearest
let vertices = []
let sw = 10
function setup() {
  rand2D = function(size){
    let arr = []
    for(let i = 0; i < size; i++){
      let row = []
      for(let j = 0; j<= i; j++){
        if(i == j)
        row.push(0)
        else
        row.push(floor(random(2)))
      }
      arr.push(row)
    }
    for (let i = 0; i < size; i++) {
      let row = arr[i]
      for(let j = i+1; j < size; j++)
        row.push(arr[j][i])
    }
    return arr
  }
  adj = rand2D(7)
  createCanvas(400,400);
}

function draw() {
    background(51);

    let dtheta = TWO_PI / adj.length
    let len = min(width,height)/2.1
    for(let i = 0; i < adj.length ; i++){
      vertices.push(p5.Vector.fromAngle(i*dtheta-HALF_PI).mult(len).add(createVector(width/2,height/2)))
    }
    stroke(255)
    for(let i = 0; i < adj.length; i++){
      let v = vertices[i]
      strokeWeight(sw)
      point(v.x,v.y)
      strokeWeight(2)
      for(let j = 0; j < adj.length; j++){
        let w = vertices[j]
        if(adj[i][j])
          line(v.x,v.y,w.x,w.y)
      }
    }
    let p = createVector(mouseX,mouseY)
    nearest = -1//the hovered vertex's index
    let i = 0
    for(let v of vertices){
      if (v.dist(p) <= sw/2.0){
        nearest = i
        strokeWeight(2*sw)
        point(v.x,v.y)
        break
      }
      i++
    }
}
function mouseDragged(){
  if(nearest !== -1 && mouseIsPressed){
    vertices[nearest] = createVector(mouseX,mouseY)
  }
}

//not used, just for copying
function show(adj){
  /*
  display any adjacency matrix
  */
  let vertices = []
  let dtheta = TWO_PI / adj.length
  let len = min(width,height)/2.1
  for(let i = 0; i < adj.length ; i++){
    vertices.push(p5.Vector.fromAngle(i*dtheta-HALF_PI).mult(len).add(createVector(width/2,height/2)))
  }

  // push()
  // stroke(random(128,255),random(128,255),random(128,255))
  stroke(255)
  // translate(width/2,height/2)
  for(let i = 0; i < adj.length; i++){
    let v = vertices[i]
    strokeWeight(10)
    point(v.x,v.y)
    strokeWeight(2)
    for(let j = 0; j < adj.length; j++){
      let w = vertices[j]
      if(adj[i][j])
        line(v.x,v.y,w.x,w.y)
    }
  }
  // pop()
}
