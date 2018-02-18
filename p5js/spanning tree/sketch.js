const vertices = [] //"unplaced" vertices
const indices = [] //vertices that are in the final graph
const edges = [] //[[v1,v2],...] list of vector pairs
const numberOfVertices = 8
let startVertex;

function setup() {
    createCanvas(400, 400);
    for (let i = 0; i < numberOfVertices; i++) {
        v = {
            pos: createVector(random(width), random(height)),
            value: Infinity
        }
        if (i === 0) v.value = 0
        vertices.push(v)
    }
}

function iterate() {
    if (vertices.length === 0) return null //exit the function
    let minIndex = 0; //index of the minimum cost startVertex
    for (let i = 0; i < vertices.length; i++) {
        if (vertices[i].value < vertices[minIndex].value) minIndex = i
    }

}

function cost(v1, v2) {
    return v1.copy().sub(v2).mag()
}

function draw() {
    background(51);
    for (let v of vertices) {
        stroke(255, 0, 0)
        strokeWeight(10)
        point(v.pos.x, v.pos.y)
    }
}
