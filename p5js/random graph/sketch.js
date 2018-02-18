const nodes = []

function setup() {
    createCanvas(400, 400);
    for (var i = 0; i < 6; i++) {
        const node = {
            pos: createVector(random(0, width), random(0, height)),
            reached: false,
            get x() {
                return this.pos.x
            },
            get y() {
                return this.pos.y
            },
            children: []
        }
        nodes.push(node)
    }
    graph = new Graph(nodes)
    stroke(255)
    strokeWeight(5)
}

function draw() {
    background(51);
    // for (let node of nodes) {
    //     point(node.x, node.y)
    // }
    graph.show()
}

class Graph {
    constructor(vertices) {
        this.connectionMatrix = []
        this.vertices = vertices
        for (let i = 0; i < vertices.length; i++) {
            const row = []
            const left = vertices[i]
            for (let j = 0; j < vertices.length; j++) {
                if (i === j) row.push(0)
                else if(random()<.5){
                    const right = vertices[j]
                    const r = left.pos.copy().sub(right.pos).mag()
                    row.push(r)
                }
            }
            this.connectionMatrix.push(row)
        }
    }
    setConnection() {
        ;
    }
    show() {
        for (let i = 0; i < this.connectionMatrix.length; i++) {
            for (let j = 0; j < this.connectionMatrix.length; j++) {
                if (this.connectionMatrix[i][j]) {
                    const left = this.vertices[i]
                    const right = this.vertices[j]
                    stroke(255)
                    strokeWeight(2)
                    line(left.x,left.y,right.x,right.y)
                }
            }
        }
        for (vertex of this.vertices) {
            strokeWeight(10)
            stroke(255,0,0)
            point(vertex.x, vertex.y)
        }
    }
}
