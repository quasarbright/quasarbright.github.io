let vertices = []

function setup() {
    createCanvas(800, 600);
    for (let i = 0; i < 100; i++) {
        vertices.push(createVector(random(width), random(height)))
    }
}

function mousePressed() {
    vertices.push(createVector(mouseX, mouseY))
    loop()
}

function clearVertices(){
    vertices = []
    loop()
}

function draw() {
    background(51);

    let reached = []
    let unreached = vertices.map((e) => e) //copy

    reached.push(unreached[0])
    unreached.splice(0, 1)

    while (unreached.length > 0) {
        let record = Infinity
        let rind;
        let uind;

        for (var i = 0; i < reached.length; i++) {
            for (var j = 0; j < unreached.length; j++) {
                let v1 = reached[i]
                let v2 = unreached[j]
                let d = dist(v1.x, v1.y, v2.x, v2.y)

                if (d < record) {
                    record = d
                    rind = i
                    uind = j
                }
            }
        }
        stroke(255)
        strokeWeight(2)
        line(reached[rind].x,reached[rind].y,unreached[uind].x,unreached[uind].y)
        reached.push(unreached[uind])
        unreached.splice(uind,1)
    }

    for (v of vertices) {
        stroke(255)
        strokeWeight(10)
        point(v.x, v.y)
    }
    stroke(255,0,0)
    strokeWeight(10)
    point(vertices[0].x, vertices[0].y)
    noLoop()
}
