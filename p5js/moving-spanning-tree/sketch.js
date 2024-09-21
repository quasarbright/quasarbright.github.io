let vertices = []

function setup() {
    createCanvas(800, 600);
    for (let i = 0; i < 100; i++) {
        vertices.push({pos: createVector(random(width), random(height)), vel: createVector(random(-2,2), random(-2,2))})
    }
}

function mousePressed() {
    vertices.push({pos: createVector(mouseX, mouseY), vel: createVector(random(-2,2), random(-2,2))})
}

function clearVertices(){
    vertices = []
}

function draw() {
    background(51);

    let reached = []
    let unreached = vertices.map(v => v.pos)

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
        point(v.pos.x, v.pos.y)
    }
    // stroke(255,0,0)
    // strokeWeight(10)
    // point(vertices[0].pos.x, vertices[0].pos.y)
    
    for (v of vertices) {
        v.pos.add(v.vel)
        if (v.pos.x < 0 || v.pos.x >= width) {
            v.vel.x = -v.vel.x
            v.pos.x += v.vel.x
        }
        if (v.pos.y < 0 || v.pos.y >= height) {
            v.vel.y = -v.vel.y
            v.pos.y += v.vel.y
        }
    }
}
