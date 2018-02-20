const circles = []
const maxrad = 50

function setup() {
    createCanvas(400, 400);
    noStroke()
    rectMode(CENTER)
    circles.push({
        pos: createVector(random(width), random(height)),
        r: random(maxrad)
    })
}

function draw() {
    background(51);

    //add a circle
    let attempts = 0
    while (attempts < 1000) {
        const newCirc = {
            pos: createVector(random(width), random(height)),
            r: random(maxrad)
        }
        failed = false
        for (circ of circles) {
            // const d = dist(circ.pos.x, circ.pos.y, newCirc.pos.x, newCirc.pos.y)
            const dx = abs(circ.pos.x-newCirc.pos.x)
            const dy = abs(circ.pos.y-newCirc.pos.y)
            if ((dy < circ.r/2.0 + newCirc.r/2.0) && (dx < circ.r/2.0 + newCirc.r/2.0)) {//maybe there's a problem with min
                failed = true
                break
            }
        }
        if(!failed){
            circles.push(newCirc)
            break
        }
        attempts++;
    }

    //draw all circles
    for (circ of circles) {
        rect(circ.pos.x, circ.pos.y, circ.r, circ.r)
    }
}
