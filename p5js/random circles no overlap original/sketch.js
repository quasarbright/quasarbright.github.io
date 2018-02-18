const circles = []
const maxrad = 50

function setup() {
    createCanvas(400, 400);
    noStroke()
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
            const d = dist(circ.pos.x, circ.pos.y, newCirc.pos.x, newCirc.pos.y)
            if (!(d >= circ.r + newCirc.r)) {
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
        ellipse(circ.pos.x, circ.pos.y, 2 * circ.r)
    }
}
