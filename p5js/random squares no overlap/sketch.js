const squares = []
const maxs = 100

function setup() {
    createCanvas(400, 400);
    noStroke()
    rectMode(CENTER)
    squares.push({
        pos: createVector(random(width), random(height)),
        s: random(maxs),
        hu:floor(random(256))
    })
    colorMode(HSB)
}

function draw() {
    background(51);

    //add a square
    let attempts = 0
    while (attempts < 1000) {
        const newSquare = {
            pos: createVector(random(width), random(height)),
            s: random(maxs),
            hu:floor(random(256))
        }
        failed = false
        for (square of squares) {
            const dx = abs(square.pos.x-newSquare.pos.x)
            const dy = abs(square.pos.y-newSquare.pos.y)
            if (min(dx,dy) < .5*(square.s+newSquare.s)) {
                failed = true
                break
            }
        }
        if(!failed){
            squares.push(newSquare)
            break
        }
        attempts++;
    }

    //draw all squares
    for (square of squares) {
        fill(square.hu,255,255)
        rect(square.pos.x, square.pos.y, square.s, square.s)
    }
}
