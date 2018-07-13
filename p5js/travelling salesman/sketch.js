let points = []
let best = []
let popsize = 10
function setup() {
    createCanvas(400,400)
    for (let i = 0; i < popsize; i++) points.push(createVector(random(width),random(height)))
    best = points.slice()
}

function draw() {
    background(51);

    let shuffled = shuffle(points)

    if ( lensq(shuffled) < lensq(best) ) best = shuffled.slice()

    for(let i = 0; i < best.length-1; i ++){
        p1 = best[i]
        p2 = best[i+1]
        stroke(255)
        strokeWeight(2)
        line(p1.x,p1.y,p2.x,p2.y)
    }
    for( let p of points){
        stroke(255)
        strokeWeight(5)
        point(p.x,p.y)
    }
}

function lensq(a){
    //expects vector array
    // returns sum of square distances
    let ans = 0
    for(let i = 0; i < a.length-1; i ++){
        p1 = a[i]
        p2 = a[i+1]
        ans += (p1.x - p2.x) * (p1.x - p2.x)
        ans += (p1.y - p2.y) * (p1.y - p2.y)
    }
    return ans
}

function shuffle(a){
    let copy = a.slice()
    let ans = []
    for(let i = a.length-1; i >= 0; i--){
        ans.push(copy.splice(floor(random(a.length)),1))
    }
    return ans
}
