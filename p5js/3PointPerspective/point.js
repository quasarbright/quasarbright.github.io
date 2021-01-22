let points = []
selected = undefined

function selectPoint(p) {
    deselectAll()
    p.selected = true
    selected = p
}

function deselectAll() {
    selected = undefined
    points.forEach(p => p.selected = false)
}

class Point {
    constructor(p,draggable,rad,c) {
        this.p = p || createVector(0,0)
        this.draggable = draggable || false
        this.rad = rad || 7
        this.c = c || color(200)
        this.selected = false
        points.push(this)
    }

    lerp(r,other) {
        return p5.Vector.lerp(this.p,other.p,r)
    }

    draw() {
        let p = this
        if (p.selected) {
            strokeWeight(2)
            stroke(255, 0, 0)
        } else {
            noStroke()
        }
        fill(p.c)
        ellipse(p.p.x, p.p.y, p.rad, p.rad)
    }
}

let derivedPoints = [] // add these in a dependency-safe order!
class DerivedPoint {
    constructor(fp,rad,c) {
        this.fp = fp
        this.p = fp()
        this.rad=rad || 7
        this.c = c || color(200)
        derivedPoints.push(this)
    }

    update() {
        this.p = this.fp()
    }

    draw() {
        let p = this
        noStroke()
        fill(p.c)
        ellipse(p.p.x, p.p.y, p.rad, p.rad)
    }
}

function lerpDerivedPoint(start,end,kf) {
    return new DerivedPoint(()=>start.lerp(kf(),end).copy())
}

let segments = []
class Segment {
    constructor(pi,pf,thickness,c) { // points!
        this.pi = pi
        this.pf = pf
        this.thickness = thickness || 5
        this.c = c || color(108)
        segments.push(this)
    }

    get startPos() {return pi.p}
    get endPos() {return pf.p}

    draw() {
        stroke(this.c)
        strokeWeight(this.thickness)
        line(this.pi.p.x,this.pi.p.y,this.pf.p.x,this.pf.p.y)
    }
}

let slidables = []
class Slidable {
    constructor(name,x,min,max,step) {
        this.name=name
        this.x = x
        this.min = min || Math.min(x,0)
        this.max = max || Math.max(x,1)
        this.step = step || 0.1
        slidables.push(this)
    }

    update() {
        this.x = float($(`#${this.name}`).val())
    }
}

function distSq(v1,v2) {
    
    let ans = p5.Vector.sub(v1,v2).magSq()
    return ans
}

function closestPoint(v,points) {
    if (points.length > 0) {
        let best = points[0]
        let bestdsq = distSq(v,best.p)
        points.forEach(point => {
            let dsq = distSq(v, point.p)
            if (dsq < bestdsq) {
                bestdsq = dsq
                best = point
            }
        });
        return best
    } else {
        return undefined
    }
}
