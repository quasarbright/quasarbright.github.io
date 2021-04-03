/*

constraint-based visualizations
kind of reactive, but uses thunks basically

draggable points have a special hook in the draw loop
mostly, other logic is confined to updates

there are draggable points, derived points,
line segments, and sliders.

draggable points can be dragged by the user and are unconstrained.
Their position is updated when dragged.

derived points have a position calculated by a thunk.
It stores the thunk and caches its result, so
you have to keep it updated manually.

Segments take in two point-like objects and uses their cached positions
every time they are drawn. (uses the .p field of a point)

each class has a list of all instances, and the constructors add
this to the list. these lists are used for world updates



*/

// INVARIANT: At most one point is selected
let points = []
selected = undefined

/**
 * select a draggable point and deselect all others
 * @param {Point} p point to select
 */
function selectPoint(p) {
    deselectAll()
    p.selected = true
    selected = p
}

/**
 * deselect all points
 */
function deselectAll() {
    selected = undefined
    points.forEach(p => p.selected = false)
}

/**
 * draggable points.
 */
class Point {
    /**
     * create a draggable point
     * @param {p5.Vector} p position
     * @param {boolean} draggable is this point draggable?
     * @param {number} rad radius
     * @param {p5.color} c fill color
     */
    constructor(p,draggable,rad,c) {
        this.p = p || createVector(0,0)
        this.draggable = draggable || false
        this.rad = rad || 7
        this.c = c || color(200)
        this.selected = false
        points.push(this)
    }

    /**
     * linear interpolate between this point and other. 
     * @param {number} r interpolation rate (should be in [0,1])
     * @param {Point} other end point 
     * @returns {p5.Vector} interpolated position
     */
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
        this.c = c || color(108)
        derivedPoints.push(this)
    }

    /**
     * linear interpolate between this point and other. 
     * @param {number} r interpolation rate (should be in [0,1])
     * @param {Point} other end point 
     * @returns {p5.Vector} interpolated position
     */
    lerp(r, other) {
        return p5.Vector.lerp(this.p, other.p, r)
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

/**
 * create a point that is interpolated between two points.
 * @param {Point} start start point
 * @param {Point} end end point
 * @param {() => number} kf thunk to compute interpolation rate
 * @returns {DerivedPoint}
 */
function lerpDerivedPoint(start,end,kf) {
    return new DerivedPoint(()=>start.lerp(kf(),end).copy())
}

/**
 * create a point that is at the intersection of two segments
 * @param {Segment} ab 
 * @param {Segment} cd 
 * @returns {DerivedPoint}
 */
function intersectionDerivedPoint(ab,cd) {
    return new DerivedPoint(()=>ab.intersection(cd).copy())
}


let segments = []
/**
 * line segment between two points
 */
class Segment {
    /**
     * create a line segment between two points
     * @param {Point} pi start point
     * @param {Point} pf end point
     * @param {number} thickness line thickness
     * @param {pt.color} c stroke color
     */
    constructor(pi,pf,thickness,c) { // points!
        this.pi = pi
        this.pf = pf
        this.thickness = thickness || 5
        this.c = c || color(108)
        segments.push(this)
    }

    // get startPos() {return pi.p}
    // get endPos() {return pf.p}

    draw() {
        stroke(this.c)
        strokeWeight(this.thickness)
        line(this.pi.p.x,this.pi.p.y,this.pf.p.x,this.pf.p.y)
    }

    /**
     * calculate slope of this line
     * @returns {number}
     */
    slope() {
        let f = this.pf.p
        let i = this.pi.p
        return (f.y-i.y) / (f.x-i.x)
    }

    /**
     * calculate intersection position of two line segments as vector
     * @param {Segment} other 
     * @returns {p5.Vector}
     */
    intersection(other) {
        let a = this.pi.p
        let b = this.pf.p
        let c = other.pi.p
        let d = other.pf.p
        let mab = this.slope()
        let mcd = other.slope()
        let x = (a.x*mab - c.x * mcd + c.y - a.y) / (mab - mcd)
        let y = (x - a.x)*mab + a.y
        return createVector(x,y)
    }
}

let slidables = []
/**
 * a float variable editable by the user
 */
class Slidable {
    /**
     * create a float float variable editable by a slider
     * @param {string} name name of the variable
     * @param {number} x value
     * @param {number} min min value
     * @param {number} max max value
     * @param {number} step step size between values
     */
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

/**
 * 
 * @param {p5.Vector} v1 
 * @param {p5.Vector} v2 
 * @returns {number} squared distance between vectors v1,v2
 */
function distSq(v1,v2) {
    let ans = p5.Vector.sub(v1,v2).magSq()
    return ans
}

/**
 * 
 * @param {p5.Vector} v 
 * @param {Array.<Point>} points 
 * @returns {?Point} the point which is closest to v, or null
 */
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
        return null
    }
}

function initSliders() {
    slidables.forEach(s => {
        $("#inputs").append($(`<label for="${s.name}">${s.name}</label>`))
        $("#inputs").append($(`<input class="slider" name="${s.name}" id="${s.name}" type="range" min="${s.min}" max="${s.max}" step="${s.step}" value="${s.x}">`))
        $("#inputs").append($(`<hr>`))
    })
}

function initWorld() {
    initSliders()
}

//updaters
function updateSelected() {
    if (mouseIsPressed) {
        mouse = createVector(mouseX, mouseY)
        if (selected !== undefined) {
            selected.p = mouse.copy()
        } else {
            draggables = points.filter(p => p.draggable && distSq(mouse, p.p) < p.rad * p.rad)
            best = closestPoint(mouse, draggables)
            if (best !== null) {
                selectPoint(best)
                best.p = mouse.copy()
            }
        }
    } else {
        deselectAll()
    }
}

function updateWorld() {
    //update sliders
    slidables.forEach(s => s.update())
    updateSelected()
    //update derived point locations
    derivedPoints.forEach(p => p.update())
}

function drawWorld() {
    // draw segments
    segments.forEach(s => s.draw())
    // draw points
    derivedPoints.forEach(p => p.draw())
    points.forEach(p => { p.draw() })
    // ensure selected points drawn on top
    points.filter(p => p.selected).forEach(p => p.draw())
}
