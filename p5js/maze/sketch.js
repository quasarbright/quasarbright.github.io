class Graph {
  // edges are [parent, child, data]
  constructor() {
    this.nodes = []
    this.edges = []
  }
  addNode(n) {
    this.nodes.push(n)
  }
  addEdge(a, b, data) {
    this.edges.push([a, b, data])
    this.edges.push([b, a, data])
  }
  getEdgeData(a, b) {
    let e = this.edges.find((e, _, _) => e[0].equals(a) && e[1].equals(b))
    if (e !== undefined) {
      return e[2]
    } else {
      return undefined
    }
  }
  getEdges(n) {
    return this.edges.filter((e, _, _) => e[0].equals(n))
  }
  getNeighboringNodes(n) {
    let es = this.getEdges(n)
    return es.map(e => e[1])
  }
}

function range(n) {
  let ans = []
  for(let i = 0; i < n; i++) {
    ans.push(i)
  }
  return ans
}

function makeMazeGraph(w, h) {
  let g = new Graph()
  range(h).forEach(r => range(w).forEach(c => g.addNode(createVector(c, r))))
  let tlNodes = g.nodes.filter(v => (v.x < w-1) && (v.y < h-1))
  tlNodes.forEach(v => {
    let d = createVector(v.x, v.y+1)
    let r = createVector(v.x+1, v.y)
    g.addEdge(v, r, random())
    g.addEdge(v, d, random())
  })
  return g
}

class VectorMap {
  constructor() {
    this.assocs = []
  }

  set(k, v) {
    this.assocs.push([k, v])
  }

  get(k) {
    let assoc = this.assocs.find(assoc => assoc[0].equals(k))
    if (assoc === undefined) {
      return undefined
    } else {
      return assoc[1]
    }
  }
}

class VectorSet {
  constructor(items) {
    this.items = []
    items.forEach(v => this.add(v))
  }

  add(v) {
    if(!this.mem(v)) {
      this.items.push(v)
    }
  }

  mem(v) {
    return this.items.find(v2 => v.equals(v2)) !== undefined
  }

  union(s) {
    let ans = new VectorSet()
    ans.items = this.items
    ans.items.forEach(v => ans.add(v))
  }
}

class UnionFind {
  constructor(vectors) {
    this.sets = new VectorMap()
    this.reps = new VectorMap()
    vectors.forEach(v => {
      this.reps.set(v, [v])
    })

  }
}

function makeMST(g) {
  let nodesNotInTree = g.nodes
  let tree = new Graph()
  while (nodesNotInTree.length !== 0) {
    let node = nodesNotInTree.pop()
    tree.addNode(node)
    
  }


}

let g
function setup() {
  createCanvas(400, 400);
  g = makeMazeGraph(10, 10)
}

function draw() {
  background(51);
}
