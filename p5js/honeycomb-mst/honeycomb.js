/**
 * Honeycomb grid generation and Hamiltonian cycle computation.
 * 
 * This module creates a hexagonal honeycomb tiling and builds a graph
 * where vertices are at hexagon corners and edges are along hexagon sides.
 * Then computes a Hamiltonian cycle through all vertices.
 */

class Honeycomb {
    constructor(rows, cols, hexRadius) {
        this.rows = rows;
        this.cols = cols;
        this.hexRadius = hexRadius;
        this.hexagons = []; // Center positions of hexagons
        this.vertices = []; // Vertices at hexagon corners
        this.edges = []; // Edges along hexagon sides
        this.vertexMap = new Map(); // Map from "x,y" key to vertex index
        
        this.generateHexagons();
        this.generateVerticesAndEdges();
        this.mst = this.generateMinimumSpanningTree();
        this.growthOrder = this.computeGrowthOrder();
    }
    
    /**
     * Generate the hexagon centers.
     * Uses pointy-top hexagon orientation.
     */
    generateHexagons() {
        // For pointy-top hexagons:
        // Width (point to point) = 2 * radius
        // Height (flat to flat) = radius * sqrt(3)
        const hexWidth = this.hexRadius * Math.sqrt(3); // horizontal distance between centers
        const hexHeight = this.hexRadius * 1.5; // vertical distance between rows
        
        for (let row = 0; row < this.rows; row++) {
            for (let col = 0; col < this.cols; col++) {
                // Offset every other row by half the horizontal spacing
                const xOffset = (row % 2) * (hexWidth / 2);
                const x = col * hexWidth + xOffset;
                const y = row * hexHeight;
                
                this.hexagons.push({ x, y, row, col });
            }
        }
    }
    
    /**
     * Generate vertices at hexagon corners and edges along hexagon sides.
     */
    generateVerticesAndEdges() {
        const vertexKey = (x, y) => `${x.toFixed(2)},${y.toFixed(2)}`;
        const edgeSet = new Set();
        
        // For each hexagon, create its 6 corner vertices
        for (const hex of this.hexagons) {
            const corners = this.getHexagonCorners(hex.x, hex.y);
            
            for (let i = 0; i < 6; i++) {
                const corner = corners[i];
                const key = vertexKey(corner.x, corner.y);
                
                // Add vertex if not already present
                if (!this.vertexMap.has(key)) {
                    const idx = this.vertices.length;
                    this.vertices.push(corner);
                    this.vertexMap.set(key, idx);
                }
                
                // Add edge to next corner
                const nextCorner = corners[(i + 1) % 6];
                const v1 = this.vertexMap.get(key);
                const v2Key = vertexKey(nextCorner.x, nextCorner.y);
                
                // Ensure second vertex exists
                if (!this.vertexMap.has(v2Key)) {
                    const idx = this.vertices.length;
                    this.vertices.push(nextCorner);
                    this.vertexMap.set(v2Key, idx);
                }
                
                const v2 = this.vertexMap.get(v2Key);
                
                // Add edge (avoid duplicates by sorting indices)
                const edgeKey = v1 < v2 ? `${v1},${v2}` : `${v2},${v1}`;
                if (!edgeSet.has(edgeKey)) {
                    edgeSet.add(edgeKey);
                    this.edges.push([v1, v2]);
                }
            }
        }
    }
    
    /**
     * Get the 6 corner positions of a hexagon.
     */
    getHexagonCorners(cx, cy) {
        const corners = [];
        for (let i = 0; i < 6; i++) {
            const angle = (Math.PI / 3) * i - Math.PI / 6;
            corners.push({
                x: cx + this.hexRadius * Math.cos(angle),
                y: cy + this.hexRadius * Math.sin(angle)
            });
        }
        return corners;
    }
    
    /**
     * Build adjacency list for the vertex graph.
     */
    buildAdjacencyList() {
        const adj = Array.from({ length: this.vertices.length }, () => []);
        
        for (const [v1, v2] of this.edges) {
            adj[v1].push(v2);
            adj[v2].push(v1);
        }
        
        return adj;
    }
    
    /**
     * Generate a random spanning tree using randomized Prim's algorithm.
     * Returns an array of edges [v1, v2] in the tree.
     */
    generateMinimumSpanningTree() {
        console.log(`Computing random spanning tree for ${this.vertices.length} vertices...`);
        
        if (this.vertices.length === 0) return [];
        
        const mstEdges = [];
        const inMST = new Set();
        
        // Start from center vertex
        const centerVertex = this.findCenterVertex();
        inMST.add(centerVertex);
        
        // Candidate edges - edges with one vertex in MST and one outside
        const candidateEdges = [];
        
        // Add all edges from start vertex
        for (const [v1, v2] of this.edges) {
            if (v1 === centerVertex) {
                candidateEdges.push([v1, v2]);
            } else if (v2 === centerVertex) {
                candidateEdges.push([v2, v1]);
            }
        }
        
        // Randomized Prim's algorithm
        while (inMST.size < this.vertices.length && candidateEdges.length > 0) {
            // Find all valid edges (one vertex in MST, one outside)
            const validEdges = [];
            for (let i = 0; i < candidateEdges.length; i++) {
                const [v1, v2] = candidateEdges[i];
                if (inMST.has(v1) && !inMST.has(v2)) {
                    validEdges.push(i);
                }
            }
            
            if (validEdges.length === 0) break;
            
            // Randomly select one of the valid edges
            const randomIdx = validEdges[Math.floor(Math.random() * validEdges.length)];
            const [v1, v2] = candidateEdges[randomIdx];
            candidateEdges.splice(randomIdx, 1);
            
            // Add edge to MST
            mstEdges.push([v1, v2]);
            inMST.add(v2);
            
            // Add new candidate edges from v2
            for (const [e1, e2] of this.edges) {
                if (e1 === v2 && !inMST.has(e2)) {
                    candidateEdges.push([e1, e2]);
                } else if (e2 === v2 && !inMST.has(e1)) {
                    candidateEdges.push([e2, e1]);
                }
            }
        }
        
        console.log(`Spanning tree has ${mstEdges.length} edges`);
        return mstEdges;
    }
    
    /**
     * Find the most central vertex in the grid.
     */
    findCenterVertex() {
        const center = this.getCenter();
        let closestVertex = 0;
        let minDist = Infinity;
        
        for (let i = 0; i < this.vertices.length; i++) {
            const v = this.vertices[i];
            const dist = Math.hypot(v.x - center.x, v.y - center.y);
            if (dist < minDist) {
                minDist = dist;
                closestVertex = i;
            }
        }
        
        return closestVertex;
    }
    
    /**
     * Compute the order in which edges should be drawn to show tree growing.
     * Groups edges by depth level for parallel expansion like a fluid.
     * Returns array of levels, where each level is an array of edges.
     */
    computeGrowthOrder() {
        if (this.mst.length === 0) return [];
        
        // Build adjacency list for MST
        const mstAdj = Array.from({ length: this.vertices.length }, () => []);
        for (const [v1, v2] of this.mst) {
            mstAdj[v1].push(v2);
            mstAdj[v2].push(v1);
        }
        
        // BFS to determine growth order by levels
        const root = this.findCenterVertex();
        const visited = new Set([root]);
        let currentLevel = [root];
        const growthLevels = [];
        
        while (currentLevel.length > 0) {
            const nextLevel = [];
            const edgesAtThisLevel = [];
            
            for (const current of currentLevel) {
                for (const neighbor of mstAdj[current]) {
                    if (!visited.has(neighbor)) {
                        visited.add(neighbor);
                        nextLevel.push(neighbor);
                        edgesAtThisLevel.push([current, neighbor]);
                    }
                }
            }
            
            if (edgesAtThisLevel.length > 0) {
                growthLevels.push(edgesAtThisLevel);
            }
            
            currentLevel = nextLevel;
        }
        
        let totalEdges = 0;
        for (const level of growthLevels) {
            totalEdges += level.length;
        }
        console.log(`Growth has ${growthLevels.length} levels with ${totalEdges} total edges`);
        return growthLevels;
    }
    
    /**
     * Get the center point of the grid for positioning.
     */
    getCenter() {
        if (this.vertices.length === 0) {
            return { x: 0, y: 0 };
        }
        
        let sumX = 0, sumY = 0;
        for (const vertex of this.vertices) {
            sumX += vertex.x;
            sumY += vertex.y;
        }
        
        return {
            x: sumX / this.vertices.length,
            y: sumY / this.vertices.length
        };
    }
}

