/**
 * Main animation logic for the honeycomb Hamiltonian cycle visualization.
 * 
 * Handles canvas setup, rendering the honeycomb grid, and animating
 * the neon glow line that traces the Hamiltonian cycle.
 */

// Canvas and context
const canvas = document.getElementById('canvas');
const ctx = canvas.getContext('2d');

// Animation state
let honeycomb;
let animationProgress = 0;
const animationSpeed = 0.02; // Units per frame
let centerX, centerY;

// Visual parameters
const HEX_RADIUS = 40;
const LINE_WIDTH = 3;

// Neon color palette
const NEON_PALETTE = [
    '#00ffff', // Cyan
    '#ff00ff', // Magenta
    '#00ff00', // Green
    '#ffff00', // Yellow
    '#ff0080', // Hot Pink
    '#00ff80', // Spring Green
    '#ff6600', // Orange
    '#8000ff', // Purple
    '#00ffcc', // Turquoise
    '#ff0040', // Red-Pink
    '#80ff00', // Lime
    '#0080ff', // Blue
];

let NEON_COLOR = NEON_PALETTE[0];
let GLOW_COLOR = NEON_PALETTE[0];

/**
 * Calculate grid dimensions to fill the screen.
 */
function calculateGridSize() {
    const hexWidth = HEX_RADIUS * Math.sqrt(3);
    const hexHeight = HEX_RADIUS * 1.5;
    
    // Calculate how many hexagons fit across and down, plus extra to go beyond edges
    const cols = Math.ceil(canvas.width / hexWidth) + 4;
    const rows = Math.ceil(canvas.height / hexHeight) + 4;
    
    return { rows, cols };
}

/**
 * Select a random neon color from the palette.
 */
function selectRandomNeonColor() {
    const color = NEON_PALETTE[Math.floor(Math.random() * NEON_PALETTE.length)];
    NEON_COLOR = color;
    GLOW_COLOR = color;
    console.log(`Selected neon color: ${color}`);
}

/**
 * Initialize the canvas and honeycomb structure.
 */
function init() {
    resizeCanvas();
    window.addEventListener('resize', resizeCanvas);
    
    selectRandomNeonColor();
    
    const { rows, cols } = calculateGridSize();
    honeycomb = new Honeycomb(rows, cols, HEX_RADIUS);
    
    console.log(`MST has ${honeycomb.mst.length} edges spanning ${honeycomb.vertices.length} vertices`);
    console.log(`Growth has ${honeycomb.growthOrder.length} levels`);
    
    // Calculate center for positioning
    const gridCenter = honeycomb.getCenter();
    centerX = canvas.width / 2 - gridCenter.x;
    centerY = canvas.height / 2 - gridCenter.y;
    
    // Start animation
    animate();
}

/**
 * Resize canvas to fill window.
 */
function resizeCanvas() {
    canvas.width = window.innerWidth;
    canvas.height = window.innerHeight;
}

/**
 * Draw a hexagon at the given position.
 */
function drawHexagon(x, y, radius) {
    ctx.beginPath();
    for (let i = 0; i < 6; i++) {
        const angle = (Math.PI / 3) * i - Math.PI / 6;
        const px = x + radius * Math.cos(angle);
        const py = y + radius * Math.sin(angle);
        if (i === 0) {
            ctx.moveTo(px, py);
        } else {
            ctx.lineTo(px, py);
        }
    }
    ctx.closePath();
}

/**
 * Draw the honeycomb grid structure.
 */
function drawGrid() {
    // Grid drawing disabled for cleaner look
}

/**
 * Draw the neon glowing tree growing from the root.
 * All edges at the same depth level grow in parallel like a fluid.
 * Uses layered shadows for the glow effect.
 */
function drawNeonTree() {
    const growthLevels = honeycomb.growthOrder;
    
    if (growthLevels.length === 0) return;
    
    const currentLevelIndex = Math.floor(animationProgress);
    const levelProgress = animationProgress - currentLevelIndex;
    
    // Draw multiple layers for glow effect
    const glowLayers = [
        { width: 15, alpha: 0.1 },
        { width: 10, alpha: 0.2 },
        { width: 6, alpha: 0.4 },
        { width: LINE_WIDTH, alpha: 1.0 }
    ];
    
    for (const layer of glowLayers) {
        ctx.strokeStyle = GLOW_COLOR;
        ctx.globalAlpha = layer.alpha;
        ctx.lineWidth = layer.width;
        ctx.lineCap = 'round';
        ctx.lineJoin = 'round';
        
        // Draw all completed levels
        for (let levelIdx = 0; levelIdx < currentLevelIndex && levelIdx < growthLevels.length; levelIdx++) {
            const edgesAtLevel = growthLevels[levelIdx];
            
            for (const [v1Idx, v2Idx] of edgesAtLevel) {
                const v1 = honeycomb.vertices[v1Idx];
                const v2 = honeycomb.vertices[v2Idx];
                
                const x1 = centerX + v1.x;
                const y1 = centerY + v1.y;
                const x2 = centerX + v2.x;
                const y2 = centerY + v2.y;
                
                ctx.beginPath();
                ctx.moveTo(x1, y1);
                ctx.lineTo(x2, y2);
                ctx.stroke();
            }
        }
        
        // Draw current level in progress (all edges at same progress)
        if (currentLevelIndex < growthLevels.length) {
            const edgesAtLevel = growthLevels[currentLevelIndex];
            
            for (const [v1Idx, v2Idx] of edgesAtLevel) {
                const v1 = honeycomb.vertices[v1Idx];
                const v2 = honeycomb.vertices[v2Idx];
                
                const x1 = centerX + v1.x;
                const y1 = centerY + v1.y;
                const x2 = centerX + v2.x;
                const y2 = centerY + v2.y;
                
                const px = x1 + (x2 - x1) * levelProgress;
                const py = y1 + (y2 - y1) * levelProgress;
                
                ctx.beginPath();
                ctx.moveTo(x1, y1);
                ctx.lineTo(px, py);
                ctx.stroke();
            }
        }
    }
    
    ctx.globalAlpha = 1.0;
}

/**
 * Main animation loop.
 */
function animate() {
    // Clear canvas
    ctx.fillStyle = '#000000';
    ctx.fillRect(0, 0, canvas.width, canvas.height);
    
    // Draw the honeycomb structure
    drawGrid();
    
    // Draw the neon tree
    drawNeonTree();
    
    // Update animation progress
    animationProgress += animationSpeed;
    
    // Reset when tree is complete (with a small delay)
    if (animationProgress > honeycomb.growthOrder.length + 10) {
        animationProgress = 0;
        // Generate a new random tree with new color
        selectRandomNeonColor();
        const { rows, cols } = calculateGridSize();
        honeycomb = new Honeycomb(rows, cols, HEX_RADIUS);
    }
    
    requestAnimationFrame(animate);
}

// Start the application
init();

