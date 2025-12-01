/**
 * VectorRenderer Module
 * Renders vector field arrows at grid intersections
 */

class VectorRenderer {
    /**
     * Create a vector renderer
     * @param {CoordinateSystem} coordinateSystem - The coordinate system for transformations
     * @param {ExpressionEvaluator} expressionEvaluator - The expression evaluator for the vector field
     */
    constructor(coordinateSystem, expressionEvaluator) {
        this.coordinateSystem = coordinateSystem;
        this.expressionEvaluator = expressionEvaluator;
    }
    
    /**
     * Render vector field arrows at all visible grid intersections
     * @param {CanvasRenderingContext2D} ctx - Canvas rendering context
     */
    render(ctx) {
        if (!this.expressionEvaluator.isValid()) {
            return; // Don't render if expressions are invalid
        }
        
        const spacing = this.coordinateSystem.getGridSpacing();
        const bounds = this.coordinateSystem.getVisibleBounds();
        
        // Calculate grid line positions in math space
        const minX = Math.floor(bounds.minX / spacing) * spacing;
        const maxX = Math.ceil(bounds.maxX / spacing) * spacing;
        const minY = Math.floor(bounds.minY / spacing) * spacing;
        const maxY = Math.ceil(bounds.maxY / spacing) * spacing;
        
        // First pass: collect all valid vectors and find max magnitude
        const vectors = [];
        let maxMagnitude = 0;
        
        for (let mathY = minY; mathY <= maxY; mathY += spacing) {
            for (let mathX = minX; mathX <= maxX; mathX += spacing) {
                const vector = this.expressionEvaluator.evaluate(mathX, mathY);
                
                // Skip points that evaluate to NaN/Infinity
                if (vector === null) {
                    continue;
                }
                
                const magnitude = Math.sqrt(vector.x * vector.x + vector.y * vector.y);
                maxMagnitude = Math.max(maxMagnitude, magnitude);
                
                vectors.push({
                    mathX,
                    mathY,
                    vx: vector.x,
                    vy: vector.y,
                    magnitude
                });
            }
        }
        
        // If no valid vectors, nothing to render
        if (vectors.length === 0 || maxMagnitude === 0) {
            return;
        }
        
        // Fixed arrow length - all arrows same visual length
        const gridSpacingPixels = spacing * this.coordinateSystem.zoomLevel;
        const fixedArrowLength = gridSpacingPixels * 0.9; // 90% of grid spacing
        
        // Second pass: render all arrows
        for (const v of vectors) {
            const pixel = this.coordinateSystem.mathToPixel(v.mathX, v.mathY);
            
            // Normalize direction and use fixed length
            const magnitude = v.magnitude;
            if (magnitude < 0.0001) continue; // Skip near-zero vectors
            
            const dirX = v.vx / magnitude;
            const dirY = v.vy / magnitude;
            
            const dx = dirX * fixedArrowLength;
            const dy = -dirY * fixedArrowLength; // Invert y for pixel space
            
            // Calculate thickness based on relative magnitude with very severe scaling
            const relativeThickness = v.magnitude / maxMagnitude;
            const thickness = 0.2 + Math.pow(relativeThickness, 0.5) * 5; // Range: 0.2 to 5.2, very severe scaling
            
            this.drawArrow(ctx, pixel.x, pixel.y, dx, dy, thickness);
        }
    }
    
    /**
     * Draw an arrow at a specific position
     * @param {CanvasRenderingContext2D} ctx - Canvas rendering context
     * @param {number} x - Starting x position in pixels
     * @param {number} y - Starting y position in pixels
     * @param {number} dx - X component of arrow direction (in pixels)
     * @param {number} dy - Y component of arrow direction (in pixels)
     * @param {number} thickness - Line thickness for the arrow
     */
    drawArrow(ctx, x, y, dx, dy, thickness) {
        const length = Math.sqrt(dx * dx + dy * dy);
        
        // Don't draw zero-length arrows
        if (length < 0.01) {
            return;
        }
        
        // Normalize direction
        const dirX = dx / length;
        const dirY = dy / length;
        
        // Calculate arrowhead size based on length and thickness
        const headLength = Math.min(length * 0.3, 8);
        const headWidth = headLength * 0.6;
        
        // Draw arrow shaft with gradient effect - darker colors
        // Check if createLinearGradient is available (not in all test environments)
        if (ctx.createLinearGradient) {
            const gradient = ctx.createLinearGradient(x, y, x + dx, y + dy);
            gradient.addColorStop(0, 'rgba(60, 120, 180, 0.4)');
            gradient.addColorStop(1, 'rgba(80, 140, 200, 0.6)');
            ctx.strokeStyle = gradient;
        } else {
            // Fallback for test environments
            ctx.strokeStyle = 'rgba(70, 130, 190, 0.5)';
        }
        ctx.lineWidth = thickness;
        ctx.lineCap = 'round';
        ctx.beginPath();
        ctx.moveTo(x, y);
        ctx.lineTo(x + dx, y + dy);
        ctx.stroke();
        
        // Draw arrowhead - darker color
        ctx.fillStyle = 'rgba(80, 140, 200, 0.6)';
        ctx.beginPath();
        
        // Tip of the arrow
        const tipX = x + dx;
        const tipY = y + dy;
        
        // Calculate perpendicular vector for arrowhead wings
        const perpX = -dirY;
        const perpY = dirX;
        
        // Calculate arrowhead points
        const baseX = tipX - dirX * headLength;
        const baseY = tipY - dirY * headLength;
        
        const wing1X = baseX + perpX * headWidth;
        const wing1Y = baseY + perpY * headWidth;
        
        const wing2X = baseX - perpX * headWidth;
        const wing2Y = baseY - perpY * headWidth;
        
        // Draw filled triangle for arrowhead
        ctx.moveTo(tipX, tipY);
        ctx.lineTo(wing1X, wing1Y);
        ctx.lineTo(wing2X, wing2Y);
        ctx.closePath();
        ctx.fill();
    }
}

export { VectorRenderer };
