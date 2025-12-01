/**
 * CoordinateSystem Module
 * Handles transformations between math space and pixel space
 * Manages zoom and pan state
 */

class CoordinateSystem {
    /**
     * Create a coordinate system
     * @param {number} canvasWidth - Width of the canvas in pixels
     * @param {number} canvasHeight - Height of the canvas in pixels
     */
    constructor(canvasWidth, canvasHeight) {
        this.canvasWidth = canvasWidth;
        this.canvasHeight = canvasHeight;
        
        // Zoom level: pixels per math unit
        // Default: 50 pixels per unit (so 10 units fit in 500 pixels)
        this.zoomLevel = 50;
        
        // Pan offset in pixels (offset of origin from canvas center)
        this.offsetX = 0;
        this.offsetY = 0;
        
        // Grid configuration
        this.maxGridLines = 20; // Maximum grid lines in each direction
        this.spacingSequence = [1, 2, 5]; // Multipliers for 10^n
    }
    
    /**
     * Convert math space coordinates to pixel space coordinates
     * Note: y-axis is inverted (positive y in math space = negative y in pixel space)
     * @param {number} mathX - X coordinate in math space
     * @param {number} mathY - Y coordinate in math space
     * @returns {{x: number, y: number}} Pixel coordinates
     */
    mathToPixel(mathX, mathY) {
        const centerX = this.canvasWidth / 2;
        const centerY = this.canvasHeight / 2;
        
        const pixelX = centerX + this.offsetX + mathX * this.zoomLevel;
        const pixelY = centerY + this.offsetY - mathY * this.zoomLevel; // Y-axis inversion
        
        return { x: pixelX, y: pixelY };
    }
    
    /**
     * Convert pixel space coordinates to math space coordinates
     * Note: y-axis is inverted (positive y in pixel space = negative y in math space)
     * @param {number} pixelX - X coordinate in pixel space
     * @param {number} pixelY - Y coordinate in pixel space
     * @returns {{x: number, y: number}} Math coordinates
     */
    pixelToMath(pixelX, pixelY) {
        const centerX = this.canvasWidth / 2;
        const centerY = this.canvasHeight / 2;
        
        const mathX = (pixelX - centerX - this.offsetX) / this.zoomLevel;
        const mathY = -(pixelY - centerY - this.offsetY) / this.zoomLevel; // Y-axis inversion
        
        return { x: mathX, y: mathY };
    }
    
    /**
     * Zoom in or out while keeping a specific pixel point fixed
     * @param {number} factor - Zoom factor (>1 zooms in, <1 zooms out)
     * @param {number} centerPixelX - X coordinate of zoom center in pixels
     * @param {number} centerPixelY - Y coordinate of zoom center in pixels
     */
    zoom(factor, centerPixelX, centerPixelY) {
        // Get the math point at the center before zoom
        const mathPoint = this.pixelToMath(centerPixelX, centerPixelY);
        
        // Apply zoom
        this.zoomLevel *= factor;
        
        // Clamp zoom to reasonable bounds
        this.zoomLevel = Math.max(0.001, Math.min(10000, this.zoomLevel));
        
        // Calculate where the math point would be after zoom
        const newPixelPoint = this.mathToPixel(mathPoint.x, mathPoint.y);
        
        // Adjust offset to keep the math point at the same pixel location
        this.offsetX += centerPixelX - newPixelPoint.x;
        this.offsetY += centerPixelY - newPixelPoint.y;
    }
    
    /**
     * Pan the view by a pixel delta
     * @param {number} deltaPixelX - Horizontal pan distance in pixels
     * @param {number} deltaPixelY - Vertical pan distance in pixels
     */
    pan(deltaPixelX, deltaPixelY) {
        this.offsetX += deltaPixelX;
        this.offsetY += deltaPixelY;
    }
    
    /**
     * Calculate appropriate grid spacing based on current zoom level
     * Uses (1, 2, 5) × 10^n sequence to maintain bounded grid line count
     * @returns {number} Grid spacing in math units
     */
    getGridSpacing() {
        // Calculate approximate number of math units visible in both dimensions
        const visibleWidth = this.canvasWidth / this.zoomLevel;
        const visibleHeight = this.canvasHeight / this.zoomLevel;
        
        // Use the larger dimension to ensure we don't exceed maxGridLines
        const maxVisibleDimension = Math.max(visibleWidth, visibleHeight);
        
        // Target spacing to get as close to maxGridLines as possible
        const targetSpacing = maxVisibleDimension / this.maxGridLines;
        
        // Find the appropriate power of 10
        const magnitude = Math.floor(Math.log10(targetSpacing));
        const base = Math.pow(10, magnitude);
        
        // Find the smallest multiplier from the sequence [1, 2, 5] that doesn't exceed maxGridLines
        let bestSpacing = base * 10; // Start with next power of 10 as fallback
        
        for (let multiplier of this.spacingSequence) {
            const spacing = multiplier * base;
            const numLines = maxVisibleDimension / spacing;
            
            // Use this spacing if it keeps us under maxGridLines
            if (numLines <= this.maxGridLines) {
                bestSpacing = spacing;
                break;
            }
        }
        
        return bestSpacing;
    }
    
    /**
     * Get the visible bounds in math space
     * @returns {{minX: number, maxX: number, minY: number, maxY: number}}
     */
    getVisibleBounds() {
        const topLeft = this.pixelToMath(0, 0);
        const bottomRight = this.pixelToMath(this.canvasWidth, this.canvasHeight);
        
        return {
            minX: topLeft.x,
            maxX: bottomRight.x,
            minY: bottomRight.y,
            maxY: topLeft.y
        };
    }
    
    /**
     * Update canvas dimensions (called on resize)
     * @param {number} width - New canvas width
     * @param {number} height - New canvas height
     */
    updateDimensions(width, height) {
        this.canvasWidth = width;
        this.canvasHeight = height;
    }
}

// Export for module usage
export { CoordinateSystem };
