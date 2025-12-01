/**
 * GridRenderer Module
 * Renders grid lines, axes, and numeric labels
 */

class GridRenderer {
    /**
     * Create a grid renderer
     * @param {CoordinateSystem} coordinateSystem - The coordinate system for transformations
     * @param {HTMLCanvasElement} canvas - The canvas element
     */
    constructor(coordinateSystem, canvas) {
        this.coordinateSystem = coordinateSystem;
        this.canvas = canvas;
        
        // Offscreen canvas for caching grid rendering
        // Only create if document is available (not in all test environments)
        if (typeof document !== 'undefined') {
            this.offscreenCanvas = document.createElement('canvas');
            this.offscreenCtx = this.offscreenCanvas.getContext('2d');
        } else {
            this.offscreenCanvas = null;
            this.offscreenCtx = null;
        }
        
        // Cache state to detect when re-render is needed
        this.cachedZoom = null;
        this.cachedOffsetX = null;
        this.cachedOffsetY = null;
        this.cachedWidth = null;
        this.cachedHeight = null;
    }
    
    /**
     * Check if grid cache needs to be updated
     * @returns {boolean} True if cache is invalid
     */
    _needsUpdate() {
        return this.cachedZoom !== this.coordinateSystem.zoomLevel ||
               this.cachedOffsetX !== this.coordinateSystem.offsetX ||
               this.cachedOffsetY !== this.coordinateSystem.offsetY ||
               this.cachedWidth !== this.canvas.width ||
               this.cachedHeight !== this.canvas.height;
    }
    
    /**
     * Update the offscreen canvas cache
     */
    _updateCache() {
        // Resize offscreen canvas if needed
        if (this.offscreenCanvas.width !== this.canvas.width ||
            this.offscreenCanvas.height !== this.canvas.height) {
            this.offscreenCanvas.width = this.canvas.width;
            this.offscreenCanvas.height = this.canvas.height;
        }
        
        // Clear offscreen canvas
        this.offscreenCtx.clearRect(0, 0, this.offscreenCanvas.width, this.offscreenCanvas.height);
        
        // Render grid to offscreen canvas
        this._renderGrid(this.offscreenCtx);
        
        // Update cache state
        this.cachedZoom = this.coordinateSystem.zoomLevel;
        this.cachedOffsetX = this.coordinateSystem.offsetX;
        this.cachedOffsetY = this.coordinateSystem.offsetY;
        this.cachedWidth = this.canvas.width;
        this.cachedHeight = this.canvas.height;
    }
    
    /**
     * Render the grid, axes, and labels
     * @param {CanvasRenderingContext2D} ctx - Canvas rendering context
     */
    render(ctx) {
        // Use caching if available, otherwise render directly
        if (this.offscreenCanvas && this.offscreenCtx) {
            // Update cache if needed
            if (this._needsUpdate()) {
                this._updateCache();
            }
            
            // Draw cached grid
            ctx.drawImage(this.offscreenCanvas, 0, 0);
        } else {
            // Render directly without caching (test environment)
            this._renderGrid(ctx);
        }
    }
    
    /**
     * Internal method to render grid to a context
     * @private
     * @param {CanvasRenderingContext2D} ctx - Canvas rendering context
     */
    _renderGrid(ctx) {
        const spacing = this.coordinateSystem.getGridSpacing();
        const bounds = this.coordinateSystem.getVisibleBounds();
        
        // Calculate grid line positions in math space
        const minX = Math.floor(bounds.minX / spacing) * spacing;
        const maxX = Math.ceil(bounds.maxX / spacing) * spacing;
        const minY = Math.floor(bounds.minY / spacing) * spacing;
        const maxY = Math.ceil(bounds.maxY / spacing) * spacing;
        
        // Draw vertical grid lines
        ctx.strokeStyle = 'rgba(48, 54, 61, 0.6)';
        ctx.lineWidth = 1;
        ctx.beginPath();
        for (let x = minX; x <= maxX; x += spacing) {
            if (Math.abs(x) < spacing * 0.001) continue; // Skip axis (will draw separately)
            const pixelTop = this.coordinateSystem.mathToPixel(x, bounds.maxY);
            const pixelBottom = this.coordinateSystem.mathToPixel(x, bounds.minY);
            ctx.moveTo(pixelTop.x, pixelTop.y);
            ctx.lineTo(pixelBottom.x, pixelBottom.y);
        }
        ctx.stroke();
        
        // Draw horizontal grid lines
        ctx.beginPath();
        for (let y = minY; y <= maxY; y += spacing) {
            if (Math.abs(y) < spacing * 0.001) continue; // Skip axis (will draw separately)
            const pixelLeft = this.coordinateSystem.mathToPixel(bounds.minX, y);
            const pixelRight = this.coordinateSystem.mathToPixel(bounds.maxX, y);
            ctx.moveTo(pixelLeft.x, pixelLeft.y);
            ctx.lineTo(pixelRight.x, pixelRight.y);
        }
        ctx.stroke();
        
        // Draw axes with distinct styling
        ctx.strokeStyle = 'rgba(88, 166, 255, 0.5)';
        ctx.lineWidth = 2.5;
        
        // Draw y-axis (x = 0)
        const yAxisTop = this.coordinateSystem.mathToPixel(0, bounds.maxY);
        const yAxisBottom = this.coordinateSystem.mathToPixel(0, bounds.minY);
        ctx.beginPath();
        ctx.moveTo(yAxisTop.x, yAxisTop.y);
        ctx.lineTo(yAxisBottom.x, yAxisBottom.y);
        ctx.stroke();
        
        // Draw x-axis (y = 0)
        const xAxisLeft = this.coordinateSystem.mathToPixel(bounds.minX, 0);
        const xAxisRight = this.coordinateSystem.mathToPixel(bounds.maxX, 0);
        ctx.beginPath();
        ctx.moveTo(xAxisLeft.x, xAxisLeft.y);
        ctx.lineTo(xAxisRight.x, xAxisRight.y);
        ctx.stroke();
        
        // Draw axis labels
        this._drawAxisLabels(ctx, spacing, bounds);
    }
    
    /**
     * Draw numeric labels on axes
     * @private
     * @param {CanvasRenderingContext2D} ctx - Canvas rendering context
     * @param {number} spacing - Grid spacing in math units
     * @param {Object} bounds - Visible bounds {minX, maxX, minY, maxY}
     */
    _drawAxisLabels(ctx, spacing, bounds) {
        ctx.fillStyle = 'rgba(139, 148, 158, 0.9)';
        ctx.font = '600 12px -apple-system, BlinkMacSystemFont, "Segoe UI", sans-serif';
        ctx.textAlign = 'center';
        ctx.textBaseline = 'top';
        
        // Check if axes are visible
        const xAxisVisible = bounds.minY <= 0 && bounds.maxY >= 0;
        const yAxisVisible = bounds.minX <= 0 && bounds.maxX >= 0;
        
        // Calculate grid line positions
        const minX = Math.floor(bounds.minX / spacing) * spacing;
        const maxX = Math.ceil(bounds.maxX / spacing) * spacing;
        const minY = Math.floor(bounds.minY / spacing) * spacing;
        const maxY = Math.ceil(bounds.maxY / spacing) * spacing;
        
        // Draw x-axis labels (labels for vertical grid lines)
        if (xAxisVisible) {
            // X-axis is visible - place labels at intersections with x-axis
            for (let x = minX; x <= maxX; x += spacing) {
                if (Math.abs(x) < spacing * 0.001) continue; // Skip origin
                const pixel = this.coordinateSystem.mathToPixel(x, 0);
                const label = this._formatNumber(x, spacing);
                ctx.fillText(label, pixel.x, pixel.y + 5);
            }
        } else {
            // X-axis is off-screen - place labels at screen edge closest to x-axis
            const labelY = bounds.maxY < 0 ? bounds.maxY : bounds.minY;
            for (let x = minX; x <= maxX; x += spacing) {
                if (Math.abs(x) < spacing * 0.001) continue;
                const pixel = this.coordinateSystem.mathToPixel(x, labelY);
                const label = this._formatNumber(x, spacing);
                ctx.fillText(label, pixel.x, pixel.y + (bounds.maxY < 0 ? -15 : 5));
            }
        }
        
        // Draw y-axis labels (labels for horizontal grid lines)
        ctx.textAlign = 'right';
        ctx.textBaseline = 'middle';
        
        if (yAxisVisible) {
            // Y-axis is visible - place labels at intersections with y-axis
            for (let y = minY; y <= maxY; y += spacing) {
                if (Math.abs(y) < spacing * 0.001) continue; // Skip origin
                const pixel = this.coordinateSystem.mathToPixel(0, y);
                const label = this._formatNumber(y, spacing);
                ctx.fillText(label, pixel.x - 5, pixel.y);
            }
        } else {
            // Y-axis is off-screen - place labels at screen edge closest to y-axis
            const labelX = bounds.maxX < 0 ? bounds.maxX : bounds.minX;
            for (let y = minY; y <= maxY; y += spacing) {
                if (Math.abs(y) < spacing * 0.001) continue;
                const pixel = this.coordinateSystem.mathToPixel(labelX, y);
                const label = this._formatNumber(y, spacing);
                ctx.fillText(label, pixel.x + (bounds.maxX < 0 ? -5 : 5), pixel.y);
            }
        }
    }
    
    /**
     * Format a number for display based on the current scale
     * @private
     * @param {number} value - The number to format
     * @param {number} spacing - Current grid spacing
     * @returns {string} Formatted number string
     */
    _formatNumber(value, spacing) {
        // Determine appropriate decimal places based on spacing
        if (spacing >= 1) {
            return value.toFixed(0);
        } else if (spacing >= 0.1) {
            return value.toFixed(1);
        } else if (spacing >= 0.01) {
            return value.toFixed(2);
        } else {
            // Use exponential notation for very small spacings
            return value.toExponential(1);
        }
    }
}

export { GridRenderer };
