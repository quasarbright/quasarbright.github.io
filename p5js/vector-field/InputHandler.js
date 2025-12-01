/**
 * InputHandler Module
 * Processes user interactions (mouse, touch, keyboard) on canvas
 * Handles zoom, pan, and particle creation
 */

class InputHandler {
    /**
     * Create an input handler
     * @param {HTMLCanvasElement} canvas - Canvas element to attach event listeners to
     * @param {CoordinateSystem} coordinateSystem - Coordinate system for transformations
     * @param {ParticleSystem} particleSystem - Particle system for particle creation
     */
    constructor(canvas, coordinateSystem, particleSystem) {
        this.canvas = canvas;
        this.coordinateSystem = coordinateSystem;
        this.particleSystem = particleSystem;
        
        // Interaction state
        this.mousePosition = { x: 0, y: 0 };
        this.isDragging = false;
        this.dragStartPosition = { x: 0, y: 0 };
        this.brushMode = false; // false = click mode, true = draw mode
        this.isPanning = false; // true when shift+drag or middle mouse
        
        // Brush settings
        this.brushThickness = 5.0; // in math units
        this.brushDensity = 3.0; // particles per brush stroke segment
        
        // Touch state for pinch gestures
        this.touches = [];
        this.lastPinchDistance = null;
        
        // Bind event handlers
        this.setupEventListeners();
    }
    
    /**
     * Set up all event listeners
     */
    setupEventListeners() {
        // Mouse events
        this.canvas.addEventListener('mousedown', (e) => this.handleMouseDown(e));
        this.canvas.addEventListener('mousemove', (e) => this.handleMouseMove(e));
        this.canvas.addEventListener('mouseup', (e) => this.handleMouseUp(e));
        this.canvas.addEventListener('wheel', (e) => this.handleWheel(e));
        
        // Touch events
        this.canvas.addEventListener('touchstart', (e) => this.handleTouchStart(e));
        this.canvas.addEventListener('touchmove', (e) => this.handleTouchMove(e));
        this.canvas.addEventListener('touchend', (e) => this.handleTouchEnd(e));
        
        // Prevent context menu on right click
        this.canvas.addEventListener('contextmenu', (e) => e.preventDefault());
    }
    
    /**
     * Set brush mode
     * @param {boolean} enabled - True for draw mode, false for click mode
     */
    setBrushMode(enabled) {
        this.brushMode = enabled;
    }
    
    /**
     * Set brush thickness
     * @param {number} thickness - Brush thickness in math units
     */
    setBrushThickness(thickness) {
        this.brushThickness = thickness;
    }
    
    /**
     * Set brush density
     * @param {number} density - Number of particles per brush stroke segment
     */
    setBrushDensity(density) {
        this.brushDensity = density;
    }
    
    /**
     * Handle mouse down event
     * @param {MouseEvent} event - Mouse event
     */
    handleMouseDown(event) {
        const rect = this.canvas.getBoundingClientRect();
        const pixelX = event.clientX - rect.left;
        const pixelY = event.clientY - rect.top;
        
        // Update mouse position
        this.mousePosition = { x: pixelX, y: pixelY };
        this.dragStartPosition = { x: pixelX, y: pixelY };
        
        // Check if panning mode (shift key or middle mouse button)
        if (event.shiftKey || event.button === 1) {
            this.isPanning = true;
            this.isDragging = true;
        } else if (event.button === 0) { // Left mouse button
            this.isDragging = true;
        }
    }
    
    /**
     * Handle mouse move event
     * @param {MouseEvent} event - Mouse event
     */
    handleMouseMove(event) {
        const rect = this.canvas.getBoundingClientRect();
        const pixelX = event.clientX - rect.left;
        const pixelY = event.clientY - rect.top;
        
        const previousPosition = { ...this.mousePosition };
        this.mousePosition = { x: pixelX, y: pixelY };
        
        // Handle dragging
        if (this.isDragging) {
            const deltaX = pixelX - previousPosition.x;
            const deltaY = pixelY - previousPosition.y;
            
            // Check if we've moved enough to be considered a drag (not just a click)
            const dragDistance = Math.sqrt(
                Math.pow(pixelX - this.dragStartPosition.x, 2) +
                Math.pow(pixelY - this.dragStartPosition.y, 2)
            );
            
            // Only process as drag if moved more than 3 pixels
            if (dragDistance > 3) {
                // Handle panning (shift+drag or middle mouse)
                if (this.isPanning) {
                    // Pan the view by the drag delta
                    this.coordinateSystem.pan(deltaX, deltaY);
                }
                // Handle brush drawing (when in brush mode and not panning)
                else if (this.brushMode) {
                    // Convert pixel coordinates to math coordinates
                    const mathCoords = this.coordinateSystem.pixelToMath(pixelX, pixelY);
                    
                    // Spawn particles along the brush path
                    this.particleSystem.addParticlesInBrush(
                        mathCoords.x,
                        mathCoords.y,
                        this.brushThickness,
                        this.brushDensity
                    );
                }
            }
        }
    }
    
    /**
     * Handle mouse up event
     * @param {MouseEvent} event - Mouse event
     */
    handleMouseUp(event) {
        const rect = this.canvas.getBoundingClientRect();
        const pixelX = event.clientX - rect.left;
        const pixelY = event.clientY - rect.top;
        
        // Calculate drag distance to distinguish click from drag
        const dragDistance = Math.sqrt(
            Math.pow(pixelX - this.dragStartPosition.x, 2) +
            Math.pow(pixelY - this.dragStartPosition.y, 2)
        );
        
        // If drag distance is small, treat as click
        const wasClick = dragDistance <= 3;
        
        // Handle click particle creation (when not in brush mode and not panning)
        if (wasClick && !this.brushMode && !this.isPanning && event.button === 0) {
            // Convert pixel coordinates to math coordinates
            const mathCoords = this.coordinateSystem.pixelToMath(pixelX, pixelY);
            
            // Create particle at click location
            this.particleSystem.addParticle(mathCoords.x, mathCoords.y);
        }
        
        // Reset dragging state
        this.isDragging = false;
        this.isPanning = false;
        
        // Store whether this was a click for use in other handlers
        this.wasClick = wasClick;
    }
    
    /**
     * Handle wheel event for zooming and panning
     * @param {WheelEvent} event - Wheel event
     */
    handleWheel(event) {
        // Prevent default scroll behavior
        event.preventDefault();
        
        // Get mouse position relative to canvas
        const rect = this.canvas.getBoundingClientRect();
        const pixelX = event.clientX - rect.left;
        const pixelY = event.clientY - rect.top;
        
        // Check if shift key is held for horizontal panning
        if (event.shiftKey) {
            // Pan horizontally
            this.coordinateSystem.pan(-event.deltaY, 0);
        } else if (event.ctrlKey || event.metaKey) {
            // Zoom when ctrl/cmd is held - much more sensitive
            const zoomSpeed = 0.005;
            const zoomFactor = Math.exp(-event.deltaY * zoomSpeed);
            this.coordinateSystem.zoom(zoomFactor, pixelX, pixelY);
        } else {
            // Default: pan vertically and horizontally
            this.coordinateSystem.pan(-event.deltaX, -event.deltaY);
        }
    }
    
    /**
     * Handle touch start event
     * @param {TouchEvent} event - Touch event
     */
    handleTouchStart(event) {
        event.preventDefault();
        
        // Store touch positions
        this.touches = Array.from(event.touches).map(touch => ({
            id: touch.identifier,
            x: touch.clientX,
            y: touch.clientY
        }));
        
        // If single touch, treat like mouse down
        if (this.touches.length === 1) {
            const rect = this.canvas.getBoundingClientRect();
            const pixelX = this.touches[0].x - rect.left;
            const pixelY = this.touches[0].y - rect.top;
            
            this.mousePosition = { x: pixelX, y: pixelY };
            this.dragStartPosition = { x: pixelX, y: pixelY };
            this.isDragging = true;
        }
        
        // If two touches, prepare for pinch
        if (this.touches.length === 2) {
            const dx = this.touches[1].x - this.touches[0].x;
            const dy = this.touches[1].y - this.touches[0].y;
            this.lastPinchDistance = Math.sqrt(dx * dx + dy * dy);
        }
    }
    
    /**
     * Handle touch move event
     * @param {TouchEvent} event - Touch event
     */
    handleTouchMove(event) {
        event.preventDefault();
        
        const newTouches = Array.from(event.touches).map(touch => ({
            id: touch.identifier,
            x: touch.clientX,
            y: touch.clientY
        }));
        
        // Handle pinch gesture (two touches)
        if (newTouches.length === 2 && this.touches.length === 2) {
            const dx = newTouches[1].x - newTouches[0].x;
            const dy = newTouches[1].y - newTouches[0].y;
            const newDistance = Math.sqrt(dx * dx + dy * dy);
            
            if (this.lastPinchDistance) {
                // Calculate zoom factor from pinch distance change with much higher sensitivity
                const distanceRatio = newDistance / this.lastPinchDistance;
                // Amplify the zoom effect significantly: values further from 1.0 get amplified more
                const zoomFactor = Math.pow(distanceRatio, 2.5);
                
                // Get center point between two touches
                const rect = this.canvas.getBoundingClientRect();
                const centerX = ((newTouches[0].x + newTouches[1].x) / 2) - rect.left;
                const centerY = ((newTouches[0].y + newTouches[1].y) / 2) - rect.top;
                
                // Apply zoom
                this.coordinateSystem.zoom(zoomFactor, centerX, centerY);
            }
            
            this.lastPinchDistance = newDistance;
        }
        // Handle single touch drag
        else if (newTouches.length === 1 && this.isDragging) {
            const rect = this.canvas.getBoundingClientRect();
            const pixelX = newTouches[0].x - rect.left;
            const pixelY = newTouches[0].y - rect.top;
            
            const previousPosition = { ...this.mousePosition };
            this.mousePosition = { x: pixelX, y: pixelY };
            
            // Handle brush drawing if in brush mode
            if (this.brushMode) {
                const mathCoords = this.coordinateSystem.pixelToMath(pixelX, pixelY);
                this.particleSystem.addParticlesInBrush(
                    mathCoords.x,
                    mathCoords.y,
                    this.brushThickness,
                    this.brushDensity
                );
            }
        }
        
        this.touches = newTouches;
    }
    
    /**
     * Handle touch end event
     * @param {TouchEvent} event - Touch event
     */
    handleTouchEnd(event) {
        event.preventDefault();
        
        const remainingTouches = Array.from(event.touches).map(touch => ({
            id: touch.identifier,
            x: touch.clientX,
            y: touch.clientY
        }));
        
        // If all touches ended
        if (remainingTouches.length === 0) {
            // Check if it was a tap (similar to click)
            if (this.touches.length === 1) {
                const rect = this.canvas.getBoundingClientRect();
                const pixelX = this.touches[0].x - rect.left;
                const pixelY = this.touches[0].y - rect.top;
                
                const dragDistance = Math.sqrt(
                    Math.pow(pixelX - this.dragStartPosition.x, 2) +
                    Math.pow(pixelY - this.dragStartPosition.y, 2)
                );
                
                // If tap (not drag) and not in brush mode, create particle
                if (dragDistance <= 10 && !this.brushMode) {
                    const mathCoords = this.coordinateSystem.pixelToMath(pixelX, pixelY);
                    this.particleSystem.addParticle(mathCoords.x, mathCoords.y);
                }
            }
            
            this.isDragging = false;
            this.lastPinchDistance = null;
        }
        
        this.touches = remainingTouches;
    }
}

export { InputHandler };
