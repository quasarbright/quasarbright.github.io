/**
 * Vector Field Visualizer
 * Main application entry point
 */

import { CoordinateSystem } from './CoordinateSystem.js';
import { ExpressionEvaluator } from './ExpressionEvaluator.js';
import { GridRenderer } from './GridRenderer.js';
import { VectorRenderer } from './VectorRenderer.js';
import { ParticleSystem } from './ParticleSystem.js';
import { InputHandler } from './InputHandler.js';
import { UIController } from './UIController.js';

class VectorFieldApp {
    constructor() {
        // Get canvas and context
        this.canvas = document.getElementById('canvas');
        this.ctx = this.canvas.getContext('2d');
        
        // Initialize canvas size
        this.handleResize();
        
        // Create coordinate system
        this.coordinateSystem = new CoordinateSystem(
            this.canvas.width,
            this.canvas.height
        );
        
        // Set up default vector field (vx = -y, vy = x - a rotation field)
        this.expressionEvaluator = new ExpressionEvaluator('-y', 'x');
        
        // Create renderers
        this.gridRenderer = new GridRenderer(this.coordinateSystem, this.canvas);
        this.vectorRenderer = new VectorRenderer(
            this.coordinateSystem,
            this.expressionEvaluator
        );
        
        // Create particle system
        this.particleSystem = new ParticleSystem(
            this.coordinateSystem,
            this.expressionEvaluator
        );
        
        // Create input handler
        this.inputHandler = new InputHandler(
            this.canvas,
            this.coordinateSystem,
            this.particleSystem
        );
        
        // Create UI controller with callback for expression updates
        this.uiController = new UIController(
            this.particleSystem,
            this.expressionEvaluator,
            this.coordinateSystem,
            (xExpr, yExpr) => this.updateExpressions(xExpr, yExpr)
        );
        
        // Connect UI controller to input handler for brush settings sync
        this.uiController.inputHandler = this.inputHandler;
        // Connect UI controller to grid renderer for cache invalidation
        this.uiController.gridRenderer = this.gridRenderer;
        
        // Set up resize handler
        window.addEventListener('resize', () => this.handleResize());
        
        // Time tracking for physics integration
        this.lastTime = null;
        
        console.log('Vector Field Visualizer initialized');
        console.log('Canvas size:', this.canvas.width, 'x', this.canvas.height);
    }
    
    /**
     * Update vector field expressions
     * @param {string} xExpr - X component expression
     * @param {string} yExpr - Y component expression
     * @returns {ExpressionEvaluator} New expression evaluator
     */
    updateExpressions(xExpr, yExpr) {
        // Create new expression evaluator
        this.expressionEvaluator = new ExpressionEvaluator(xExpr, yExpr);
        
        // Update references in renderers and particle system
        this.vectorRenderer.expressionEvaluator = this.expressionEvaluator;
        this.particleSystem.expressionEvaluator = this.expressionEvaluator;
        
        return this.expressionEvaluator;
    }
    
    /**
     * Handle canvas resize
     * Updates canvas dimensions and coordinate system
     */
    handleResize() {
        // Set canvas size to match viewport
        this.canvas.width = window.innerWidth;
        this.canvas.height = window.innerHeight;
        
        // Update coordinate system dimensions
        if (this.coordinateSystem) {
            this.coordinateSystem.updateDimensions(
                this.canvas.width,
                this.canvas.height
            );
        }
    }
    
    resizeCanvas() {
        // Set canvas size to match viewport
        this.canvas.width = window.innerWidth;
        this.canvas.height = window.innerHeight;
    }
    
    /**
     * Main animation loop
     * Combines update and render, calculates deltaTime
     * @param {number} currentTime - Current timestamp from requestAnimationFrame
     */
    animate(currentTime) {
        // Calculate deltaTime in seconds
        let deltaTime = 0;
        if (this.lastTime !== null) {
            deltaTime = (currentTime - this.lastTime) / 1000; // Convert ms to seconds
            
            // Cap deltaTime to prevent large jumps (e.g., when tab is inactive)
            deltaTime = Math.min(deltaTime, 0.1); // Max 100ms
        }
        this.lastTime = currentTime;
        
        // Update physics
        this.update(deltaTime);
        
        // Render frame
        this.render();
        
        // Request next frame
        requestAnimationFrame((time) => this.animate(time));
    }
    
    start() {
        console.log('Application started');
        // Start animation loop
        requestAnimationFrame((time) => this.animate(time));
    }
    
    /**
     * Update loop - updates physics simulation
     * @param {number} deltaTime - Time elapsed since last update in seconds
     */
    update(deltaTime) {
        // Get pause state from UI controller
        const isPaused = this.uiController.getPauseState();
        
        // Update particle system physics
        this.particleSystem.update(deltaTime, isPaused);
    }
    
    /**
     * Render loop - draws all visual elements
     * Uses requestAnimationFrame for smooth rendering
     */
    render() {
        // Clear canvas
        this.ctx.fillStyle = '#0a0a0a';
        this.ctx.fillRect(0, 0, this.canvas.width, this.canvas.height);
        
        // Render grid lines and axes
        this.gridRenderer.render(this.ctx);
        
        // Render vector field arrows
        this.vectorRenderer.render(this.ctx);
        
        // Render particles and trails
        this.particleSystem.render(this.ctx);
        
        // Render brush cursor (always visible)
        if (this.inputHandler.mousePosition) {
            this.renderBrushCursor(this.ctx);
        }
    }
    
    /**
     * Render brush cursor showing the brush area
     * @param {CanvasRenderingContext2D} ctx - Canvas rendering context
     */
    renderBrushCursor(ctx) {
        const mousePos = this.inputHandler.mousePosition;
        
        // Brush thickness is diameter, so divide by 2 to get radius
        // Then convert from math units to pixels
        const radiusPixels = (this.inputHandler.brushThickness / 2) * this.coordinateSystem.zoomLevel;
        
        // Draw hollow circle
        ctx.strokeStyle = 'rgba(121, 192, 255, 0.6)';
        ctx.lineWidth = 2;
        ctx.beginPath();
        ctx.arc(mousePos.x, mousePos.y, radiusPixels, 0, Math.PI * 2);
        ctx.stroke();
        
        // Draw crosshair at center
        ctx.strokeStyle = 'rgba(121, 192, 255, 0.4)';
        ctx.lineWidth = 1;
        const crosshairSize = 8;
        ctx.beginPath();
        ctx.moveTo(mousePos.x - crosshairSize, mousePos.y);
        ctx.lineTo(mousePos.x + crosshairSize, mousePos.y);
        ctx.moveTo(mousePos.x, mousePos.y - crosshairSize);
        ctx.lineTo(mousePos.x, mousePos.y + crosshairSize);
        ctx.stroke();
    }
}

// Initialize application when DOM is loaded
document.addEventListener('DOMContentLoaded', () => {
    const app = new VectorFieldApp();
    
    // Set up event listeners for window resize
    // (already done in constructor, but ensuring it's clear)
    
    // Initialize UI
    app.uiController.setupMathFields();
    app.uiController.setupEventListeners();
    
    // Sync input handler with UI controller brush settings
    app.inputHandler.setBrushMode(true); // Always in brush mode
    app.inputHandler.setBrushThickness(app.uiController.getBrushThickness());
    app.inputHandler.setBrushDensity(3.0); // Fixed density
    
    // Start animation loop
    app.start();
});

// Export for module usage in tests
export { VectorFieldApp };
