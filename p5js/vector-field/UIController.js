/**
 * UIController Module
 * Manages UI elements, user interactions, and application state
 */

class UIController {
    /**
     * Create a UI controller
     * @param {ParticleSystem} particleSystem - Particle system instance
     * @param {ExpressionEvaluator} expressionEvaluator - Expression evaluator instance
     * @param {CoordinateSystem} coordinateSystem - Coordinate system instance
     * @param {Function} onExpressionUpdate - Callback when expressions are updated
     */
    constructor(particleSystem, expressionEvaluator, coordinateSystem, onExpressionUpdate) {
        this.particleSystem = particleSystem;
        this.expressionEvaluator = expressionEvaluator;
        this.coordinateSystem = coordinateSystem;
        this.onExpressionUpdate = onExpressionUpdate;
        
        // Reference to input handler (will be set by main app)
        this.inputHandler = null;
        // Reference to grid renderer (will be set by main app)
        this.gridRenderer = null;
        
        // UI element references
        this.vxInput = null;
        this.vyInput = null;
        this.updateBtn = null;
        this.errorMessage = null;
        this.pauseBtn = null;
        this.spawnGridBtn = null;
        this.clearBtn = null;
        this.thicknessSlider = null;
        this.thicknessValue = null;
        this.gridLinesSlider = null;
        this.gridLinesValue = null;
        
        // Application state
        this.isPaused = false;
        this.brushThickness = 5.0;
    }
    
    /**
     * Set up MathLive math field editors
     * Initializes the math input fields with proper configuration
     */
    setupMathFields() {
        // Get references to math-field elements
        this.vxInput = document.getElementById('vx-input');
        this.vyInput = document.getElementById('vy-input');
        
        if (!this.vxInput || !this.vyInput) {
            console.error('MathLive fields not found in DOM');
            return;
        }
        
        // Configure MathLive options for both fields
        const mathFieldConfig = {
            // Virtual keyboard configuration
            virtualKeyboardMode: 'manual',
            
            // Customize available functions and symbols
            virtualKeyboards: 'numeric functions symbols',
            
            // Enable smart mode for easier input
            smartMode: true,
            
            // Inline shortcuts for common operations
            inlineShortcuts: {
                'pi': '\\pi',
                'sqrt': '\\sqrt',
                'sin': '\\sin',
                'cos': '\\cos',
                'tan': '\\tan',
                'exp': '\\exp',
                'ln': '\\ln',
                'log': '\\log'
            }
        };
        
        // Apply configuration to both fields
        Object.assign(this.vxInput, mathFieldConfig);
        Object.assign(this.vyInput, mathFieldConfig);
        
        // Set up event listeners for real-time updates (optional)
        // For now, we'll rely on the Update button
    }
    
    /**
     * Update vector field expressions from MathLive inputs
     * Converts LaTeX to math.js format and validates
     * @param {string} xExpr - X component expression (math.js format)
     * @param {string} yExpr - Y component expression (math.js format)
     */
    updateExpressions(xExpr, yExpr) {
        // Get error message element if not already cached
        if (!this.errorMessage) {
            this.errorMessage = document.getElementById('error-message');
        }
        
        // Call the callback to update the expression evaluator
        // The callback should create a new ExpressionEvaluator and update the system
        if (this.onExpressionUpdate) {
            const result = this.onExpressionUpdate(xExpr, yExpr);
            
            // Display validation errors if invalid
            if (result && !result.isValid()) {
                this.showError(result.getError());
            } else {
                this.hideError();
            }
        }
    }
    
    /**
     * Convert LaTeX from MathLive to math.js compatible format
     * @param {string} latex - LaTeX expression from MathLive
     * @returns {string} math.js compatible expression
     */
    latexToMathJS(latex) {
        // Basic LaTeX to math.js conversions
        let expr = latex;
        
        // Remove LaTeX spacing commands
        expr = expr.replace(/\\,/g, '');
        expr = expr.replace(/\\:/g, '');
        expr = expr.replace(/\\;/g, '');
        expr = expr.replace(/\\ /g, '');
        
        // Convert fractions: \frac{a}{b} -> (a)/(b)
        expr = expr.replace(/\\frac\{([^}]+)\}\{([^}]+)\}/g, '(($1)/($2))');
        
        // Convert square root: \sqrt{x} -> sqrt(x)
        expr = expr.replace(/\\sqrt\{([^}]+)\}/g, 'sqrt($1)');
        
        // Convert powers: x^{2} -> x^(2) or x^2 -> x^2
        expr = expr.replace(/\^{([^}]+)}/g, '^($1)');
        
        // Convert trigonometric functions
        expr = expr.replace(/\\sin/g, 'sin');
        expr = expr.replace(/\\cos/g, 'cos');
        expr = expr.replace(/\\tan/g, 'tan');
        
        // Convert logarithms
        expr = expr.replace(/\\ln/g, 'ln');
        expr = expr.replace(/\\log/g, 'log');
        
        // Convert exponential
        expr = expr.replace(/\\exp/g, 'exp');
        
        // Convert pi
        expr = expr.replace(/\\pi/g, 'pi');
        
        // Convert absolute value: \left|x\right| -> abs(x)
        expr = expr.replace(/\\left\|([^|]+)\\right\|/g, 'abs($1)');
        
        // Remove remaining LaTeX commands (left, right, etc.)
        expr = expr.replace(/\\left/g, '');
        expr = expr.replace(/\\right/g, '');
        
        // Convert implicit multiplication: 2x -> 2*x
        expr = expr.replace(/(\d)([a-zA-Z])/g, '$1*$2');
        
        return expr;
    }
    
    /**
     * Show error message in UI
     * @param {string} message - Error message to display
     */
    showError(message) {
        if (this.errorMessage) {
            this.errorMessage.textContent = message;
            this.errorMessage.classList.add('visible');
        }
    }
    
    /**
     * Hide error message in UI
     */
    hideError() {
        if (this.errorMessage) {
            this.errorMessage.textContent = '';
            this.errorMessage.classList.remove('visible');
        }
    }
    
    /**
     * Toggle pause/resume state
     * Updates button text and internal state
     */
    togglePause() {
        this.isPaused = !this.isPaused;
        
        // Update button text
        if (!this.pauseBtn) {
            this.pauseBtn = document.getElementById('pause-btn');
        }
        
        if (this.pauseBtn) {
            this.pauseBtn.textContent = this.isPaused ? '▶ Resume' : '⏸ Pause';
        }
    }
    
    /**
     * Get current pause state
     * @returns {boolean} True if paused
     */
    getPauseState() {
        return this.isPaused;
    }
    
    /**
     * Spawn a grid of particles across the visible area
     */
    spawnParticleGrid() {
        // Get visible bounds from coordinate system
        const bounds = this.coordinateSystem.getVisibleBounds();
        
        // Spawn particles
        const density = 20;
        this.particleSystem.addParticleGrid(bounds, density);
    }
    
    /**
     * Clear all particles from the system
     */
    clearParticles() {
        this.particleSystem.clear();
    }
    
    /**
     * Set brush thickness
     * @param {number} value - Thickness value
     */
    setBrushThickness(value) {
        this.brushThickness = parseFloat(value);
        
        // Update display value
        if (!this.thicknessValue) {
            this.thicknessValue = document.getElementById('thickness-value');
        }
        
        if (this.thicknessValue) {
            this.thicknessValue.textContent = this.brushThickness.toFixed(1);
        }
        
        // Update input handler if connected
        if (this.inputHandler) {
            this.inputHandler.setBrushThickness(this.brushThickness);
        }
    }
    
    /**
     * Get current brush thickness
     * @returns {number} Current brush thickness
     */
    getBrushThickness() {
        return this.brushThickness;
    }
    
    /**
     * Set max grid lines
     * @param {number} value - Max grid lines value
     */
    setMaxGridLines(value) {
        const maxGridLines = parseInt(value);
        
        // Update display value
        if (!this.gridLinesValue) {
            this.gridLinesValue = document.getElementById('grid-lines-value');
        }
        
        if (this.gridLinesValue) {
            this.gridLinesValue.textContent = maxGridLines;
        }
        
        // Update coordinate system
        this.coordinateSystem.maxGridLines = maxGridLines;
        
        // Invalidate grid renderer cache to force re-render
        if (this.gridRenderer) {
            this.gridRenderer.cachedZoom = null;
        }
    }
    
    /**
     * Set up keyboard shortcuts
     * @private
     */
    _setupKeyboardShortcuts() {
        document.addEventListener('keydown', (e) => {
            // Ignore if user is typing in an input field
            if (e.target.tagName === 'INPUT' || 
                e.target.tagName === 'TEXTAREA' ||
                e.target.tagName === 'MATH-FIELD') {
                return;
            }
            
            switch(e.key) {
                case ' ':
                    // Space bar - pause/resume
                    e.preventDefault();
                    this.togglePause();
                    break;
                    
                case 'Escape':
                    // Escape - clear particles
                    e.preventDefault();
                    this.clearParticles();
                    break;
                    
                case '+':
                case '=':
                    // Plus/equals - zoom in
                    e.preventDefault();
                    this._zoomWithKeyboard(1.2);
                    break;
                    
                case '-':
                case '_':
                    // Minus - zoom out
                    e.preventDefault();
                    this._zoomWithKeyboard(0.8);
                    break;
            }
        });
    }
    
    /**
     * Zoom using keyboard (centered on viewport)
     * @private
     * @param {number} factor - Zoom factor
     */
    _zoomWithKeyboard(factor) {
        // Zoom centered on the middle of the viewport
        const centerX = window.innerWidth / 2;
        const centerY = window.innerHeight / 2;
        this.coordinateSystem.zoom(factor, centerX, centerY);
    }
    
    /**
     * Initialize all UI event listeners
     * Should be called after DOM is loaded
     */
    setupEventListeners() {
        // Set up keyboard shortcuts
        this._setupKeyboardShortcuts();
        
        // Get UI element references
        this.updateBtn = document.getElementById('update-btn');
        this.pauseBtn = document.getElementById('pause-btn');
        this.spawnGridBtn = document.getElementById('spawn-grid-btn');
        this.clearBtn = document.getElementById('clear-btn');
        this.thicknessSlider = document.getElementById('thickness-slider');
        this.gridLinesSlider = document.getElementById('grid-lines-slider');
        
        // Update button
        if (this.updateBtn) {
            this.updateBtn.addEventListener('click', () => {
                const vxLatex = this.vxInput.value;
                const vyLatex = this.vyInput.value;
                
                // Convert LaTeX to math.js format
                const vxExpr = this.latexToMathJS(vxLatex);
                const vyExpr = this.latexToMathJS(vyLatex);
                
                this.updateExpressions(vxExpr, vyExpr);
            });
        }
        
        // Pause button
        if (this.pauseBtn) {
            this.pauseBtn.addEventListener('click', () => {
                this.togglePause();
            });
        }
        
        // Spawn grid button
        if (this.spawnGridBtn) {
            this.spawnGridBtn.addEventListener('click', () => {
                this.spawnParticleGrid();
            });
        }
        
        // Clear button
        if (this.clearBtn) {
            this.clearBtn.addEventListener('click', () => {
                this.clearParticles();
            });
        }
        
        // Thickness slider
        if (this.thicknessSlider) {
            this.thicknessSlider.addEventListener('input', (e) => {
                this.setBrushThickness(e.target.value);
            });
        }
        
        // Grid lines slider
        if (this.gridLinesSlider) {
            this.gridLinesSlider.addEventListener('input', (e) => {
                this.setMaxGridLines(e.target.value);
            });
        }
        
        // Preset buttons
        const presetButtons = document.querySelectorAll('.preset-btn');
        presetButtons.forEach(btn => {
            btn.addEventListener('click', () => {
                const vx = btn.getAttribute('data-vx');
                const vy = btn.getAttribute('data-vy');
                
                // Update MathLive fields
                if (this.vxInput && this.vyInput) {
                    this.vxInput.value = vx;
                    this.vyInput.value = vy;
                }
                
                // Update expressions
                this.updateExpressions(vx, vy);
            });
        });
    }
}

export { UIController };
