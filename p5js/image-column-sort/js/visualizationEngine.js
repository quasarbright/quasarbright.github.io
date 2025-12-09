/**
 * @fileoverview VisualizationEngine module for animating sorting algorithm steps.
 * 
 * This module manages the animation loop that processes sorting steps sequentially,
 * coordinating with the image processor, highlight manager, and audio generator
 * to create a smooth visualization of the sorting process.
 */

import { renderColumnsAt } from './imageProcessor.js';
import { swapColumns } from './types.js';

/**
 * VisualizationEngine class manages the sorting animation and step execution.
 */
export class VisualizationEngine {
    /**
     * @param {HTMLCanvasElement} canvas - Canvas element for rendering
     * @param {HTMLImageElement} sourceImage - Source image for column rendering
     * @param {import('./highlightManager.js').HighlightManager} highlightManager - Highlight manager instance
     * @param {import('./audioGenerator.js').AudioGenerator} audioGenerator - Audio generator instance
     */
    constructor(canvas, sourceImage, highlightManager, audioGenerator) {
        this.canvas = canvas;
        this.sourceImage = sourceImage;
        this.highlightManager = highlightManager;
        this.audioGenerator = audioGenerator;
        
        // Animation state
        this.isPlaying = false;
        this.isPaused = false;
        this.currentStepIndex = 0;
        this.targetStepsPerSecond = 60; // Target steps per second
        this.actualStepsPerSecond = 0; // Measured actual rate
        
        // Sorting data
        /** @type {import('./types.js').SortingStep[]} */
        this.steps = [];
        /** @type {number[]} */
        this.columnOrder = [];
        /** @type {number[]} */
        this.scrambledOrder = [];
        
        // Animation frame tracking
        this.animationFrameId = null;
        this.lastFrameTime = 0;
        this.stepsExecutedInWindow = 0;
        this.measurementWindowStart = 0;
        
        // Statistics tracking
        this.compareCount = 0;
        this.swapCount = 0;
        
        // Callbacks
        this.onComplete = null;
        this.onStepExecuted = null;
    }
    
    /**
     * Starts the visualization with the given sorting steps and column order.
     * 
     * @param {import('./types.js').SortingStep[]} steps - Array of sorting steps to visualize
     * @param {number[]} columnOrder - Current column order array (will be modified during sorting)
     * @param {number[]} scrambledOrder - Scrambled state to restore on reset
     */
    start(steps, columnOrder, scrambledOrder) {
        // Stop any ongoing animation
        this.stop();
        
        // Initialize state
        this.steps = steps;
        this.columnOrder = columnOrder;
        this.scrambledOrder = [...scrambledOrder];
        this.currentStepIndex = 0;
        this.isPlaying = true;
        this.isPaused = false;
        this.lastFrameTime = performance.now();
        this.measurementWindowStart = performance.now();
        this.stepsExecutedInWindow = 0;
        
        // Reset statistics
        this.compareCount = 0;
        this.swapCount = 0;
        this._updateStats();
        
        // Start animation loop
        this._animate();
    }
    
    /**
     * Main animation loop using requestAnimationFrame.
     * Dynamically adjusts steps per frame to achieve target steps/second.
     * Batches rendering updates for efficiency when executing multiple steps per frame.
     * 
     * @private
     */
    _animate() {
        if (!this.isPlaying) {
            return;
        }
        
        // Schedule next frame
        this.animationFrameId = requestAnimationFrame(() => this._animate());
        
        // Check if paused
        if (this.isPaused) {
            this.lastFrameTime = performance.now();
            this.measurementWindowStart = performance.now();
            this.stepsExecutedInWindow = 0;
            return;
        }
        
        const currentTime = performance.now();
        const frameDelta = currentTime - this.lastFrameTime;
        this.lastFrameTime = currentTime;
        
        // Calculate how many steps to execute this frame to hit target rate
        // Target: stepsPerSecond steps per 1000ms
        // This frame: (frameDelta / 1000) * stepsPerSecond steps
        const targetStepsThisFrame = Math.max(1, Math.round((frameDelta / 1000) * this.targetStepsPerSecond));
        
        // Track columns that need to be redrawn this frame
        const columnsToRedraw = new Set();
        let lastStepType = null;
        
        // Execute steps for this frame
        let stepsExecuted = 0;
        for (let i = 0; i < targetStepsThisFrame; i++) {
            // Check if we've completed all steps
            if (this.currentStepIndex >= this.steps.length) {
                // Render any pending updates before completing
                if (columnsToRedraw.size > 0) {
                    this._batchRender(columnsToRedraw);
                }
                this._onAnimationComplete();
                return;
            }
            
            // Execute current step (without rendering)
            const step = this.steps[this.currentStepIndex];
            const affectedColumns = this._executeStepNoRender(step);
            
            // Track affected columns for batch rendering
            if (affectedColumns) {
                affectedColumns.forEach(col => columnsToRedraw.add(col));
            }
            
            lastStepType = step.type;
            
            // Move to next step
            this.currentStepIndex++;
            stepsExecuted++;
            
            // Notify callback if set
            if (this.onStepExecuted) {
                this.onStepExecuted(step, this.currentStepIndex);
            }
        }
        
        // Batch render all affected columns once per frame
        if (columnsToRedraw.size > 0) {
            this._batchRender(columnsToRedraw);
        }
        
        // Track actual performance over a measurement window
        this.stepsExecutedInWindow += stepsExecuted;
        const windowDuration = currentTime - this.measurementWindowStart;
        
        // Update measurement every 500ms
        if (windowDuration >= 500) {
            this.actualStepsPerSecond = Math.round((this.stepsExecutedInWindow / windowDuration) * 1000);
            this.measurementWindowStart = currentTime;
            this.stepsExecutedInWindow = 0;
        }
    }
    
    /**
     * Pauses the animation at the current step.
     * The animation can be resumed from this state.
     */
    pause() {
        if (!this.isPlaying || this.isPaused) {
            return; // Already paused or not playing
        }
        
        this.isPaused = true;
        
        // Stop any currently playing audio
        this.audioGenerator.stopCurrentTone();
    }
    
    /**
     * Resumes the animation from a paused state.
     */
    resume() {
        if (!this.isPlaying || !this.isPaused) {
            return; // Not paused or not playing
        }
        
        this.isPaused = false;
        this.lastFrameTime = performance.now(); // Reset timing
        this.measurementWindowStart = performance.now();
        this.stepsExecutedInWindow = 0;
    }
    
    /**
     * Checks if the animation is currently paused.
     * 
     * @returns {boolean} True if paused
     */
    isPausedState() {
        return this.isPaused;
    }
    
    /**
     * Checks if the animation is currently playing (including paused state).
     * 
     * @returns {boolean} True if playing
     */
    isPlayingState() {
        return this.isPlaying;
    }
    
    /**
     * Sets the target steps per second.
     * The change takes effect immediately, even during active animation.
     * 
     * @param {number} stepsPerSec - Target steps per second
     */
    setStepsPerSecond(stepsPerSec) {
        if (stepsPerSec < 1) {
            console.warn('Steps per second must be at least 1');
            this.targetStepsPerSecond = 1;
        } else {
            this.targetStepsPerSecond = stepsPerSec;
        }
    }
    
    /**
     * Gets the target steps per second.
     * 
     * @returns {number} Target steps per second
     */
    getStepsPerSecond() {
        return this.targetStepsPerSecond;
    }
    
    /**
     * Gets the actual measured steps per second.
     * 
     * @returns {number} Actual steps per second
     */
    getActualStepsPerSecond() {
        return this.actualStepsPerSecond;
    }
    
    /**
     * Resets the visualization to the scrambled state.
     * Stops any ongoing animation and restores the column order to the scrambled state.
     * 
     * @returns {number[]} The restored scrambled column order
     */
    reset() {
        // Stop any ongoing animation
        this.stop();
        
        // Clear all highlights
        this.highlightManager.clearHighlights();
        
        // Restore the scrambled order
        for (let i = 0; i < this.scrambledOrder.length; i++) {
            this.columnOrder[i] = this.scrambledOrder[i];
        }
        
        // Reset step index
        this.currentStepIndex = 0;
        
        // Reset statistics
        this.compareCount = 0;
        this.swapCount = 0;
        this._updateStats();
        
        // Return the restored order for the caller to re-render if needed
        return this.columnOrder;
    }
    
    /**
     * Stops the animation completely.
     */
    stop() {
        this.isPlaying = false;
        this.isPaused = false;
        
        if (this.animationFrameId !== null) {
            cancelAnimationFrame(this.animationFrameId);
            this.animationFrameId = null;
        }
        
        // Stop any currently playing audio
        this.audioGenerator.stopCurrentTone();
    }
    
    /**
     * Called when animation completes all steps.
     * 
     * @private
     */
    _onAnimationComplete() {
        this.stop();
        
        if (this.onComplete) {
            this.onComplete();
        }
    }
    
    /**
     * Executes a single sorting step without rendering.
     * Returns the affected column indices for batch rendering.
     * 
     * @private
     * @param {import('./types.js').SortingStep} step - Step to execute
     * @returns {number[]|null} Array of affected column indices, or null if no rendering needed
     */
    _executeStepNoRender(step) {
        switch (step.type) {
            case 'compare':
                return this._executeCompareStepNoRender(step.indices);
            
            case 'swap':
                return this._executeSwapStepNoRender(step.indices);
            
            case 'complete':
                return this._executeCompleteStepNoRender();
            
            default:
                console.warn('Unknown step type:', step);
                return null;
        }
    }
    
    /**
     * Batch renders all affected columns at once.
     * 
     * @private
     * @param {Set<number>} columnIndices - Set of column indices to redraw
     */
    _batchRender(columnIndices) {
        try {
            const indices = Array.from(columnIndices);
            
            // Redraw all affected columns
            renderColumnsAt(this.canvas, this.sourceImage, this.columnOrder, indices);
            
            // Render highlights on top
            this.highlightManager.renderHighlights(
                this.canvas,
                this.sourceImage,
                this.columnOrder.length
            );
        } catch (error) {
            console.error('Rendering error during visualization:', error);
            // Stop animation on rendering failure
            this.stop();
            throw error;
        }
    }
    
    /**
     * Executes a compare step without rendering.
     * 
     * @private
     * @param {[number, number]} indices - Indices of columns being compared
     * @returns {number[]} Array of affected column indices
     */
    _executeCompareStepNoRender(indices) {
        const [i, j] = indices;
        
        // Increment comparison counter
        this.compareCount++;
        this._updateStats();
        
        // Get previous highlights to redraw
        const previousHighlights = [
            ...this.highlightManager.getComparisonHighlights(),
            ...this.highlightManager.getOperationHighlights()
        ];
        
        // Clear all highlights first, then set only comparison highlight (green)
        this.highlightManager.clearHighlights();
        this.highlightManager.setComparisonHighlight([i, j]);
        
        // Play audio for the comparison if enabled
        if (this.audioGenerator.isEnabled()) {
            // Play tone based on the higher position index
            const maxIndex = Math.max(i, j);
            const frequency = this.audioGenerator.calculateFrequency(
                maxIndex,
                this.columnOrder.length
            );
            this.audioGenerator.playTone(frequency, 20); // Short duration for batched steps
        }
        
        // Return all columns that need redrawing
        return [...previousHighlights, i, j];
    }
    
    /**
     * Executes a swap step without rendering.
     * 
     * @private
     * @param {[number, number]} indices - Indices of columns to swap
     * @returns {number[]} Array of affected column indices
     */
    _executeSwapStepNoRender(indices) {
        const [i, j] = indices;
        
        // Increment swap counter
        this.swapCount++;
        this._updateStats();
        
        // Get previous highlights to redraw
        const previousHighlights = [
            ...this.highlightManager.getComparisonHighlights(),
            ...this.highlightManager.getOperationHighlights()
        ];
        
        // Perform the swap in the column order array
        swapColumns(this.columnOrder, i, j);
        
        // Clear all highlights first, then set only operation highlight (red)
        this.highlightManager.clearHighlights();
        this.highlightManager.setOperationHighlight([i, j]);
        
        // Play audio for the swap if enabled
        if (this.audioGenerator.isEnabled()) {
            // Play tone based on the higher position index
            const maxIndex = Math.max(i, j);
            const frequency = this.audioGenerator.calculateFrequency(
                maxIndex,
                this.columnOrder.length
            );
            this.audioGenerator.playTone(frequency, 20); // Short duration for batched steps
        }
        
        // Return all columns that need redrawing
        return [...previousHighlights, i, j];
    }
    
    /**
     * Executes a complete step without rendering.
     * 
     * @private
     * @returns {number[]} Array of affected column indices
     */
    _executeCompleteStepNoRender() {
        // Get all currently highlighted columns before clearing
        const previousHighlights = [
            ...this.highlightManager.getComparisonHighlights(),
            ...this.highlightManager.getOperationHighlights()
        ];
        
        // Clear all highlights
        this.highlightManager.clearHighlights();
        
        // Return columns that need redrawing
        return previousHighlights;
    }
    
    /**
     * Updates the statistics display in the UI.
     * Shows stats unless hidden by endless mode.
     * 
     * @private
     */
    _updateStats() {
        // Check if we're in a browser environment
        if (typeof document === 'undefined') {
            return;
        }
        
        const statsInfo = document.getElementById('statsInfo');
        const imageWidth = document.getElementById('imageWidth');
        const compareCount = document.getElementById('compareCount');
        const swapCount = document.getElementById('swapCount');
        
        if (statsInfo && imageWidth && compareCount && swapCount) {
            // Only show stats if not hidden by endless mode
            if (statsInfo.dataset.endlessHidden !== 'true') {
                statsInfo.style.display = 'block';
            }
            
            imageWidth.textContent = this.columnOrder.length.toString();
            compareCount.textContent = this.compareCount.toString();
            swapCount.textContent = this.swapCount.toString();
        }
    }
}
