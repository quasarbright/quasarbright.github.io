/**
 * @fileoverview Highlight management module for visual feedback during sorting.
 * 
 * This module manages the visual highlights that appear on columns during sorting operations:
 * - Comparison highlights (green) - shown when two columns are being compared
 * - Operation highlights (red) - shown when columns are being actively manipulated
 * 
 * The HighlightManager maintains highlight state and provides rendering functionality
 * to draw semi-transparent overlays on the canvas.
 */

/**
 * Color constants for different highlight types
 */
const COMPARISON_COLOR = 'rgba(0, 255, 0, 0.3)';  // Semi-transparent green
const OPERATION_COLOR = 'rgba(255, 0, 0, 0.3)';   // Semi-transparent red

/**
 * HighlightManager class manages visual highlights during sorting visualization.
 */
export class HighlightManager {
    constructor() {
        /**
         * Current comparison highlights (green)
         * @type {number[]}
         */
        this.comparisonIndices = [];

        /**
         * Current operation highlights (red)
         * @type {number[]}
         */
        this.operationIndices = [];
    }

    /**
     * Sets the comparison highlight on specified column indices.
     * This applies a green semi-transparent overlay to indicate columns being compared.
     * 
     * @param {number[]} indices - Array of column positions to highlight
     */
    setComparisonHighlight(indices) {
        this.comparisonIndices = [...indices];
    }

    /**
     * Sets the operation highlight on specified column indices.
     * This applies a red semi-transparent overlay to indicate columns being operated on.
     * 
     * @param {number[]} indices - Array of column positions to highlight
     */
    setOperationHighlight(indices) {
        this.operationIndices = [...indices];
    }

    /**
     * Clears all highlights.
     * This removes both comparison and operation highlights.
     */
    clearHighlights() {
        this.comparisonIndices = [];
        this.operationIndices = [];
    }

    /**
     * Gets the current comparison highlight indices.
     * 
     * @returns {number[]} Array of indices with comparison highlights
     */
    getComparisonHighlights() {
        return [...this.comparisonIndices];
    }

    /**
     * Gets the current operation highlight indices.
     * 
     * @returns {number[]} Array of indices with operation highlights
     */
    getOperationHighlights() {
        return [...this.operationIndices];
    }

    /**
     * Checks if there are any active highlights.
     * 
     * @returns {boolean} True if any highlights are active
     */
    hasHighlights() {
        return this.comparisonIndices.length > 0 || this.operationIndices.length > 0;
    }

    /**
     * Renders highlight overlays on the canvas.
     * This draws semi-transparent colored rectangles over the highlighted columns.
     * 
     * @param {HTMLCanvasElement} canvas - The canvas to render highlights on
     * @param {HTMLImageElement} sourceImage - The source image (used to calculate dimensions)
     * @param {number} numColumns - Total number of columns in the image
     */
    renderHighlights(canvas, sourceImage, numColumns) {
        if (!this.hasHighlights()) {
            return;
        }

        const ctx = canvas.getContext('2d');
        const imageWidth = sourceImage.width;
        const imageHeight = sourceImage.height;
        const colWidth = imageWidth / numColumns;

        // Render operation highlights (red) first so they appear behind comparison highlights
        for (const index of this.operationIndices) {
            this._renderHighlight(ctx, index, colWidth, imageHeight, OPERATION_COLOR);
        }

        // Render comparison highlights (green) on top
        for (const index of this.comparisonIndices) {
            this._renderHighlight(ctx, index, colWidth, imageHeight, COMPARISON_COLOR);
        }
    }

    /**
     * Renders a single highlight rectangle at the specified column position.
     * 
     * @private
     * @param {CanvasRenderingContext2D} ctx - Canvas rendering context
     * @param {number} columnIndex - Column position to highlight
     * @param {number} colWidth - Width of each column
     * @param {number} height - Height of the highlight rectangle
     * @param {string} color - RGBA color string for the highlight
     */
    _renderHighlight(ctx, columnIndex, colWidth, height, color) {
        ctx.fillStyle = color;
        ctx.fillRect(
            columnIndex * colWidth,  // x position
            0,                        // y position (top of canvas)
            colWidth,                 // width
            height                    // height (full column height)
        );
    }
}

