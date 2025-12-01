/**
 * Property-based tests for CoordinateSystem
 * Using fast-check for property-based testing
 */

import { describe, it, expect } from 'vitest';
import * as fc from 'fast-check';
import { CoordinateSystem } from './CoordinateSystem.js';

describe('CoordinateSystem', () => {
    // Feature: vector-field-visualizer, Property 19: Coordinate transformation y-axis inversion
    // Validates: Requirements 6.5
    it('should preserve coordinates through round-trip transformation with y-axis inversion', () => {
        fc.assert(
            fc.property(
                fc.integer({ min: 100, max: 2000 }), // canvasWidth
                fc.integer({ min: 100, max: 2000 }), // canvasHeight
                fc.double({ min: -1000, max: 1000, noNaN: true }), // mathX
                fc.double({ min: -1000, max: 1000, noNaN: true }), // mathY
                (canvasWidth, canvasHeight, mathX, mathY) => {
                    const coordSys = new CoordinateSystem(canvasWidth, canvasHeight);
                    
                    // Convert math to pixel and back
                    const pixel = coordSys.mathToPixel(mathX, mathY);
                    const mathBack = coordSys.pixelToMath(pixel.x, pixel.y);
                    
                    // Should get back the original point (within floating point precision)
                    expect(mathBack.x).toBeCloseTo(mathX, 10);
                    expect(mathBack.y).toBeCloseTo(mathY, 10);
                }
            ),
            { numRuns: 100 }
        );
    });
    
    // Additional test to verify y-axis inversion specifically
    it('should invert y-axis correctly (positive y in math space = negative y in pixel space)', () => {
        fc.assert(
            fc.property(
                fc.integer({ min: 100, max: 2000 }), // canvasWidth
                fc.integer({ min: 100, max: 2000 }), // canvasHeight
                fc.double({ min: 0.1, max: 1000, noNaN: true }), // positive mathY
                (canvasWidth, canvasHeight, mathY) => {
                    const coordSys = new CoordinateSystem(canvasWidth, canvasHeight);
                    
                    // For a point at (0, positive y) in math space
                    const pixel = coordSys.mathToPixel(0, mathY);
                    
                    // The pixel y should be less than center (moving up on screen)
                    const centerY = canvasHeight / 2;
                    expect(pixel.y).toBeLessThan(centerY);
                }
            ),
            { numRuns: 100 }
        );
    });
    
    // Feature: vector-field-visualizer, Property 8: Zoom preserves point under cursor
    // Validates: Requirements 3.1
    it('should keep the mathematical point under cursor at the same pixel location after zoom', () => {
        fc.assert(
            fc.property(
                fc.integer({ min: 100, max: 2000 }), // canvasWidth
                fc.integer({ min: 100, max: 2000 }), // canvasHeight
                fc.double({ min: 0.1, max: 10, noNaN: true }), // zoom factor
                fc.integer({ min: 0, max: 1999 }), // centerPixelX
                fc.integer({ min: 0, max: 1999 }), // centerPixelY
                (canvasWidth, canvasHeight, zoomFactor, centerPixelX, centerPixelY) => {
                    // Ensure center is within canvas bounds
                    const validCenterX = Math.min(centerPixelX, canvasWidth - 1);
                    const validCenterY = Math.min(centerPixelY, canvasHeight - 1);
                    
                    const coordSys = new CoordinateSystem(canvasWidth, canvasHeight);
                    
                    // Get the math point at the center before zoom
                    const mathPointBefore = coordSys.pixelToMath(validCenterX, validCenterY);
                    
                    // Perform zoom
                    coordSys.zoom(zoomFactor, validCenterX, validCenterY);
                    
                    // Get the math point at the same pixel location after zoom
                    const mathPointAfter = coordSys.pixelToMath(validCenterX, validCenterY);
                    
                    // The math point should be the same (within floating point precision)
                    expect(mathPointAfter.x).toBeCloseTo(mathPointBefore.x, 5);
                    expect(mathPointAfter.y).toBeCloseTo(mathPointBefore.y, 5);
                }
            ),
            { numRuns: 100 }
        );
    });
    
    // Feature: vector-field-visualizer, Property 13: Pan preserves zoom
    // Validates: Requirements 4.3
    it('should maintain zoom level after panning', () => {
        fc.assert(
            fc.property(
                fc.integer({ min: 100, max: 2000 }), // canvasWidth
                fc.integer({ min: 100, max: 2000 }), // canvasHeight
                fc.double({ min: -1000, max: 1000, noNaN: true }), // deltaPixelX
                fc.double({ min: -1000, max: 1000, noNaN: true }), // deltaPixelY
                (canvasWidth, canvasHeight, deltaPixelX, deltaPixelY) => {
                    const coordSys = new CoordinateSystem(canvasWidth, canvasHeight);
                    
                    // Store initial zoom level
                    const initialZoom = coordSys.zoomLevel;
                    
                    // Perform pan
                    coordSys.pan(deltaPixelX, deltaPixelY);
                    
                    // Zoom level should remain unchanged
                    expect(coordSys.zoomLevel).toBe(initialZoom);
                }
            ),
            { numRuns: 100 }
        );
    });
    
    // Feature: vector-field-visualizer, Property 12: Pan direction correctness
    // Validates: Requirements 4.1
    it('should pan in the correct direction', () => {
        fc.assert(
            fc.property(
                fc.integer({ min: 100, max: 2000 }), // canvasWidth
                fc.integer({ min: 100, max: 2000 }), // canvasHeight
                fc.double({ min: 10, max: 100, noNaN: true }), // positive deltaPixelX
                (canvasWidth, canvasHeight, deltaPixelX) => {
                    const coordSys = new CoordinateSystem(canvasWidth, canvasHeight);
                    
                    // Get a point in math space before pan
                    const pixelX = canvasWidth / 2;
                    const pixelY = canvasHeight / 2;
                    const mathBefore = coordSys.pixelToMath(pixelX, pixelY);
                    
                    // Pan to the right (positive deltaPixelX)
                    coordSys.pan(deltaPixelX, 0);
                    
                    // The same pixel location should now correspond to a different math point
                    // Panning right means the view moves right, so the math point at center should move left
                    const mathAfter = coordSys.pixelToMath(pixelX, pixelY);
                    
                    // Math X should decrease (view moved right, so center point moved left in math space)
                    expect(mathAfter.x).toBeLessThan(mathBefore.x);
                }
            ),
            { numRuns: 100 }
        );
    });
});

describe('CoordinateSystem - Grid Spacing', () => {
    // Feature: vector-field-visualizer, Property 9: Grid spacing follows allowed sequence
    // Validates: Requirements 5.1
    it('should use grid spacing from (1, 2, 5) × 10^n sequence', () => {
        fc.assert(
            fc.property(
                fc.integer({ min: 100, max: 2000 }), // canvasWidth
                fc.integer({ min: 100, max: 2000 }), // canvasHeight
                fc.double({ min: 0.01, max: 1000, noNaN: true }), // zoom level
                (canvasWidth, canvasHeight, zoomLevel) => {
                    const coordSys = new CoordinateSystem(canvasWidth, canvasHeight);
                    coordSys.zoomLevel = zoomLevel;
                    
                    const spacing = coordSys.getGridSpacing();
                    
                    // Check if spacing is of the form (1, 2, or 5) × 10^n
                    // Find the power of 10
                    const magnitude = Math.floor(Math.log10(spacing));
                    const base = Math.pow(10, magnitude);
                    const multiplier = spacing / base;
                    
                    // Multiplier should be close to 1, 2, or 5 (within floating point precision)
                    const validMultipliers = [1, 2, 5, 10]; // 10 is next in sequence
                    const isValid = validMultipliers.some(m => Math.abs(multiplier - m) < 0.01);
                    
                    expect(isValid).toBe(true);
                }
            ),
            { numRuns: 100 }
        );
    });
    
    // Feature: vector-field-visualizer, Property 10: Grid density stays bounded
    // Validates: Requirements 3.2, 5.2
    it('should maintain bounded number of grid lines', () => {
        fc.assert(
            fc.property(
                fc.integer({ min: 100, max: 2000 }), // canvasWidth
                fc.integer({ min: 100, max: 2000 }), // canvasHeight
                fc.double({ min: 0.01, max: 1000, noNaN: true }), // zoom level
                (canvasWidth, canvasHeight, zoomLevel) => {
                    const coordSys = new CoordinateSystem(canvasWidth, canvasHeight);
                    coordSys.zoomLevel = zoomLevel;
                    
                    const spacing = coordSys.getGridSpacing();
                    const bounds = coordSys.getVisibleBounds();
                    
                    // Calculate number of grid lines
                    const horizontalLines = Math.ceil((bounds.maxY - bounds.minY) / spacing);
                    const verticalLines = Math.ceil((bounds.maxX - bounds.minX) / spacing);
                    
                    // Should not exceed maxGridLines in each direction
                    expect(horizontalLines).toBeLessThanOrEqual(coordSys.maxGridLines * 2); // Allow some margin
                    expect(verticalLines).toBeLessThanOrEqual(coordSys.maxGridLines * 2);
                }
            ),
            { numRuns: 100 }
        );
    });
    
    // Feature: vector-field-visualizer, Property 11: Grid spacing progression
    // Validates: Requirements 3.2
    it('should progress through spacing sequence when zooming', () => {
        fc.assert(
            fc.property(
                fc.integer({ min: 500, max: 2000 }), // canvasWidth
                fc.integer({ min: 500, max: 2000 }), // canvasHeight
                fc.double({ min: 1.5, max: 3, noNaN: true }), // zoom factor
                (canvasWidth, canvasHeight, zoomFactor) => {
                    const coordSys = new CoordinateSystem(canvasWidth, canvasHeight);
                    
                    const spacingBefore = coordSys.getGridSpacing();
                    
                    // Zoom in
                    coordSys.zoom(zoomFactor, canvasWidth / 2, canvasHeight / 2);
                    
                    const spacingAfter = coordSys.getGridSpacing();
                    
                    // When zooming in, spacing should decrease or stay the same
                    expect(spacingAfter).toBeLessThanOrEqual(spacingBefore);
                }
            ),
            { numRuns: 100 }
        );
    });
});
