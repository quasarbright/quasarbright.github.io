/**
 * VectorRenderer Tests
 * Tests for vector field rendering functionality
 */

import { describe, it, expect } from 'vitest';
import * as fc from 'fast-check';
import { VectorRenderer } from './VectorRenderer.js';
import { CoordinateSystem } from './CoordinateSystem.js';
import { ExpressionEvaluator } from './ExpressionEvaluator.js';

describe('VectorRenderer', () => {
    // Helper to create a mock canvas context that tracks draw calls
    function createMockContext() {
        const calls = {
            arrows: []
        };
        
        const ctx = {
            strokeStyle: '',
            fillStyle: '',
            lineWidth: 0,
            lineCap: '',
            beginPath: () => {},
            moveTo: () => {},
            lineTo: () => {},
            closePath: () => {},
            stroke: () => {},
            fill: () => {}
        };
        
        return { ctx, calls };
    }
    
    // Helper to capture arrow draw calls
    function captureArrows(renderer, ctx) {
        const arrows = [];
        const originalDrawArrow = renderer.drawArrow.bind(renderer);
        renderer.drawArrow = (ctx, x, y, dx, dy, thickness) => {
            arrows.push({ x, y, dx, dy, thickness });
            originalDrawArrow(ctx, x, y, dx, dy, thickness);
        };
        return { arrows, restore: () => { renderer.drawArrow = originalDrawArrow; } };
    }
    
    // Feature: vector-field-visualizer, Property 5: Arrows exist at all grid intersections
    // **Validates: Requirements 2.1**
    it('should render arrows at all valid grid intersections', () => {
        fc.assert(
            fc.property(
                fc.integer({ min: 400, max: 1200 }), // canvas width
                fc.integer({ min: 400, max: 1200 }), // canvas height
                fc.double({ min: 10, max: 200 }),    // zoom level
                fc.double({ min: -500, max: 500 }),  // offsetX
                fc.double({ min: -500, max: 500 }),  // offsetY
                (width, height, zoom, offsetX, offsetY) => {
                    // Create coordinate system with random state
                    const coordSys = new CoordinateSystem(width, height);
                    coordSys.zoomLevel = zoom;
                    coordSys.offsetX = offsetX;
                    coordSys.offsetY = offsetY;
                    
                    // Use a simple valid vector field
                    const evaluator = new ExpressionEvaluator('-y', 'x');
                    
                    // Create renderer
                    const renderer = new VectorRenderer(coordSys, evaluator);
                    
                    // Calculate expected number of grid intersections
                    const spacing = coordSys.getGridSpacing();
                    const bounds = coordSys.getVisibleBounds();
                    
                    const minX = Math.floor(bounds.minX / spacing) * spacing;
                    const maxX = Math.ceil(bounds.maxX / spacing) * spacing;
                    const minY = Math.floor(bounds.minY / spacing) * spacing;
                    const maxY = Math.ceil(bounds.maxY / spacing) * spacing;
                    
                    let expectedIntersections = 0;
                    for (let y = minY; y <= maxY; y += spacing) {
                        for (let x = minX; x <= maxX; x += spacing) {
                            const vector = evaluator.evaluate(x, y);
                            if (vector !== null) {
                                expectedIntersections++;
                            }
                        }
                    }
                    
                    // Count actual arrows drawn by intercepting drawArrow calls
                    let arrowCount = 0;
                    const originalDrawArrow = renderer.drawArrow.bind(renderer);
                    renderer.drawArrow = () => {
                        arrowCount++;
                    };
                    
                    const { ctx } = createMockContext();
                    renderer.render(ctx);
                    
                    // Restore original method
                    renderer.drawArrow = originalDrawArrow;
                    
                    // The number of arrows should equal the number of valid intersections
                    return arrowCount === expectedIntersections;
                }
            ),
            { numRuns: 100 }
        );
    });
    
    // Feature: vector-field-visualizer, Property 6: Arrow length constraint
    // **Validates: Requirements 2.2**
    it('should ensure no arrow exceeds grid spacing in length', () => {
        fc.assert(
            fc.property(
                fc.integer({ min: 400, max: 1200 }), // canvas width
                fc.integer({ min: 400, max: 1200 }), // canvas height
                fc.double({ min: 10, max: 200 }),    // zoom level
                fc.double({ min: -500, max: 500 }),  // offsetX
                fc.double({ min: -500, max: 500 }),  // offsetY
                fc.constantFrom(
                    ['-y', 'x'],           // rotation field
                    ['x', 'y'],            // source field
                    ['sin(x)', 'cos(y)'],  // wave field
                    ['x*x', 'y*y']         // quadratic field
                ),
                (width, height, zoom, offsetX, offsetY, [xExpr, yExpr]) => {
                    // Create coordinate system with random state
                    const coordSys = new CoordinateSystem(width, height);
                    coordSys.zoomLevel = zoom;
                    coordSys.offsetX = offsetX;
                    coordSys.offsetY = offsetY;
                    
                    // Use the selected vector field
                    const evaluator = new ExpressionEvaluator(xExpr, yExpr);
                    
                    // Create renderer
                    const renderer = new VectorRenderer(coordSys, evaluator);
                    
                    // Get grid spacing in pixels
                    const spacing = coordSys.getGridSpacing();
                    const gridSpacingPixels = spacing * coordSys.zoomLevel;
                    
                    // Capture arrow draw calls
                    const { arrows, restore } = captureArrows(renderer, null);
                    
                    const { ctx } = createMockContext();
                    renderer.render(ctx);
                    
                    restore();
                    
                    // Check that all arrows have length <= grid spacing (with small tolerance for floating point)
                    const tolerance = 0.01;
                    return arrows.every(arrow => {
                        const length = Math.sqrt(arrow.dx * arrow.dx + arrow.dy * arrow.dy);
                        return length <= gridSpacingPixels + tolerance;
                    });
                }
            ),
            { numRuns: 100 }
        );
    });
    
    // Feature: vector-field-visualizer, Property 7: Arrow thickness reflects relative magnitude
    // **Validates: Requirements 2.3**
    it('should scale arrow thickness proportionally to vector magnitude', () => {
        fc.assert(
            fc.property(
                fc.integer({ min: 400, max: 1200 }), // canvas width
                fc.integer({ min: 400, max: 1200 }), // canvas height
                fc.double({ min: 10, max: 200 }),    // zoom level
                fc.double({ min: -500, max: 500 }),  // offsetX
                fc.double({ min: -500, max: 500 }),  // offsetY
                (width, height, zoom, offsetX, offsetY) => {
                    // Create coordinate system with random state
                    const coordSys = new CoordinateSystem(width, height);
                    coordSys.zoomLevel = zoom;
                    coordSys.offsetX = offsetX;
                    coordSys.offsetY = offsetY;
                    
                    // Use a field with varying magnitudes: vx = x, vy = y
                    // This creates vectors with different magnitudes at different positions
                    const evaluator = new ExpressionEvaluator('x', 'y');
                    
                    // Create renderer
                    const renderer = new VectorRenderer(coordSys, evaluator);
                    
                    // Capture arrow draw calls
                    const { arrows, restore } = captureArrows(renderer, null);
                    
                    const { ctx } = createMockContext();
                    renderer.render(ctx);
                    
                    restore();
                    
                    // If we have fewer than 2 arrows, we can't test relative thickness
                    if (arrows.length < 2) {
                        return true;
                    }
                    
                    // Calculate magnitudes for each arrow
                    const arrowData = arrows.map(arrow => {
                        // The arrow dx, dy are already scaled, so we need to get the original vector
                        // We can approximate by looking at the length ratio
                        const length = Math.sqrt(arrow.dx * arrow.dx + arrow.dy * arrow.dy);
                        return {
                            thickness: arrow.thickness,
                            length: length
                        };
                    });
                    
                    // Find arrows with max and min lengths
                    const maxLength = Math.max(...arrowData.map(a => a.length));
                    const minLength = Math.min(...arrowData.map(a => a.length));
                    
                    // If all arrows have the same length, they should have the same thickness
                    if (Math.abs(maxLength - minLength) < 0.01) {
                        const thicknesses = arrowData.map(a => a.thickness);
                        const maxThickness = Math.max(...thicknesses);
                        const minThickness = Math.min(...thicknesses);
                        return Math.abs(maxThickness - minThickness) < 0.01;
                    }
                    
                    // Otherwise, arrows with larger lengths should have larger thickness
                    const maxLengthArrow = arrowData.find(a => Math.abs(a.length - maxLength) < 0.01);
                    const minLengthArrow = arrowData.find(a => Math.abs(a.length - minLength) < 0.01);
                    
                    return maxLengthArrow.thickness >= minLengthArrow.thickness;
                }
            ),
            { numRuns: 100 }
        );
    });
});
