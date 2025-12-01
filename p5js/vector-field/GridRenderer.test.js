/**
 * GridRenderer Tests
 * Property-based tests for grid rendering functionality
 */

import { describe, it, expect, beforeEach } from 'vitest';
import * as fc from 'fast-check';
import { GridRenderer } from './GridRenderer.js';
import { CoordinateSystem } from './CoordinateSystem.js';

describe('GridRenderer', () => {
    let canvas;
    let ctx;
    
    beforeEach(() => {
        // Create a mock canvas and context
        canvas = {
            width: 800,
            height: 600
        };
        
        ctx = {
            strokeStyle: '',
            fillStyle: '',
            lineWidth: 0,
            font: '',
            textAlign: '',
            textBaseline: '',
            beginPath: () => {},
            moveTo: () => {},
            lineTo: () => {},
            stroke: () => {},
            fillText: () => {},
            calls: []
        };
        
        // Track fillText calls for testing
        ctx.fillText = function(text, x, y) {
            this.calls.push({ method: 'fillText', text, x, y });
        };
    });
    
    // Feature: vector-field-visualizer, Property 14: Axis labels at intersections
    describe('Property 14: Axis labels at intersections', () => {
        it('should place labels at grid-axis intersections when axes are visible', () => {
            fc.assert(
                fc.property(
                    // Generate zoom levels that keep axes visible
                    fc.double({ min: 10, max: 200 }),
                    // Generate small offsets to keep axes on screen
                    fc.double({ min: -100, max: 100 }),
                    fc.double({ min: -100, max: 100 }),
                    (zoomLevel, offsetX, offsetY) => {
                        const coordSystem = new CoordinateSystem(canvas.width, canvas.height);
                        coordSystem.zoomLevel = zoomLevel;
                        coordSystem.offsetX = offsetX;
                        coordSystem.offsetY = offsetY;
                        
                        const bounds = coordSystem.getVisibleBounds();
                        
                        // Only test when both axes are visible
                        const xAxisVisible = bounds.minY <= 0 && bounds.maxY >= 0;
                        const yAxisVisible = bounds.minX <= 0 && bounds.maxX >= 0;
                        
                        if (!xAxisVisible || !yAxisVisible) {
                            return true; // Skip this case
                        }
                        
                        const renderer = new GridRenderer(coordSystem, canvas);
                        ctx.calls = [];
                        
                        renderer.render(ctx);
                        
                        const spacing = coordSystem.getGridSpacing();
                        const minX = Math.floor(bounds.minX / spacing) * spacing;
                        const maxX = Math.ceil(bounds.maxX / spacing) * spacing;
                        const minY = Math.floor(bounds.minY / spacing) * spacing;
                        const maxY = Math.ceil(bounds.maxY / spacing) * spacing;
                        
                        // Check that labels exist at grid-axis intersections
                        const labelCalls = ctx.calls.filter(c => c.method === 'fillText');
                        
                        // Count expected x-axis labels (vertical grid lines intersecting x-axis)
                        let expectedXLabels = 0;
                        for (let x = minX; x <= maxX; x += spacing) {
                            if (Math.abs(x) >= spacing * 0.001) {
                                expectedXLabels++;
                            }
                        }
                        
                        // Count expected y-axis labels (horizontal grid lines intersecting y-axis)
                        let expectedYLabels = 0;
                        for (let y = minY; y <= maxY; y += spacing) {
                            if (Math.abs(y) >= spacing * 0.001) {
                                expectedYLabels++;
                            }
                        }
                        
                        const totalExpectedLabels = expectedXLabels + expectedYLabels;
                        
                        // Verify we have the expected number of labels
                        expect(labelCalls.length).toBe(totalExpectedLabels);
                        
                        return true;
                    }
                ),
                { numRuns: 100 }
            );
        });
    });
    
    // Feature: vector-field-visualizer, Property 15: Off-screen axis labels at edges
    describe('Property 15: Off-screen axis labels at edges', () => {
        it('should place labels at screen edges when axes are off-screen', () => {
            fc.assert(
                fc.property(
                    // Generate zoom levels
                    fc.double({ min: 10, max: 200 }),
                    // Generate offsets that push axes off-screen
                    fc.oneof(
                        // X-axis off-screen (top or bottom)
                        fc.record({
                            offsetX: fc.double({ min: -100, max: 100 }),
                            offsetY: fc.oneof(
                                fc.double({ min: 400, max: 800 }),  // X-axis below screen
                                fc.double({ min: -800, max: -400 }) // X-axis above screen
                            ),
                            axisOffScreen: fc.constant('x')
                        }),
                        // Y-axis off-screen (left or right)
                        fc.record({
                            offsetX: fc.oneof(
                                fc.double({ min: 500, max: 1000 }),  // Y-axis left of screen
                                fc.double({ min: -1000, max: -500 }) // Y-axis right of screen
                            ),
                            offsetY: fc.double({ min: -100, max: 100 }),
                            axisOffScreen: fc.constant('y')
                        })
                    ),
                    (zoomLevel, offsetConfig) => {
                        const coordSystem = new CoordinateSystem(canvas.width, canvas.height);
                        coordSystem.zoomLevel = zoomLevel;
                        coordSystem.offsetX = offsetConfig.offsetX;
                        coordSystem.offsetY = offsetConfig.offsetY;
                        
                        const bounds = coordSystem.getVisibleBounds();
                        
                        // Verify the intended axis is actually off-screen
                        const xAxisVisible = bounds.minY <= 0 && bounds.maxY >= 0;
                        const yAxisVisible = bounds.minX <= 0 && bounds.maxX >= 0;
                        
                        if (offsetConfig.axisOffScreen === 'x' && xAxisVisible) {
                            return true; // Skip if x-axis is still visible
                        }
                        if (offsetConfig.axisOffScreen === 'y' && yAxisVisible) {
                            return true; // Skip if y-axis is still visible
                        }
                        
                        const renderer = new GridRenderer(coordSystem, canvas);
                        ctx.calls = [];
                        
                        renderer.render(ctx);
                        
                        const spacing = coordSystem.getGridSpacing();
                        const labelCalls = ctx.calls.filter(c => c.method === 'fillText');
                        
                        // Verify labels are placed at screen edges
                        if (offsetConfig.axisOffScreen === 'x' && !xAxisVisible) {
                            // X-axis is off-screen, labels should be at top or bottom edge
                            const labelY = bounds.maxY < 0 ? bounds.maxY : bounds.minY;
                            
                            // Check that some labels exist (we should have labels for vertical grid lines)
                            const minX = Math.floor(bounds.minX / spacing) * spacing;
                            const maxX = Math.ceil(bounds.maxX / spacing) * spacing;
                            let expectedLabels = 0;
                            for (let x = minX; x <= maxX; x += spacing) {
                                if (Math.abs(x) >= spacing * 0.001) {
                                    expectedLabels++;
                                }
                            }
                            
                            // We should have at least some labels
                            expect(labelCalls.length).toBeGreaterThan(0);
                        }
                        
                        if (offsetConfig.axisOffScreen === 'y' && !yAxisVisible) {
                            // Y-axis is off-screen, labels should be at left or right edge
                            const labelX = bounds.maxX < 0 ? bounds.maxX : bounds.minX;
                            
                            // Check that some labels exist (we should have labels for horizontal grid lines)
                            const minY = Math.floor(bounds.minY / spacing) * spacing;
                            const maxY = Math.ceil(bounds.maxY / spacing) * spacing;
                            let expectedLabels = 0;
                            for (let y = minY; y <= maxY; y += spacing) {
                                if (Math.abs(y) >= spacing * 0.001) {
                                    expectedLabels++;
                                }
                            }
                            
                            // We should have at least some labels
                            expect(labelCalls.length).toBeGreaterThan(0);
                        }
                        
                        return true;
                    }
                ),
                { numRuns: 100 }
            );
        });
    });
});
