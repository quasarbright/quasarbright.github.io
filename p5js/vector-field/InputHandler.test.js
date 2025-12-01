/**
 * Tests for InputHandler module
 */

import { describe, it, test, expect, beforeEach, afterEach } from 'vitest';
import { InputHandler } from './InputHandler.js';
import { CoordinateSystem } from './CoordinateSystem.js';
import { ParticleSystem } from './ParticleSystem.js';
import { ExpressionEvaluator } from './ExpressionEvaluator.js';

describe('InputHandler', () => {
    let canvas;
    let coordinateSystem;
    let particleSystem;
    let inputHandler;
    let expressionEvaluator;
    
    beforeEach(() => {
        // Create a mock canvas
        canvas = document.createElement('canvas');
        canvas.width = 800;
        canvas.height = 600;
        document.body.appendChild(canvas);
        
        // Create coordinate system
        coordinateSystem = new CoordinateSystem(800, 600);
        
        // Create expression evaluator
        expressionEvaluator = new ExpressionEvaluator('-y', 'x');
        
        // Create particle system
        particleSystem = new ParticleSystem(coordinateSystem, expressionEvaluator);
        
        // Create input handler
        inputHandler = new InputHandler(canvas, coordinateSystem, particleSystem);
    });
    
    afterEach(() => {
        document.body.removeChild(canvas);
    });
    
    describe('Constructor', () => {
        test('should initialize with correct default state', () => {
            expect(inputHandler.canvas).toBe(canvas);
            expect(inputHandler.coordinateSystem).toBe(coordinateSystem);
            expect(inputHandler.particleSystem).toBe(particleSystem);
            expect(inputHandler.brushMode).toBe(false);
            expect(inputHandler.isDragging).toBe(false);
            expect(inputHandler.isPanning).toBe(false);
        });
    });
    
    describe('Brush settings', () => {
        test('should set brush mode', () => {
            inputHandler.setBrushMode(true);
            expect(inputHandler.brushMode).toBe(true);
            
            inputHandler.setBrushMode(false);
            expect(inputHandler.brushMode).toBe(false);
        });
        
        test('should set brush thickness', () => {
            inputHandler.setBrushThickness(10.5);
            expect(inputHandler.brushThickness).toBe(10.5);
        });
        
        test('should set brush density', () => {
            inputHandler.setBrushDensity(7.5);
            expect(inputHandler.brushDensity).toBe(7.5);
        });
    });
    
    describe('Click particle creation', () => {
        test('should create particle on click in click mode', () => {
            const initialCount = particleSystem.particles.length;
            
            // Simulate mouse down
            const mouseDownEvent = new MouseEvent('mousedown', {
                clientX: 400,
                clientY: 300,
                button: 0
            });
            canvas.dispatchEvent(mouseDownEvent);
            
            // Simulate mouse up at same position (click)
            const mouseUpEvent = new MouseEvent('mouseup', {
                clientX: 400,
                clientY: 300,
                button: 0
            });
            canvas.dispatchEvent(mouseUpEvent);
            
            expect(particleSystem.particles.length).toBe(initialCount + 1);
        });
        
        test('should not create particle on click in brush mode', () => {
            inputHandler.setBrushMode(true);
            const initialCount = particleSystem.particles.length;
            
            // Simulate click
            const mouseDownEvent = new MouseEvent('mousedown', {
                clientX: 400,
                clientY: 300,
                button: 0
            });
            canvas.dispatchEvent(mouseDownEvent);
            
            const mouseUpEvent = new MouseEvent('mouseup', {
                clientX: 400,
                clientY: 300,
                button: 0
            });
            canvas.dispatchEvent(mouseUpEvent);
            
            expect(particleSystem.particles.length).toBe(initialCount);
        });
    });
    
    describe('Zoom handling', () => {
        test('should zoom in on wheel scroll up', () => {
            const initialZoom = coordinateSystem.zoomLevel;
            
            // Simulate wheel event (scroll up = negative deltaY = zoom in)
            const wheelEvent = new WheelEvent('wheel', {
                clientX: 400,
                clientY: 300,
                deltaY: -100
            });
            canvas.dispatchEvent(wheelEvent);
            
            expect(coordinateSystem.zoomLevel).toBeGreaterThan(initialZoom);
        });
        
        test('should zoom out on wheel scroll down', () => {
            const initialZoom = coordinateSystem.zoomLevel;
            
            // Simulate wheel event (scroll down = positive deltaY = zoom out)
            const wheelEvent = new WheelEvent('wheel', {
                clientX: 400,
                clientY: 300,
                deltaY: 100
            });
            canvas.dispatchEvent(wheelEvent);
            
            expect(coordinateSystem.zoomLevel).toBeLessThan(initialZoom);
        });
    });
    
    describe('Pan handling', () => {
        test('should pan on shift+drag', () => {
            const initialOffsetX = coordinateSystem.offsetX;
            const initialOffsetY = coordinateSystem.offsetY;
            
            // Simulate shift+mouse down
            const mouseDownEvent = new MouseEvent('mousedown', {
                clientX: 400,
                clientY: 300,
                button: 0,
                shiftKey: true
            });
            canvas.dispatchEvent(mouseDownEvent);
            
            // Simulate mouse move
            const mouseMoveEvent = new MouseEvent('mousemove', {
                clientX: 450,
                clientY: 350,
                shiftKey: true
            });
            canvas.dispatchEvent(mouseMoveEvent);
            
            // Offset should have changed
            expect(coordinateSystem.offsetX).not.toBe(initialOffsetX);
            expect(coordinateSystem.offsetY).not.toBe(initialOffsetY);
        });
        
        test('should pan on middle mouse drag', () => {
            const initialOffsetX = coordinateSystem.offsetX;
            const initialOffsetY = coordinateSystem.offsetY;
            
            // Simulate middle mouse down
            const mouseDownEvent = new MouseEvent('mousedown', {
                clientX: 400,
                clientY: 300,
                button: 1
            });
            canvas.dispatchEvent(mouseDownEvent);
            
            // Simulate mouse move
            const mouseMoveEvent = new MouseEvent('mousemove', {
                clientX: 450,
                clientY: 350
            });
            canvas.dispatchEvent(mouseMoveEvent);
            
            // Offset should have changed
            expect(coordinateSystem.offsetX).not.toBe(initialOffsetX);
            expect(coordinateSystem.offsetY).not.toBe(initialOffsetY);
        });
    });
    
    describe('Mouse tracking', () => {
        test('should track mouse position', () => {
            const mouseMoveEvent = new MouseEvent('mousemove', {
                clientX: 500,
                clientY: 400
            });
            canvas.dispatchEvent(mouseMoveEvent);
            
            expect(inputHandler.mousePosition.x).toBe(500);
            expect(inputHandler.mousePosition.y).toBe(400);
        });
        
        test('should distinguish click from drag', () => {
            // Simulate mouse down
            const mouseDownEvent = new MouseEvent('mousedown', {
                clientX: 400,
                clientY: 300,
                button: 0
            });
            canvas.dispatchEvent(mouseDownEvent);
            
            // Simulate mouse up at different position (drag)
            const mouseUpEvent = new MouseEvent('mouseup', {
                clientX: 450,
                clientY: 350,
                button: 0
            });
            canvas.dispatchEvent(mouseUpEvent);
            
            // wasClick should be false (it was a drag)
            expect(inputHandler.wasClick).toBe(false);
        });
    });
});
