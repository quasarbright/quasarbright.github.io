/**
 * UIController Tests
 */

import { describe, it, expect, beforeEach, vi } from 'vitest';
import { UIController } from './UIController.js';

describe('UIController', () => {
    let uiController;
    let mockParticleSystem;
    let mockExpressionEvaluator;
    let mockCoordinateSystem;
    let mockCallback;
    
    beforeEach(() => {
        // Create mock objects
        mockParticleSystem = {
            clear: vi.fn(),
            addParticleGrid: vi.fn()
        };
        
        mockExpressionEvaluator = {
            isValid: vi.fn(() => true),
            getError: vi.fn(() => null)
        };
        
        mockCoordinateSystem = {
            getVisibleBounds: vi.fn(() => ({
                minX: -10,
                maxX: 10,
                minY: -10,
                maxY: 10
            }))
        };
        
        mockCallback = vi.fn(() => mockExpressionEvaluator);
        
        uiController = new UIController(
            mockParticleSystem,
            mockExpressionEvaluator,
            mockCoordinateSystem,
            mockCallback
        );
    });
    
    describe('Constructor', () => {
        it('should initialize with correct default state', () => {
            expect(uiController.isPaused).toBe(false);
            expect(uiController.brushMode).toBe('click');
            expect(uiController.brushThickness).toBe(5.0);
            expect(uiController.brushDensity).toBe(3.0);
        });
        
        it('should store references to dependencies', () => {
            expect(uiController.particleSystem).toBe(mockParticleSystem);
            expect(uiController.expressionEvaluator).toBe(mockExpressionEvaluator);
            expect(uiController.coordinateSystem).toBe(mockCoordinateSystem);
            expect(uiController.onExpressionUpdate).toBe(mockCallback);
        });
    });
    
    describe('Pause/Resume', () => {
        it('should toggle pause state', () => {
            expect(uiController.isPaused).toBe(false);
            
            uiController.togglePause();
            expect(uiController.isPaused).toBe(true);
            
            uiController.togglePause();
            expect(uiController.isPaused).toBe(false);
        });
        
        it('should return current pause state', () => {
            expect(uiController.getPauseState()).toBe(false);
            
            uiController.togglePause();
            expect(uiController.getPauseState()).toBe(true);
        });
    });
    
    describe('Particle Grid', () => {
        it('should spawn particle grid with visible bounds', () => {
            uiController.spawnParticleGrid();
            
            expect(mockCoordinateSystem.getVisibleBounds).toHaveBeenCalled();
            expect(mockParticleSystem.addParticleGrid).toHaveBeenCalledWith(
                { minX: -10, maxX: 10, minY: -10, maxY: 10 },
                10
            );
        });
    });
    
    describe('Clear Particles', () => {
        it('should call particle system clear method', () => {
            uiController.clearParticles();
            
            expect(mockParticleSystem.clear).toHaveBeenCalled();
        });
    });
    
    describe('Brush Controls', () => {
        it('should set and get brush thickness', () => {
            uiController.setBrushThickness(10.5);
            expect(uiController.getBrushThickness()).toBe(10.5);
        });
        
        it('should set and get brush density', () => {
            uiController.setBrushDensity(7.5);
            expect(uiController.getBrushDensity()).toBe(7.5);
        });
        
        it('should set and get brush mode', () => {
            uiController.setBrushMode('draw');
            expect(uiController.getBrushMode()).toBe('draw');
            
            uiController.setBrushMode('click');
            expect(uiController.getBrushMode()).toBe('click');
        });
    });
    
    describe('LaTeX to MathJS Conversion', () => {
        it('should convert basic expressions', () => {
            expect(uiController.latexToMathJS('x')).toBe('x');
            expect(uiController.latexToMathJS('y')).toBe('y');
            expect(uiController.latexToMathJS('-y')).toBe('-y');
        });
        
        it('should convert fractions', () => {
            const result = uiController.latexToMathJS('\\frac{x}{y}');
            expect(result).toBe('((x)/(y))');
        });
        
        it('should convert square roots', () => {
            const result = uiController.latexToMathJS('\\sqrt{x}');
            expect(result).toBe('sqrt(x)');
        });
        
        it('should convert powers', () => {
            const result = uiController.latexToMathJS('x^{2}');
            expect(result).toBe('x^(2)');
        });
        
        it('should convert trigonometric functions', () => {
            expect(uiController.latexToMathJS('\\sin x')).toBe('sin x');
            expect(uiController.latexToMathJS('\\cos x')).toBe('cos x');
            expect(uiController.latexToMathJS('\\tan x')).toBe('tan x');
        });
        
        it('should convert pi', () => {
            const result = uiController.latexToMathJS('\\pi');
            expect(result).toBe('pi');
        });
        
        it('should handle implicit multiplication', () => {
            const result = uiController.latexToMathJS('2x');
            expect(result).toBe('2*x');
        });
    });
    
    describe('Expression Update', () => {
        it('should call update callback with expressions', () => {
            uiController.updateExpressions('x', 'y');
            
            expect(mockCallback).toHaveBeenCalledWith('x', 'y');
        });
        
        it('should handle valid expressions without showing error', () => {
            mockCallback.mockReturnValue({
                isValid: () => true,
                getError: () => null
            });
            
            uiController.updateExpressions('x', 'y');
            
            // Error should not be shown (we can't test DOM directly in unit tests)
            expect(mockCallback).toHaveBeenCalled();
        });
        
        it('should handle invalid expressions', () => {
            mockCallback.mockReturnValue({
                isValid: () => false,
                getError: () => 'Syntax error'
            });
            
            uiController.updateExpressions('invalid', 'expression');
            
            expect(mockCallback).toHaveBeenCalled();
        });
    });
});
