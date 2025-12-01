/**
 * Tests for ExpressionEvaluator
 */

import { describe, it, expect } from 'vitest';
import * as fc from 'fast-check';
import { ExpressionEvaluator } from './ExpressionEvaluator.js';

describe('ExpressionEvaluator', () => {
    // Unit tests for basic functionality
    describe('Basic functionality', () => {
        it('should evaluate simple expressions', () => {
            const evaluator = new ExpressionEvaluator('-y', 'x');
            expect(evaluator.isValid()).toBe(true);
            
            const result = evaluator.evaluate(3, 4);
            expect(result).toEqual({ x: -4, y: 3 });
        });
        
        it('should handle mathematical functions', () => {
            const evaluator = new ExpressionEvaluator('sin(x)', 'cos(y)');
            expect(evaluator.isValid()).toBe(true);
            
            const result = evaluator.evaluate(0, 0);
            expect(result).toEqual({ x: 0, y: 1 });
        });
    });
    
    // Feature: vector-field-visualizer, Property 1: Valid expression evaluation produces numeric results
    // Validates: Requirements 1.2
    describe('Property 1: Valid expression evaluation produces numeric results', () => {
        it('should produce finite numeric results for valid expressions at any point', () => {
            // Generator for valid simple expressions using x and y
            const validExpressionArb = fc.oneof(
                fc.constant('x'),
                fc.constant('y'),
                fc.constant('x + y'),
                fc.constant('x - y'),
                fc.constant('x * y'),
                fc.constant('x * 2'),
                fc.constant('y * 2'),
                fc.constant('-x'),
                fc.constant('-y'),
                fc.constant('sin(x)'),
                fc.constant('cos(y)'),
                fc.constant('x * x'),
                fc.constant('y * y'),
                fc.constant('sqrt(abs(x))'),
                fc.constant('exp(x / 10)'), // Scaled to avoid overflow
                fc.constant('x + y * 2'),
                fc.constant('cos(x) + sin(y)')
            );
            
            // Generator for finite coordinate values
            const finiteCoordArb = fc.double({ min: -100, max: 100, noNaN: true });
            
            fc.assert(
                fc.property(
                    validExpressionArb,
                    validExpressionArb,
                    finiteCoordArb,
                    finiteCoordArb,
                    (xExpr, yExpr, x, y) => {
                        const evaluator = new ExpressionEvaluator(xExpr, yExpr);
                        
                        // Expression should be valid
                        if (!evaluator.isValid()) {
                            return true; // Skip if expression is invalid
                        }
                        
                        const result = evaluator.evaluate(x, y);
                        
                        // If result is not null, it should contain finite numbers
                        if (result !== null) {
                            expect(typeof result.x).toBe('number');
                            expect(typeof result.y).toBe('number');
                            expect(isFinite(result.x)).toBe(true);
                            expect(isFinite(result.y)).toBe(true);
                        }
                        
                        return true;
                    }
                ),
                { numRuns: 100 }
            );
        });
    });
    
    // Feature: vector-field-visualizer, Property 2: Syntax errors are detected
    // Validates: Requirements 1.3
    describe('Property 2: Syntax errors are detected', () => {
        it('should detect and report syntax errors in expressions', () => {
            // Generator for expressions with syntax errors that math.js actually rejects
            const invalidSyntaxArb = fc.oneof(
                fc.constant('x +'),           // Incomplete expression
                fc.constant('* y'),           // Missing left operand
                fc.constant('(x + y'),        // Unmatched opening parenthesis
                fc.constant('x + y)'),        // Unmatched closing parenthesis
                fc.constant('sin('),          // Incomplete function call
                fc.constant('sqrt(x'),        // Unclosed function
                fc.constant('x + (y * )'),    // Empty expression in parentheses
                fc.constant('..x'),           // Invalid syntax
                fc.constant('x + * y'),       // Invalid operator sequence
                fc.constant(')('),            // Invalid parentheses order
                fc.constant('x +* y'),        // Invalid operator combination
                fc.constant('cos(x'),         // Unclosed function
                fc.constant('(((x)'),         // Multiple unclosed parentheses
                fc.constant('x)))'),          // Multiple unmatched closing
                fc.constant('x / '),          // Incomplete division
                fc.constant('^ y')            // Missing left operand for power
            );
            
            fc.assert(
                fc.property(
                    invalidSyntaxArb,
                    (invalidExpr) => {
                        const evaluator = new ExpressionEvaluator(invalidExpr, 'x');
                        
                        // Should be marked as invalid
                        expect(evaluator.isValid()).toBe(false);
                        
                        // Should have an error message
                        expect(evaluator.getError()).not.toBe(null);
                        expect(typeof evaluator.getError()).toBe('string');
                        expect(evaluator.getError().length).toBeGreaterThan(0);
                        
                        // evaluate should return null
                        expect(evaluator.evaluate(1, 1)).toBe(null);
                        
                        return true;
                    }
                ),
                { numRuns: 100 }
            );
        });
    });
    
    // Feature: vector-field-visualizer, Property 3: Undefined variables are detected
    // Validates: Requirements 1.4
    describe('Property 3: Undefined variables are detected', () => {
        it('should detect expressions with undefined variables (only x and y allowed)', () => {
            // Generator for expressions with undefined variables
            const undefinedVarExprArb = fc.oneof(
                fc.constant('z'),                    // Single undefined variable
                fc.constant('x + z'),                // Mix of valid and invalid
                fc.constant('a * b'),                // Multiple undefined
                fc.constant('sin(t)'),               // Undefined in function
                fc.constant('x + y + z'),            // One undefined among valid
                fc.constant('foo'),                  // Named variable
                fc.constant('x * bar'),              // Mix
                fc.constant('alpha + beta'),         // Greek letters
                fc.constant('x1'),                   // Variable with number
                fc.constant('myVar'),                // CamelCase variable
                fc.constant('x + y + w + z'),        // Multiple undefined
                fc.constant('cos(theta)'),           // Common physics variable
                fc.constant('r * cos(phi)')          // Polar coordinates
            );
            
            fc.assert(
                fc.property(
                    undefinedVarExprArb,
                    (expr) => {
                        const evaluator = new ExpressionEvaluator(expr, 'x');
                        
                        // Should be marked as invalid
                        expect(evaluator.isValid()).toBe(false);
                        
                        // Should have an error message mentioning undefined variables
                        const error = evaluator.getError();
                        expect(error).not.toBe(null);
                        expect(typeof error).toBe('string');
                        expect(error.toLowerCase()).toContain('undefined');
                        
                        // evaluate should return null
                        expect(evaluator.evaluate(1, 1)).toBe(null);
                        
                        return true;
                    }
                ),
                { numRuns: 100 }
            );
        });
    });
    
    // Feature: vector-field-visualizer, Property 4: Invalid values are handled gracefully
    // Validates: Requirements 1.5
    describe('Property 4: Invalid values are handled gracefully', () => {
        it('should return null for points that evaluate to NaN or Infinity', () => {
            // Expressions that can produce NaN or Infinity at certain points
            const problematicExpressions = [
                { xExpr: '1 / x', yExpr: 'y', testX: 0, testY: 1 },        // Division by zero -> Infinity
                { xExpr: 'sqrt(x)', yExpr: 'y', testX: -1, testY: 1 },     // Sqrt of negative -> NaN
                { xExpr: 'log(x)', yExpr: 'y', testX: 0, testY: 1 },       // Log of zero -> -Infinity
                { xExpr: 'log(x)', yExpr: 'y', testX: -1, testY: 1 },      // Log of negative -> NaN
                { xExpr: 'x', yExpr: '1 / y', testX: 1, testY: 0 },        // Division by zero in y
                { xExpr: 'x / y', yExpr: 'y', testX: 1, testY: 0 },        // Division by zero
                { xExpr: 'asin(x)', yExpr: 'y', testX: 2, testY: 1 },      // Asin out of domain -> NaN
                { xExpr: 'x', yExpr: 'sqrt(y)', testX: 1, testY: -1 }      // Sqrt of negative in y
            ];
            
            problematicExpressions.forEach(({ xExpr, yExpr, testX, testY }) => {
                const evaluator = new ExpressionEvaluator(xExpr, yExpr);
                
                // Expression should be valid (syntax is correct)
                expect(evaluator.isValid()).toBe(true);
                
                // But evaluation at problematic point should return null
                const result = evaluator.evaluate(testX, testY);
                expect(result).toBe(null);
            });
        });
        
        it('should handle NaN/Infinity gracefully across random inputs', () => {
            // Generator for expressions that might produce NaN/Infinity
            const riskyExpressionArb = fc.oneof(
                fc.constant('1 / x'),
                fc.constant('1 / y'),
                fc.constant('sqrt(x)'),
                fc.constant('sqrt(y)'),
                fc.constant('log(x)'),
                fc.constant('log(y)'),
                fc.constant('x / y'),
                fc.constant('y / x'),
                fc.constant('tan(x)'),
                fc.constant('asin(x)'),
                fc.constant('acos(y)')
            );
            
            // Generator for coordinates that might cause issues
            const riskyCoordArb = fc.oneof(
                fc.constant(0),           // Division by zero
                fc.constant(-1),          // Negative for sqrt/log
                fc.constant(Math.PI / 2), // Tan singularity
                fc.constant(2),           // Out of domain for asin/acos
                fc.constant(-2),          // Out of domain for asin/acos
                fc.double({ min: -10, max: 10, noNaN: true })
            );
            
            fc.assert(
                fc.property(
                    riskyExpressionArb,
                    riskyExpressionArb,
                    riskyCoordArb,
                    riskyCoordArb,
                    (xExpr, yExpr, x, y) => {
                        const evaluator = new ExpressionEvaluator(xExpr, yExpr);
                        
                        if (!evaluator.isValid()) {
                            return true; // Skip invalid expressions
                        }
                        
                        const result = evaluator.evaluate(x, y);
                        
                        // Result should either be null or contain finite numbers
                        if (result !== null) {
                            expect(isFinite(result.x)).toBe(true);
                            expect(isFinite(result.y)).toBe(true);
                        }
                        
                        // If result is null, that's acceptable (graceful handling)
                        return true;
                    }
                ),
                { numRuns: 100 }
            );
        });
    });
});
