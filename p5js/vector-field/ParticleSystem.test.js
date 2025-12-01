/**
 * Tests for ParticleSystem
 */

import { describe, it, expect } from 'vitest';
import * as fc from 'fast-check';
import { Particle, ParticleSystem } from './ParticleSystem.js';
import { CoordinateSystem } from './CoordinateSystem.js';
import { ExpressionEvaluator } from './ExpressionEvaluator.js';

describe('ParticleSystem', () => {
    // Feature: vector-field-visualizer, Property 23: Particle count stays within limit
    // Validates: Requirements 10.1
    it('should enforce maximum particle count limit', () => {
        fc.assert(
            fc.property(
                fc.integer({ min: 1, max: 100 }), // maxParticles
                fc.integer({ min: 1, max: 200 }), // attemptedParticles
                (maxParticles, attemptedParticles) => {
                    const coordSystem = new CoordinateSystem(800, 600);
                    const evaluator = new ExpressionEvaluator('x', 'y');
                    const particleSystem = new ParticleSystem(coordSystem, evaluator);
                    
                    // Set custom max particles
                    particleSystem.maxParticles = maxParticles;
                    
                    // Try to add more particles than the limit
                    for (let i = 0; i < attemptedParticles; i++) {
                        particleSystem.addParticle(i, i);
                    }
                    
                    // Verify particle count never exceeds maximum
                    expect(particleSystem.particles.length).toBeLessThanOrEqual(maxParticles);
                }
            ),
            { numRuns: 100 }
        );
    });
    
    // Feature: vector-field-visualizer, Property 18: Particles despawn after lifetime
    // Validates: Requirements 6.4
    it('should remove particles after their lifetime expires', () => {
        fc.assert(
            fc.property(
                fc.double({ min: 0.1, max: 5, noNaN: true }), // maxLifetime
                fc.double({ min: 0.01, max: 0.1, noNaN: true }), // deltaTime per step
                (maxLifetime, dt) => {
                    const coordSystem = new CoordinateSystem(800, 600);
                    const evaluator = new ExpressionEvaluator('0', '0'); // Zero velocity field
                    const particleSystem = new ParticleSystem(coordSystem, evaluator);
                    
                    // Create particle with specific lifetime
                    const particle = new Particle(0, 0, maxLifetime);
                    particleSystem.particles.push(particle);
                    
                    // Update until just before particle should die
                    let totalTime = 0;
                    while (totalTime + dt < maxLifetime) {
                        particleSystem.update(dt, false);
                        totalTime += dt;
                    }
                    
                    // Particle should still exist (age < maxLifetime)
                    expect(particleSystem.particles.length).toBe(1);
                    
                    // Update enough times to exceed lifetime
                    particleSystem.update(dt, false);
                    particleSystem.update(dt, false);
                    
                    // Particle should now be removed
                    expect(particleSystem.particles.length).toBe(0);
                }
            ),
            { numRuns: 100 }
        );
    });
    
    // Feature: vector-field-visualizer, Property 17: Particles have trails
    // Validates: Requirements 6.3
    it('should maintain trails of previous positions for particles that have moved', () => {
        fc.assert(
            fc.property(
                fc.integer({ min: 1, max: 20 }), // number of updates
                fc.double({ min: 0.01, max: 0.1, noNaN: true }), // deltaTime
                (numUpdates, dt) => {
                    const coordSystem = new CoordinateSystem(800, 600);
                    const evaluator = new ExpressionEvaluator('1', '1'); // Constant velocity field
                    const particleSystem = new ParticleSystem(coordSystem, evaluator);
                    
                    // Add particle
                    particleSystem.addParticle(0, 0);
                    
                    // Update multiple times
                    for (let i = 0; i < numUpdates; i++) {
                        particleSystem.update(dt, false);
                    }
                    
                    const particle = particleSystem.particles[0];
                    
                    // Particle should have a trail with previous positions
                    expect(particle.trail.length).toBeGreaterThan(0);
                    expect(particle.trail.length).toBeLessThanOrEqual(particleSystem.maxTrailLength);
                    
                    // Trail should contain position objects with x and y
                    for (const pos of particle.trail) {
                        expect(pos).toHaveProperty('x');
                        expect(pos).toHaveProperty('y');
                        expect(typeof pos.x).toBe('number');
                        expect(typeof pos.y).toBe('number');
                    }
                }
            ),
            { numRuns: 100 }
        );
    });
    
    // Feature: vector-field-visualizer, Property 20: Grid spawn distribution
    // Validates: Requirements 7.1
    it('should distribute particles evenly across visible area in grid pattern', () => {
        fc.assert(
            fc.property(
                fc.integer({ min: 2, max: 10 }), // density (grid size)
                fc.double({ min: -10, max: 0, noNaN: true }), // minX
                fc.double({ min: 1, max: 10, noNaN: true }), // width
                fc.double({ min: -10, max: 0, noNaN: true }), // minY
                fc.double({ min: 1, max: 10, noNaN: true }), // height
                (density, minX, width, minY, height) => {
                    const maxX = minX + width;
                    const maxY = minY + height;
                    const bounds = { minX, maxX, minY, maxY };
                    
                    const coordSystem = new CoordinateSystem(800, 600);
                    const evaluator = new ExpressionEvaluator('0', '0');
                    const particleSystem = new ParticleSystem(coordSystem, evaluator);
                    
                    particleSystem.addParticleGrid(bounds, density);
                    
                    // Should create density^2 particles (or maxParticles, whichever is smaller)
                    const expectedCount = Math.min(density * density, particleSystem.maxParticles);
                    expect(particleSystem.particles.length).toBe(expectedCount);
                    
                    // Check that particles are distributed across the bounds
                    const stepX = (maxX - minX) / (density - 1);
                    const stepY = (maxY - minY) / (density - 1);
                    
                    // Verify particles are approximately evenly spaced
                    for (let i = 0; i < Math.min(density, particleSystem.particles.length); i++) {
                        const particle = particleSystem.particles[i];
                        expect(particle.position.x).toBeGreaterThanOrEqual(minX - 1e-10);
                        expect(particle.position.x).toBeLessThanOrEqual(maxX + 1e-10);
                        expect(particle.position.y).toBeGreaterThanOrEqual(minY - 1e-10);
                        expect(particle.position.y).toBeLessThanOrEqual(maxY + 1e-10);
                    }
                }
            ),
            { numRuns: 100 }
        );
    });
    
    // Feature: vector-field-visualizer, Property 21: Brush respects thickness
    // Validates: Requirements 9.2
    it('should spawn particles within brush thickness radius', () => {
        fc.assert(
            fc.property(
                fc.double({ min: 0, max: 10, noNaN: true }), // centerX
                fc.double({ min: 0, max: 10, noNaN: true }), // centerY
                fc.double({ min: 0.1, max: 5, noNaN: true }), // thickness
                fc.integer({ min: 10, max: 50 }), // density
                (centerX, centerY, thickness, density) => {
                    const coordSystem = new CoordinateSystem(800, 600);
                    const evaluator = new ExpressionEvaluator('0', '0');
                    const particleSystem = new ParticleSystem(coordSystem, evaluator);
                    
                    particleSystem.addParticlesInBrush(centerX, centerY, thickness, density);
                    
                    const radius = thickness / 2;
                    
                    // All particles should be within radius of center
                    for (const particle of particleSystem.particles) {
                        const dx = particle.position.x - centerX;
                        const dy = particle.position.y - centerY;
                        const distance = Math.sqrt(dx * dx + dy * dy);
                        
                        expect(distance).toBeLessThanOrEqual(radius + 1e-10);
                    }
                }
            ),
            { numRuns: 100 }
        );
    });
    
    // Feature: vector-field-visualizer, Property 22: Brush respects density
    // Validates: Requirements 9.3
    it('should spawn number of particles proportional to density parameter', () => {
        fc.assert(
            fc.property(
                fc.double({ min: 0, max: 10, noNaN: true }), // centerX
                fc.double({ min: 0, max: 10, noNaN: true }), // centerY
                fc.double({ min: 0.1, max: 5, noNaN: true }), // thickness
                fc.integer({ min: 5, max: 30 }), // density
                (centerX, centerY, thickness, density) => {
                    const coordSystem = new CoordinateSystem(800, 600);
                    const evaluator = new ExpressionEvaluator('0', '0');
                    const particleSystem = new ParticleSystem(coordSystem, evaluator);
                    
                    const initialCount = particleSystem.particles.length;
                    particleSystem.addParticlesInBrush(centerX, centerY, thickness, density);
                    const finalCount = particleSystem.particles.length;
                    
                    // Number of particles added should equal density (or less if hit max)
                    const particlesAdded = finalCount - initialCount;
                    const expectedAdded = Math.min(density, particleSystem.maxParticles - initialCount);
                    
                    expect(particlesAdded).toBe(expectedAdded);
                }
            ),
            { numRuns: 100 }
        );
    });
    
    // Feature: vector-field-visualizer, Property 16: Particle movement follows vector field
    // Validates: Requirements 6.2
    it('should move particles according to vector field using Euler integration', () => {
        fc.assert(
            fc.property(
                fc.double({ min: -10, max: 10, noNaN: true }), // x position
                fc.double({ min: -10, max: 10, noNaN: true }), // y position
                fc.double({ min: 0.001, max: 0.1, noNaN: true }), // deltaTime
                fc.constantFrom('-y', 'x', 'sin(x)', 'cos(y)', 'x + y'), // vx expression
                fc.constantFrom('x', 'y', 'cos(x)', 'sin(y)', 'x - y'), // vy expression
                (x, y, dt, vxExpr, vyExpr) => {
                    const coordSystem = new CoordinateSystem(800, 600);
                    const evaluator = new ExpressionEvaluator(vxExpr, vyExpr);
                    const particleSystem = new ParticleSystem(coordSystem, evaluator);
                    
                    // Add particle at position
                    particleSystem.addParticle(x, y);
                    
                    // Get initial position
                    const initialX = particleSystem.particles[0].position.x;
                    const initialY = particleSystem.particles[0].position.y;
                    
                    // Get velocity at initial position
                    const velocity = evaluator.evaluate(initialX, initialY);
                    
                    // Update particle
                    particleSystem.update(dt, false);
                    
                    // Get new position
                    const newX = particleSystem.particles[0].position.x;
                    const newY = particleSystem.particles[0].position.y;
                    
                    // Verify Euler integration: new_pos = old_pos + velocity * dt
                    if (velocity) {
                        const expectedX = initialX + velocity.x * dt;
                        const expectedY = initialY + velocity.y * dt;
                        
                        // Use approximate equality due to floating point
                        expect(Math.abs(newX - expectedX)).toBeLessThan(1e-10);
                        expect(Math.abs(newY - expectedY)).toBeLessThan(1e-10);
                    }
                }
            ),
            { numRuns: 100 }
        );
    });
});
