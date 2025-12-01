/**
 * Particle System Module
 * Manages particle lifecycle, physics simulation, and rendering
 */

/**
 * Particle class representing a single particle in the vector field
 */
class Particle {
    /**
     * Create a particle
     * @param {number} x - X coordinate in math space
     * @param {number} y - Y coordinate in math space
     * @param {number} maxLifetime - Maximum lifetime in seconds
     */
    constructor(x, y, maxLifetime) {
        this.position = { x, y };
        this.trail = [];
        this.age = 0;
        this.maxLifetime = maxLifetime;
    }
    
    /**
     * Check if the particle is still alive
     * @returns {boolean} True if particle is alive
     */
    isAlive() {
        return this.age < this.maxLifetime;
    }
}

/**
 * ParticleSystem class managing all particles
 */
class ParticleSystem {
    /**
     * Create a particle system
     * @param {CoordinateSystem} coordinateSystem - Coordinate system for transformations
     * @param {ExpressionEvaluator} expressionEvaluator - Expression evaluator for vector field
     */
    constructor(coordinateSystem, expressionEvaluator) {
        this.coordinateSystem = coordinateSystem;
        this.expressionEvaluator = expressionEvaluator;
        
        this.particles = [];
        this.maxTrailLength = 50;
        this.defaultLifetime = 10; // seconds
    }
    
    /**
     * Add a single particle at the specified math space coordinates
     * @param {number} mathX - X coordinate in math space
     * @param {number} mathY - Y coordinate in math space
     */
    addParticle(mathX, mathY) {
        const particle = new Particle(mathX, mathY, this.defaultLifetime);
        this.particles.push(particle);
    }
    
    /**
     * Add particles in a grid pattern across the visible area
     * @param {Object} bounds - Visible bounds {minX, maxX, minY, maxY}
     * @param {number} density - Number of particles per axis (e.g., 10 = 10x10 grid)
     */
    addParticleGrid(bounds, density) {
        const { minX, maxX, minY, maxY } = bounds;
        const stepX = (maxX - minX) / (density - 1);
        const stepY = (maxY - minY) / (density - 1);
        
        for (let i = 0; i < density; i++) {
            for (let j = 0; j < density; j++) {
                const x = minX + i * stepX;
                const y = minY + j * stepY;
                this.addParticle(x, y);
            }
        }
    }
    
    /**
     * Add particles within a circular brush area
     * @param {number} mathX - Center X coordinate in math space
     * @param {number} mathY - Center Y coordinate in math space
     * @param {number} thickness - Brush radius in math units
     * @param {number} density - Number of particles to spawn
     */
    addParticlesInBrush(mathX, mathY, thickness, density) {
        const radius = thickness / 2;
        
        for (let i = 0; i < density; i++) {
            // Generate random position within circle using polar coordinates
            const angle = Math.random() * Math.PI * 2;
            const distance = Math.sqrt(Math.random()) * radius; // sqrt for uniform distribution
            
            const x = mathX + distance * Math.cos(angle);
            const y = mathY + distance * Math.sin(angle);
            
            this.addParticle(x, y);
        }
    }
    
    /**
     * Update all particles based on the vector field
     * @param {number} deltaTime - Time step in seconds
     * @param {boolean} isPaused - Whether simulation is paused
     */
    update(deltaTime, isPaused) {
        if (isPaused) {
            return;
        }
        
        // Update each particle
        for (let i = this.particles.length - 1; i >= 0; i--) {
            const particle = this.particles[i];
            
            // Update age
            particle.age += deltaTime;
            
            // Remove dead particles
            if (!particle.isAlive()) {
                this.particles.splice(i, 1);
                continue;
            }
            
            // Evaluate vector field at particle position
            const velocity = this.expressionEvaluator.evaluate(
                particle.position.x,
                particle.position.y
            );
            
            // Skip update if velocity is invalid (NaN/Infinity)
            if (!velocity) {
                continue;
            }
            
            // Add current position to trail before moving
            particle.trail.push({ x: particle.position.x, y: particle.position.y });
            
            // Limit trail length to prevent memory issues
            if (particle.trail.length > this.maxTrailLength) {
                particle.trail.shift();
            }
            
            // Euler integration: position += velocity * deltaTime
            particle.position.x += velocity.x * deltaTime;
            particle.position.y += velocity.y * deltaTime;
        }
    }
    
    /**
     * Render all particles and their trails
     * @param {CanvasRenderingContext2D} ctx - Canvas rendering context
     */
    render(ctx) {
        // Get visible bounds for culling off-screen particles
        const bounds = this.coordinateSystem.getVisibleBounds();
        const margin = 1; // Small margin to avoid edge artifacts
        
        // Filter visible particles
        const visibleParticles = this.particles.filter(particle => {
            return particle.position.x >= bounds.minX - margin &&
                   particle.position.x <= bounds.maxX + margin &&
                   particle.position.y >= bounds.minY - margin &&
                   particle.position.y <= bounds.maxY + margin;
        });
        
        // Skip rendering if no visible particles
        if (visibleParticles.length === 0) {
            return;
        }
        
        // Batch trail rendering
        ctx.lineWidth = 2.5;
        ctx.lineCap = 'round';
        ctx.lineJoin = 'round';
        
        for (const particle of visibleParticles) {
            // Draw trail with decreasing opacity
            if (particle.trail.length > 1) {
                ctx.beginPath();
                
                // Convert first trail position to pixel space
                const firstPixel = this.coordinateSystem.mathToPixel(
                    particle.trail[0].x,
                    particle.trail[0].y
                );
                ctx.moveTo(firstPixel.x, firstPixel.y);
                
                // Draw trail segments with decreasing opacity
                for (let i = 1; i < particle.trail.length; i++) {
                    const pixel = this.coordinateSystem.mathToPixel(
                        particle.trail[i].x,
                        particle.trail[i].y
                    );
                    
                    // Calculate opacity based on position in trail (older = more transparent)
                    const opacity = (i / particle.trail.length) * 0.6;
                    ctx.strokeStyle = `rgba(121, 192, 255, ${opacity})`;
                    
                    ctx.lineTo(pixel.x, pixel.y);
                    ctx.stroke();
                    ctx.beginPath();
                    ctx.moveTo(pixel.x, pixel.y);
                }
            }
        }
        
        // Batch particle rendering - draw all glows first, then all cores
        // This reduces context state changes
        
        // Draw outer glows
        ctx.fillStyle = 'rgba(121, 192, 255, 0.3)';
        for (const particle of visibleParticles) {
            const pixel = this.coordinateSystem.mathToPixel(
                particle.position.x,
                particle.position.y
            );
            ctx.beginPath();
            ctx.arc(pixel.x, pixel.y, 6, 0, Math.PI * 2);
            ctx.fill();
        }
        
        // Draw inner particles
        ctx.fillStyle = 'rgba(121, 192, 255, 0.95)';
        for (const particle of visibleParticles) {
            const pixel = this.coordinateSystem.mathToPixel(
                particle.position.x,
                particle.position.y
            );
            ctx.beginPath();
            ctx.arc(pixel.x, pixel.y, 3.5, 0, Math.PI * 2);
            ctx.fill();
        }
    }
    
    /**
     * Remove all particles from the system
     */
    clear() {
        this.particles = [];
    }
}

export { Particle, ParticleSystem };
