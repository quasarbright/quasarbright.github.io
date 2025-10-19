/**
 * Main Game Module
 * Initializes the game, handles the render loop, and manages visual effects.
 */

class Game {
    constructor() {
        this.canvas = document.getElementById('gameCanvas');
        this.scene = null;
        this.camera = null;
        this.renderer = null;
        this.city = null;
        this.player = null;
        this.clock = new THREE.Clock();
        
        // Visual effects
        this.grapplingLines = { left: null, right: null };
        this.trailParticles = [];
        
        // UI elements
        this.gasBar = document.getElementById('gas-bar');
        this.gasText = document.getElementById('gas-text');
        this.speedText = document.getElementById('speed-text');
        this.instructions = document.getElementById('instructions');
        
        this.isStarted = false;
        
        this.init();
    }

    /**
     * Initializes the game scene, camera, and objects
     */
    init() {
        // Setup Three.js
        this.setupScene();
        this.setupLighting();
        
        // Create city
        this.city = new City(this.scene);
        this.city.generate();
        
        // Create player
        this.player = new Player(this.camera, this.city);
        
        // Setup grappling line visuals
        this.setupGrapplingLines();
        
        // Start render loop
        this.animate();
        
        // Wait for user to click to start
        document.addEventListener('click', () => this.start(), { once: true });
    }

    /**
     * Sets up the Three.js scene, camera, and renderer
     */
    setupScene() {
        this.scene = new THREE.Scene();
        this.scene.background = new THREE.Color(0x87CEEB); // Sky blue
        this.scene.fog = new THREE.Fog(0x87CEEB, 100, 1000);
        
        // Camera
        this.camera = new THREE.PerspectiveCamera(
            75,
            window.innerWidth / window.innerHeight,
            0.1,
            2000
        );
        this.camera.position.set(0, 50, 0);
        
        // Renderer
        this.renderer = new THREE.WebGLRenderer({ 
            canvas: this.canvas,
            antialias: true 
        });
        this.renderer.setSize(window.innerWidth, window.innerHeight);
        this.renderer.shadowMap.enabled = true;
        this.renderer.shadowMap.type = THREE.PCFSoftShadowMap;
        
        // Handle window resize
        window.addEventListener('resize', () => {
            this.camera.aspect = window.innerWidth / window.innerHeight;
            this.camera.updateProjectionMatrix();
            this.renderer.setSize(window.innerWidth, window.innerHeight);
        });
    }

    /**
     * Sets up scene lighting
     */
    setupLighting() {
        // Ambient light
        const ambientLight = new THREE.AmbientLight(0xffffff, 0.6);
        this.scene.add(ambientLight);
        
        // Directional light (sun)
        const sunLight = new THREE.DirectionalLight(0xffffff, 0.8);
        sunLight.position.set(100, 200, 100);
        sunLight.castShadow = true;
        sunLight.shadow.camera.left = -200;
        sunLight.shadow.camera.right = 200;
        sunLight.shadow.camera.top = 200;
        sunLight.shadow.camera.bottom = -200;
        sunLight.shadow.camera.far = 500;
        sunLight.shadow.mapSize.width = 2048;
        sunLight.shadow.mapSize.height = 2048;
        this.scene.add(sunLight);
        
        // Hemisphere light for better color gradation
        const hemiLight = new THREE.HemisphereLight(0x87CEEB, 0x545454, 0.4);
        this.scene.add(hemiLight);
    }

    /**
     * Sets up visual representations for grappling lines
     */
    setupGrapplingLines() {
        const lineMaterial = new THREE.LineBasicMaterial({ 
            color: 0x00ff00,
            linewidth: 2,
            transparent: true,
            opacity: 0.8
        });
        
        const leftGeometry = new THREE.BufferGeometry();
        this.grapplingLines.left = new THREE.Line(leftGeometry, lineMaterial);
        this.scene.add(this.grapplingLines.left);
        
        const rightGeometry = new THREE.BufferGeometry();
        this.grapplingLines.right = new THREE.Line(rightGeometry, lineMaterial);
        this.scene.add(this.grapplingLines.right);
    }

    /**
     * Starts the game (called after first click)
     */
    start() {
        if (this.isStarted) return;
        
        this.canvas.requestPointerLock();
        this.instructions.classList.add('hidden');
        this.isStarted = true;
    }

    /**
     * Main animation loop
     */
    animate() {
        requestAnimationFrame(() => this.animate());
        
        if (!this.isStarted) {
            this.renderer.render(this.scene, this.camera);
            return;
        }
        
        const deltaTime = Math.min(this.clock.getDelta(), 0.1); // Cap delta time
        
        this.player.update(deltaTime);
        this.updateVisuals();
        this.updateUI();
        
        this.renderer.render(this.scene, this.camera);
    }

    /**
     * Updates visual effects (grappling lines, particles, etc.)
     */
    updateVisuals() {
        // Update left grappling line
        if (this.player.leftHook) {
            this.updateGrapplingLine(
                this.grapplingLines.left,
                this.player.position,
                this.player.leftHook.position
            );
            this.grapplingLines.left.visible = true;
        } else {
            this.grapplingLines.left.visible = false;
        }
        
        // Update right grappling line
        if (this.player.rightHook) {
            this.updateGrapplingLine(
                this.grapplingLines.right,
                this.player.position,
                this.player.rightHook.position
            );
            this.grapplingLines.right.visible = true;
        } else {
            this.grapplingLines.right.visible = false;
        }
        
        // Add speed trail when moving fast
        if (this.player.velocity.length() > 30) {
            this.addTrailParticle();
        }
        
        this.updateTrailParticles();
    }

    /**
     * Updates a grappling line between two points
     */
    updateGrapplingLine(line, start, end) {
        const points = [];
        const segments = 20;
        
        // Create a curved line (simulating rope sag)
        for (let i = 0; i <= segments; i++) {
            const t = i / segments;
            const point = new THREE.Vector3(
                start.x + (end.x - start.x) * t,
                start.y + (end.y - start.y) * t - Math.sin(t * Math.PI) * 5, // Sag
                start.z + (end.z - start.z) * t
            );
            points.push(point);
        }
        
        line.geometry.setFromPoints(points);
    }

    /**
     * Adds a trail particle for visual effect when moving fast
     */
    addTrailParticle() {
        // Limit number of particles
        if (this.trailParticles.length > 50) return;
        
        const geometry = new THREE.SphereGeometry(0.5, 4, 4);
        const material = new THREE.MeshBasicMaterial({ 
            color: 0x00ff00,
            transparent: true,
            opacity: 0.6
        });
        const particle = new THREE.Mesh(geometry, material);
        particle.position.copy(this.player.position);
        particle.userData.life = 1.0;
        
        this.scene.add(particle);
        this.trailParticles.push(particle);
    }

    /**
     * Updates and removes old trail particles
     */
    updateTrailParticles() {
        for (let i = this.trailParticles.length - 1; i >= 0; i--) {
            const particle = this.trailParticles[i];
            particle.userData.life -= 0.02;
            particle.material.opacity = particle.userData.life * 0.6;
            
            if (particle.userData.life <= 0) {
                this.scene.remove(particle);
                particle.geometry.dispose();
                particle.material.dispose();
                this.trailParticles.splice(i, 1);
            }
        }
    }

    /**
     * Updates UI elements (gas bar, speed, etc.)
     */
    updateUI() {
        // Update gas bar
        const gasPercent = this.player.getGasPercent();
        this.gasBar.style.width = gasPercent + '%';
        this.gasText.textContent = gasPercent + '%';
        
        // Change color based on gas level
        this.gasBar.classList.remove('low', 'empty');
        if (gasPercent < 20) {
            this.gasBar.classList.add('empty');
        } else if (gasPercent < 40) {
            this.gasBar.classList.add('low');
        }
        
        // Update speed
        const speed = this.player.getSpeed();
        this.speedText.textContent = speed + ' km/h';
        
        // Color speed based on value
        if (speed > 80) {
            this.speedText.style.color = '#ff5722';
        } else if (speed > 50) {
            this.speedText.style.color = '#ff9800';
        } else {
            this.speedText.style.color = '#4CAF50';
        }
    }
}

// Start the game when page loads
window.addEventListener('DOMContentLoaded', () => {
    new Game();
});

