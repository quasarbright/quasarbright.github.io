/**
 * Player Controller with ODM Gear Mechanics
 * Handles player movement, grappling hooks, swinging physics, and gas propulsion.
 */

class Player {
    constructor(camera, city) {
        this.camera = camera;
        this.city = city;
        
        // Position and velocity
        this.position = new THREE.Vector3(0, 50, 0);
        this.velocity = new THREE.Vector3(0, 0, 0);
        
        // Camera rotation
        this.yaw = 0;
        this.pitch = 0;
        
        // ODM Gear state
        this.leftHook = null;
        this.rightHook = null;
        this.gas = 100;
        this.maxGas = 100;
        
        // Physics constants
        this.gravity = -20;
        this.airControl = 0.5;
        this.boostPower = 40;
        this.gasConsumptionBoost = 15; // per second
        this.gasConsumptionGrapple = 5; // per second
        this.maxSpeed = 100;
        this.reelSpeed = 2;
        
        // Input state
        this.keys = {};
        this.mouseButtons = { left: false, right: false };
        
        this.setupControls();
    }

    /**
     * Sets up keyboard and mouse controls
     */
    setupControls() {
        document.addEventListener('keydown', (e) => {
            this.keys[e.code] = true;
            
            if (e.code === 'KeyR') {
                this.detachHooks();
            }
        });
        
        document.addEventListener('keyup', (e) => {
            this.keys[e.code] = false;
        });
        
        document.addEventListener('mousemove', (e) => {
            if (document.pointerLockElement) {
                this.yaw -= e.movementX * 0.002;
                this.pitch -= e.movementY * 0.002;
                this.pitch = Math.max(-Math.PI / 2, Math.min(Math.PI / 2, this.pitch));
            }
        });
        
        document.addEventListener('mousedown', (e) => {
            if (e.button === 0) { // Left click
                this.fireHook('left');
            } else if (e.button === 2) { // Right click
                this.fireHook('right');
            }
        });
        
        document.addEventListener('mouseup', (e) => {
            if (e.button === 0) {
                this.mouseButtons.left = false;
            } else if (e.button === 2) {
                this.mouseButtons.right = false;
            }
        });
        
        // Prevent context menu
        document.addEventListener('contextmenu', (e) => e.preventDefault());
    }

    /**
     * Fires a grappling hook
     */
    fireHook(side) {
        if (this.gas <= 0) return;
        
        // Find attachment point
        const direction = this.getViewDirection();
        const targetPos = this.position.clone().add(direction.multiplyScalar(300));
        
        const nearest = this.city.findNearestSurface(targetPos, 300);
        
        if (nearest) {
            const hook = {
                position: nearest.point.clone(),
                restLength: this.position.distanceTo(nearest.point),
                maxLength: this.position.distanceTo(nearest.point)
            };
            
            if (side === 'left') {
                this.leftHook = hook;
            } else {
                this.rightHook = hook;
            }
            
            // Small pull towards hook
            const toHook = hook.position.clone().sub(this.position).normalize();
            this.velocity.add(toHook.multiplyScalar(5));
        }
    }

    /**
     * Detaches all hooks
     */
    detachHooks() {
        this.leftHook = null;
        this.rightHook = null;
    }

    /**
     * Gets the camera's view direction
     */
    getViewDirection() {
        const direction = new THREE.Vector3(0, 0, -1);
        direction.applyEuler(new THREE.Euler(this.pitch, this.yaw, 0, 'YXZ'));
        return direction;
    }

    /**
     * Updates player state each frame
     */
    update(deltaTime) {
        this.handleInput(deltaTime);
        this.applyPhysics(deltaTime);
        this.updateGas(deltaTime);
        this.updateCamera();
        this.checkGroundCollision();
    }

    /**
     * Processes player input
     */
    handleInput(deltaTime) {
        const isGrappled = this.leftHook || this.rightHook;
        
        // Air control (WASD)
        const forward = this.getViewDirection();
        forward.y = 0;
        forward.normalize();
        
        const right = new THREE.Vector3();
        right.crossVectors(forward, new THREE.Vector3(0, 1, 0)).normalize();
        
        const moveForce = new THREE.Vector3();
        
        if (this.keys['KeyW']) moveForce.add(forward);
        if (this.keys['KeyS']) moveForce.sub(forward);
        if (this.keys['KeyD']) moveForce.add(right);
        if (this.keys['KeyA']) moveForce.sub(right);
        
        if (moveForce.length() > 0) {
            moveForce.normalize().multiplyScalar(this.airControl);
            this.velocity.add(moveForce);
        }
        
        // Boost (Space) - only when grappled and has gas
        if (this.keys['Space'] && isGrappled && this.gas > 0) {
            const boostDir = this.getViewDirection();
            this.velocity.add(boostDir.multiplyScalar(this.boostPower * deltaTime));
            this.gas -= this.gasConsumptionBoost * deltaTime;
        }
        
        // Reel in (Shift)
        if (this.keys['ShiftLeft'] || this.keys['ShiftRight']) {
            if (this.leftHook) {
                this.leftHook.maxLength = Math.max(5, this.leftHook.maxLength - this.reelSpeed);
            }
            if (this.rightHook) {
                this.rightHook.maxLength = Math.max(5, this.rightHook.maxLength - this.reelSpeed);
            }
        } else {
            // Restore max length when not reeling
            if (this.leftHook) {
                this.leftHook.maxLength = Math.min(
                    this.leftHook.restLength,
                    this.leftHook.maxLength + this.reelSpeed * 0.5
                );
            }
            if (this.rightHook) {
                this.rightHook.maxLength = Math.min(
                    this.rightHook.restLength,
                    this.rightHook.maxLength + this.reelSpeed * 0.5
                );
            }
        }
    }

    /**
     * Applies physics including gravity and grappling hook forces
     */
    applyPhysics(deltaTime) {
        // Apply gravity
        this.velocity.y += this.gravity * deltaTime;
        
        // Apply grappling hook physics
        if (this.leftHook) {
            this.applyHookPhysics(this.leftHook, deltaTime);
        }
        if (this.rightHook) {
            this.applyHookPhysics(this.rightHook, deltaTime);
        }
        
        // Apply drag
        const drag = 0.98;
        this.velocity.multiplyScalar(drag);
        
        // Limit max speed
        const speed = this.velocity.length();
        if (speed > this.maxSpeed) {
            this.velocity.multiplyScalar(this.maxSpeed / speed);
        }
        
        // Update position
        const movement = this.velocity.clone().multiplyScalar(deltaTime);
        this.position.add(movement);
    }

    /**
     * Applies spring-like physics for a grappling hook
     */
    applyHookPhysics(hook, deltaTime) {
        const toHook = hook.position.clone().sub(this.position);
        const distance = toHook.length();
        
        // Only apply force if exceeding max length
        if (distance > hook.maxLength) {
            const direction = toHook.normalize();
            const springForce = (distance - hook.maxLength) * 10; // Spring constant
            
            this.velocity.add(direction.multiplyScalar(springForce * deltaTime));
            
            // Damping perpendicular to rope
            const ropeDir = direction;
            const velocityAlongRope = this.velocity.clone().projectOnVector(ropeDir);
            const velocityPerpendicular = this.velocity.clone().sub(velocityAlongRope);
            
            this.velocity.sub(velocityPerpendicular.multiplyScalar(0.05));
        }
    }

    /**
     * Updates gas level
     */
    updateGas(deltaTime) {
        // Consume gas while grappled
        if (this.leftHook || this.rightHook) {
            this.gas -= this.gasConsumptionGrapple * deltaTime;
        }
        
        // Regenerate gas slowly when not using ODM gear
        if (!this.leftHook && !this.rightHook && !this.keys['Space']) {
            this.gas += 10 * deltaTime;
        }
        
        this.gas = Math.max(0, Math.min(this.maxGas, this.gas));
        
        // Auto-detach if out of gas
        if (this.gas <= 0) {
            this.detachHooks();
        }
    }

    /**
     * Updates camera position and rotation
     */
    updateCamera() {
        this.camera.position.copy(this.position);
        this.camera.rotation.order = 'YXZ';
        this.camera.rotation.y = this.yaw;
        this.camera.rotation.x = this.pitch;
    }

    /**
     * Handles collision with ground
     */
    checkGroundCollision() {
        if (this.position.y < 2) {
            this.position.y = 2;
            this.velocity.y = Math.max(0, this.velocity.y);
            
            // Apply ground friction
            this.velocity.x *= 0.9;
            this.velocity.z *= 0.9;
        }
    }

    /**
     * Gets current speed in km/h for display
     */
    getSpeed() {
        return Math.round(this.velocity.length() * 3.6); // Convert to km/h
    }

    /**
     * Gets gas percentage for display
     */
    getGasPercent() {
        return Math.round((this.gas / this.maxGas) * 100);
    }
}

