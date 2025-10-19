/**
 * City Generation Module
 * Creates a procedural Manhattan-like city with buildings arranged in a grid pattern.
 */

class City {
    constructor(scene) {
        this.scene = scene;
        this.buildings = [];
        this.buildingMeshes = [];
    }

    /**
     * Generates a grid-based city with varied building heights
     */
    generate() {
        const citySize = 20; // 20x20 blocks
        const blockSize = 50;
        const streetWidth = 15;
        const spacing = blockSize + streetWidth;

        // Create ground
        this.createGround(citySize, spacing);

        // Generate buildings in a grid
        for (let x = -citySize / 2; x < citySize / 2; x++) {
            for (let z = -citySize / 2; z < citySize / 2; z++) {
                // Skip some buildings randomly for variety
                if (Math.random() > 0.85) continue;

                const posX = x * spacing;
                const posZ = z * spacing;
                
                // Vary building dimensions
                const width = blockSize * (0.7 + Math.random() * 0.3);
                const depth = blockSize * (0.7 + Math.random() * 0.3);
                const height = this.getBuildingHeight(x, z, citySize);

                this.createBuilding(posX, posZ, width, depth, height);
            }
        }

        // Add some landmark tall buildings
        this.createLandmarkBuildings(citySize, spacing);
    }

    /**
     * Determines building height with taller buildings near center (Manhattan-style)
     */
    getBuildingHeight(x, z, citySize) {
        const distFromCenter = Math.sqrt(x * x + z * z) / citySize;
        const centerBonus = (1 - distFromCenter) * 100;
        const baseHeight = 30 + Math.random() * 40;
        return baseHeight + centerBonus + Math.random() * 60;
    }

    /**
     * Creates a single building with windows
     */
    createBuilding(x, z, width, depth, height) {
        const geometry = new THREE.BoxGeometry(width, height, depth);
        
        // Create building material with windows
        const material = new THREE.MeshLambertMaterial({
            color: this.getBuildingColor(),
            flatShading: true
        });

        const building = new THREE.Mesh(geometry, material);
        building.position.set(x, height / 2, z);
        building.castShadow = true;
        building.receiveShadow = true;

        // Add windows
        this.addWindows(building, width, depth, height);

        this.scene.add(building);
        this.buildingMeshes.push(building);
        
        // Store building data for collision detection
        this.buildings.push({
            x: x,
            y: height / 2,
            z: z,
            width: width,
            height: height,
            depth: depth,
            mesh: building
        });
    }

    /**
     * Adds window details to buildings
     */
    addWindows(building, width, depth, height) {
        const windowMaterial = new THREE.MeshBasicMaterial({ 
            color: 0xffffaa,
            transparent: true,
            opacity: 0.6
        });

        const floors = Math.floor(height / 4);
        const windowsPerRow = Math.floor(Math.max(width, depth) / 4);

        // Add windows on each floor (front and back faces)
        for (let floor = 1; floor < floors; floor++) {
            const y = -height / 2 + floor * 4;
            
            for (let i = 0; i < windowsPerRow; i++) {
                const xOffset = -width / 2 + (i + 0.5) * (width / windowsPerRow);
                
                // Front/back windows
                if (Math.random() > 0.3) { // 70% chance of lit window
                    const windowGeom = new THREE.PlaneGeometry(1.5, 2);
                    const window1 = new THREE.Mesh(windowGeom, windowMaterial);
                    window1.position.set(xOffset, y, depth / 2 + 0.1);
                    building.add(window1);

                    const window2 = new THREE.Mesh(windowGeom, windowMaterial);
                    window2.position.set(xOffset, y, -depth / 2 - 0.1);
                    window2.rotation.y = Math.PI;
                    building.add(window2);
                }
            }
        }

        // Side windows
        const sideWindowsPerRow = Math.floor(depth / 4);
        for (let floor = 1; floor < floors; floor++) {
            const y = -height / 2 + floor * 4;
            
            for (let i = 0; i < sideWindowsPerRow; i++) {
                const zOffset = -depth / 2 + (i + 0.5) * (depth / sideWindowsPerRow);
                
                if (Math.random() > 0.3) {
                    const windowGeom = new THREE.PlaneGeometry(1.5, 2);
                    const window1 = new THREE.Mesh(windowGeom, windowMaterial);
                    window1.position.set(width / 2 + 0.1, y, zOffset);
                    window1.rotation.y = -Math.PI / 2;
                    building.add(window1);

                    const window2 = new THREE.Mesh(windowGeom, windowMaterial);
                    window2.position.set(-width / 2 - 0.1, y, zOffset);
                    window2.rotation.y = Math.PI / 2;
                    building.add(window2);
                }
            }
        }
    }

    /**
     * Returns a varied building color (grays, browns, etc.)
     */
    getBuildingColor() {
        const colors = [
            0x808080, // Gray
            0x696969, // Dim gray
            0x778899, // Light slate gray
            0x606060, // Dark gray
            0x8B7355, // Brown
            0x5F5F5F, // Darker gray
        ];
        return colors[Math.floor(Math.random() * colors.length)];
    }

    /**
     * Creates special landmark tall buildings
     */
    createLandmarkBuildings(citySize, spacing) {
        const landmarks = [
            { x: 2, z: 3, height: 200 },
            { x: -3, z: -2, height: 180 },
            { x: 5, z: -5, height: 220 },
            { x: -4, z: 4, height: 190 },
        ];

        landmarks.forEach(landmark => {
            const posX = landmark.x * spacing;
            const posZ = landmark.z * spacing;
            this.createBuilding(posX, posZ, 40, 40, landmark.height);
        });
    }

    /**
     * Creates the ground plane
     */
    createGround(citySize, spacing) {
        const groundSize = citySize * spacing + 500;
        const groundGeometry = new THREE.PlaneGeometry(groundSize, groundSize);
        const groundMaterial = new THREE.MeshLambertMaterial({ 
            color: 0x333333,
            side: THREE.DoubleSide 
        });
        
        const ground = new THREE.Mesh(groundGeometry, groundMaterial);
        ground.rotation.x = -Math.PI / 2;
        ground.position.y = 0;
        ground.receiveShadow = true;
        
        this.scene.add(ground);
    }

    /**
     * Finds the nearest building surface point to a given position
     */
    findNearestSurface(position, maxDistance = 300) {
        let nearest = null;
        let minDist = maxDistance;

        for (const building of this.buildings) {
            // Check each face of the building
            const faces = this.getBuildingFaces(building);
            
            for (const face of faces) {
                const point = this.closestPointOnFace(position, face);
                const dist = position.distanceTo(point);
                
                if (dist < minDist) {
                    minDist = dist;
                    nearest = {
                        point: point,
                        distance: dist,
                        building: building
                    };
                }
            }
        }

        return nearest;
    }

    /**
     * Gets the faces of a building for grappling hook attachment
     */
    getBuildingFaces(building) {
        const halfW = building.width / 2;
        const halfD = building.depth / 2;
        const halfH = building.height / 2;

        return [
            // Front face
            {
                normal: new THREE.Vector3(0, 0, 1),
                point: new THREE.Vector3(building.x, building.y, building.z + halfD),
                bounds: { x: [building.x - halfW, building.x + halfW], y: [0, building.height], z: building.z + halfD }
            },
            // Back face
            {
                normal: new THREE.Vector3(0, 0, -1),
                point: new THREE.Vector3(building.x, building.y, building.z - halfD),
                bounds: { x: [building.x - halfW, building.x + halfW], y: [0, building.height], z: building.z - halfD }
            },
            // Right face
            {
                normal: new THREE.Vector3(1, 0, 0),
                point: new THREE.Vector3(building.x + halfW, building.y, building.z),
                bounds: { x: building.x + halfW, y: [0, building.height], z: [building.z - halfD, building.z + halfD] }
            },
            // Left face
            {
                normal: new THREE.Vector3(-1, 0, 0),
                point: new THREE.Vector3(building.x - halfW, building.y, building.z),
                bounds: { x: building.x - halfW, y: [0, building.height], z: [building.z - halfD, building.z + halfD] }
            },
        ];
    }

    /**
     * Finds the closest point on a building face
     */
    closestPointOnFace(position, face) {
        const point = position.clone();
        
        // Project onto the face plane
        if (face.bounds.x instanceof Array) {
            point.x = Math.max(face.bounds.x[0], Math.min(face.bounds.x[1], position.x));
        } else {
            point.x = face.bounds.x;
        }
        
        if (face.bounds.y instanceof Array) {
            point.y = Math.max(face.bounds.y[0], Math.min(face.bounds.y[1], position.y));
        } else {
            point.y = face.bounds.y;
        }
        
        if (face.bounds.z instanceof Array) {
            point.z = Math.max(face.bounds.z[0], Math.min(face.bounds.z[1], position.z));
        } else {
            point.z = face.bounds.z;
        }

        return point;
    }
}

