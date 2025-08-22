precision mediump float;

uniform vec2 u_resolution;
uniform vec2 u_points[50]; // Support up to 50 points
uniform vec2 u_velocities[50]; // Velocity data for weighted calculations
uniform int u_pointCount;
uniform int u_distanceMetric;
uniform bool u_showColors;
uniform bool u_showGradient;
uniform bool u_showEdges;
uniform bool u_showDots;

// Calculate distance based on selected metric
float calculateDistance(vec2 a, vec2 b, int metric) {
    vec2 diff = a - b;
    
    if (metric == 0) {
        // Euclidean distance (squared to avoid sqrt)
        return dot(diff, diff);
    } else if (metric == 1) {
        // Manhattan distance (L1 norm)
        return abs(diff.x) + abs(diff.y);
    } else if (metric == 2) {
        // Chebyshev distance (L∞ norm)
        return max(abs(diff.x), abs(diff.y));
    } else if (metric == 3) {
        // Minkowski distance with p=3
        vec2 absDiff = abs(diff);
        return pow(pow(absDiff.x, 3.0) + pow(absDiff.y, 3.0), 1.0/3.0);
    } else if (metric == 4) {
        // Circular distance (interesting warped effect)
        float angle = atan(diff.y, diff.x);
        float radius = length(diff);
        return radius * (1.0 + 0.3 * sin(6.0 * angle));
    }
    
    // Default to Euclidean
    return dot(diff, diff);
}

// Calculate weighted distance for a specific point
float calculateWeightedDistance(vec2 a, vec2 b, vec2 velocity, vec2 position) {
    vec2 diff = a - b;
    
    // Calculate weight based on velocity magnitude and position
    float velocityMag = length(velocity);
    float positionWeight = 0.5 + 0.5 * sin(position.x * 10.0) * cos(position.y * 10.0);
    
    // Combine velocity and position effects
    float weight = 0.3 + velocityMag * 200.0 + positionWeight * 0.7;
    
    // Apply directional bias based on velocity direction
    vec2 normalizedVel = normalize(velocity + vec2(0.001)); // Avoid division by zero
    float directionalBias = 1.0 + 0.4 * dot(normalize(diff), normalizedVel);
    
    return dot(diff, diff) * weight * directionalBias;
}

// Complex number operations for Newton's method
vec2 complexMul(vec2 a, vec2 b) {
    return vec2(a.x * b.x - a.y * b.y, a.x * b.y + a.y * b.x);
}

vec2 complexDiv(vec2 a, vec2 b) {
    float denom = dot(b, b);
    if (denom < 1e-10) return vec2(0.0); // Avoid division by zero
    return vec2(a.x * b.x + a.y * b.y, a.y * b.x - a.x * b.y) / denom;
}

// Evaluate polynomial: product of (z - root_i) for all roots
vec2 evaluatePolynomial(vec2 z) {
    vec2 result = vec2(1.0, 0.0);
    
    for (int i = 0; i < 50; i++) {
        if (i >= u_pointCount) break;
        
        vec2 root = u_points[i];
        vec2 factor = z - root;
        result = complexMul(result, factor);
    }
    
    return result;
}

// Evaluate polynomial derivative: sum of products excluding one factor
vec2 evaluateDerivative(vec2 z) {
    vec2 result = vec2(0.0, 0.0);
    
    for (int i = 0; i < 50; i++) {
        if (i >= u_pointCount) break;
        
        vec2 term = vec2(1.0, 0.0);
        
        // Product of all factors except the i-th one
        for (int j = 0; j < 50; j++) {
            if (j >= u_pointCount || j == i) continue;
            
            vec2 root = u_points[j];
            vec2 factor = z - root;
            term = complexMul(term, factor);
        }
        
        result = result + term;
    }
    
    return result;
}

// Newton's method to find closest root
vec2 newtonMethod(vec2 startPoint) {
    vec2 z = startPoint;
    
    // Perform Newton iterations
    for (int iter = 0; iter < 8; iter++) {
        vec2 f = evaluatePolynomial(z);
        vec2 df = evaluateDerivative(z);
        
        // Newton step: z = z - f(z)/f'(z)
        vec2 delta = complexDiv(f, df);
        z = z - delta;
        
        // Early termination if converged
        if (length(delta) < 1e-6) break;
    }
    
    return z;
}

// Improved color blending function
vec3 betterMix(vec3 a, vec3 b, float t) {
    // Use overlay blending to preserve saturation
    // This avoids both muddy browns and washed out whites
    vec3 mixed = mix(a, b, t);
    
    // Enhance saturation to compensate for mixing
    vec3 gray = vec3(dot(mixed, vec3(0.299, 0.587, 0.114)));
    return mix(gray, mixed, 1.4);
}

// Generate a color based on point index
vec3 getColor(int index) {
    float hue = float(index) * 0.618033988749; // Golden ratio for nice color distribution
    hue = fract(hue);
    
    // Convert HSV to RGB
    vec3 c = vec3(hue, 0.8, 0.9);
    vec4 K = vec4(1.0, 2.0 / 3.0, 1.0 / 3.0, 3.0);
    vec3 p = abs(fract(c.xxx + K.xyz) * 6.0 - K.www);
    return c.z * mix(K.xxx, clamp(p - K.xxx, 0.0, 1.0), c.y);
}

void main() {
    vec2 pos = gl_FragCoord.xy;
    
    // Store distances to all points for weighted blending
    float distances[50];
    float minDist = 1e6;
    int closestPoint = 0;
    
    if (u_distanceMetric == 6) {
        // Newton's method fractal
        vec2 normalizedPos = pos / u_resolution;
        vec2 convergedPoint = newtonMethod(normalizedPos);
        
        // Calculate distances to all roots
        for (int i = 0; i < 50; i++) {
            if (i >= u_pointCount) {
                distances[i] = 1e6;
                continue;
            }
            
            vec2 rootPos = u_points[i];
            float distance = length(convergedPoint - rootPos);
            distances[i] = distance;
            
            if (distance < minDist) {
                minDist = distance;
                closestPoint = i;
            }
        }
    } else {
        // Standard distance metrics
        for (int i = 0; i < 50; i++) {
            if (i >= u_pointCount) {
                distances[i] = 1e6;
                continue;
            }
            
            vec2 pointPos = u_points[i] * u_resolution;
            float distance;
            
            if (u_distanceMetric == 5) {
                // Weighted Euclidean - need to access arrays with constant indices
                vec2 velocity = vec2(0.0);
                vec2 position = vec2(0.0);
                
                // Unroll loop for array access with constant indices
                if (i == 0) { velocity = u_velocities[0]; position = u_points[0]; }
                else if (i == 1) { velocity = u_velocities[1]; position = u_points[1]; }
                else if (i == 2) { velocity = u_velocities[2]; position = u_points[2]; }
                else if (i == 3) { velocity = u_velocities[3]; position = u_points[3]; }
                else if (i == 4) { velocity = u_velocities[4]; position = u_points[4]; }
                else if (i == 5) { velocity = u_velocities[5]; position = u_points[5]; }
                else if (i == 6) { velocity = u_velocities[6]; position = u_points[6]; }
                else if (i == 7) { velocity = u_velocities[7]; position = u_points[7]; }
                else if (i == 8) { velocity = u_velocities[8]; position = u_points[8]; }
                else if (i == 9) { velocity = u_velocities[9]; position = u_points[9]; }
                else if (i == 10) { velocity = u_velocities[10]; position = u_points[10]; }
                else if (i == 11) { velocity = u_velocities[11]; position = u_points[11]; }
                else if (i == 12) { velocity = u_velocities[12]; position = u_points[12]; }
                else if (i == 13) { velocity = u_velocities[13]; position = u_points[13]; }
                else if (i == 14) { velocity = u_velocities[14]; position = u_points[14]; }
                else if (i == 15) { velocity = u_velocities[15]; position = u_points[15]; }
                else if (i == 16) { velocity = u_velocities[16]; position = u_points[16]; }
                else if (i == 17) { velocity = u_velocities[17]; position = u_points[17]; }
                else if (i == 18) { velocity = u_velocities[18]; position = u_points[18]; }
                else if (i == 19) { velocity = u_velocities[19]; position = u_points[19]; }
                else if (i == 20) { velocity = u_velocities[20]; position = u_points[20]; }
                else if (i == 21) { velocity = u_velocities[21]; position = u_points[21]; }
                else if (i == 22) { velocity = u_velocities[22]; position = u_points[22]; }
                else if (i == 23) { velocity = u_velocities[23]; position = u_points[23]; }
                else if (i == 24) { velocity = u_velocities[24]; position = u_points[24]; }
                else if (i == 25) { velocity = u_velocities[25]; position = u_points[25]; }
                else if (i == 26) { velocity = u_velocities[26]; position = u_points[26]; }
                else if (i == 27) { velocity = u_velocities[27]; position = u_points[27]; }
                else if (i == 28) { velocity = u_velocities[28]; position = u_points[28]; }
                else if (i == 29) { velocity = u_velocities[29]; position = u_points[29]; }
                else if (i == 30) { velocity = u_velocities[30]; position = u_points[30]; }
                else if (i == 31) { velocity = u_velocities[31]; position = u_points[31]; }
                else if (i == 32) { velocity = u_velocities[32]; position = u_points[32]; }
                else if (i == 33) { velocity = u_velocities[33]; position = u_points[33]; }
                else if (i == 34) { velocity = u_velocities[34]; position = u_points[34]; }
                else if (i == 35) { velocity = u_velocities[35]; position = u_points[35]; }
                else if (i == 36) { velocity = u_velocities[36]; position = u_points[36]; }
                else if (i == 37) { velocity = u_velocities[37]; position = u_points[37]; }
                else if (i == 38) { velocity = u_velocities[38]; position = u_points[38]; }
                else if (i == 39) { velocity = u_velocities[39]; position = u_points[39]; }
                else if (i == 40) { velocity = u_velocities[40]; position = u_points[40]; }
                else if (i == 41) { velocity = u_velocities[41]; position = u_points[41]; }
                else if (i == 42) { velocity = u_velocities[42]; position = u_points[42]; }
                else if (i == 43) { velocity = u_velocities[43]; position = u_points[43]; }
                else if (i == 44) { velocity = u_velocities[44]; position = u_points[44]; }
                else if (i == 45) { velocity = u_velocities[45]; position = u_points[45]; }
                else if (i == 46) { velocity = u_velocities[46]; position = u_points[46]; }
                else if (i == 47) { velocity = u_velocities[47]; position = u_points[47]; }
                else if (i == 48) { velocity = u_velocities[48]; position = u_points[48]; }
                else if (i == 49) { velocity = u_velocities[49]; position = u_points[49]; }
                
                distance = calculateWeightedDistance(pos, pointPos, velocity, position);
            } else {
                distance = calculateDistance(pos, pointPos, u_distanceMetric);
            }
            
            distances[i] = distance;
            
            if (distance < minDist) {
                minDist = distance;
                closestPoint = i;
            }
        }
    }
    
    // Start with black background
    vec3 color = vec3(0.0, 0.0, 0.0);
    
    // Add colors if enabled
    if (u_showColors) {
        if (u_showGradient) {
            // Calculate weights for all points based on inverse distance
            vec3 weightedColor = vec3(0.0);
            float totalWeight = 0.0;
            
            // Use inverse distance weighting with falloff
            for (int i = 0; i < 50; i++) {
                if (i >= u_pointCount) break;
                
                float distance = distances[i];
                // Add small epsilon to avoid division by zero
                float weight = 1.0 / (distance + 0.001);
                // Apply exponential falloff for smoother gradients
                weight = pow(weight, 2.0);
                
                weightedColor += getColor(i) * weight;
                totalWeight += weight;
            }
            
            // Normalize the weighted color
            if (totalWeight > 0.0) {
                color = weightedColor / totalWeight;
                // Apply saturation enhancement
                vec3 gray = vec3(dot(color, vec3(0.299, 0.587, 0.114)));
                color = mix(gray, color, 1.4);
            } else {
                color = getColor(closestPoint);
            }
        } else {
            color = getColor(closestPoint);
        }
    }
    
    // Add edge highlighting if enabled - using Voronoi cell ID discontinuity
    if (u_showEdges) {
        bool isEdge = false;
        
        // Sample neighboring pixels to detect cell boundaries
        vec2 offsets[8];
        offsets[0] = vec2(-1.0, -1.0);
        offsets[1] = vec2(0.0, -1.0);
        offsets[2] = vec2(1.0, -1.0);
        offsets[3] = vec2(-1.0, 0.0);
        offsets[4] = vec2(1.0, 0.0);
        offsets[5] = vec2(-1.0, 1.0);
        offsets[6] = vec2(0.0, 1.0);
        offsets[7] = vec2(1.0, 1.0);
        
        // Check if any neighboring pixels belong to different Voronoi cells
        for (int i = 0; i < 8; i++) {
            vec2 neighborPos = pos + offsets[i];
            
            // Find closest point for this neighbor position
            int neighborClosest = 0;
            float neighborMinDist = 1e6;
            
            if (u_distanceMetric == 6) {
                // Newton's method
                vec2 normalizedNeighbor = neighborPos / u_resolution;
                vec2 convergedNeighbor = newtonMethod(normalizedNeighbor);
                
                for (int j = 0; j < 50; j++) {
                    if (j >= u_pointCount) break;
                    
                    vec2 rootPos = u_points[j];
                    float distance = length(convergedNeighbor - rootPos);
                    
                    if (distance < neighborMinDist) {
                        neighborMinDist = distance;
                        neighborClosest = j;
                    }
                }
            } else {
                // Standard distance metrics
                for (int j = 0; j < 50; j++) {
                    if (j >= u_pointCount) break;
                    
                    vec2 pointPos = u_points[j] * u_resolution;
                    float distance;
                    
                                         if (u_distanceMetric == 5) {
                         // For weighted Euclidean, use proper weighted calculation
                         vec2 velocity = vec2(0.0);
                         vec2 position = vec2(0.0);
                         
                         // Get velocity and position for this point
                         if (j == 0) { velocity = u_velocities[0]; position = u_points[0]; }
                         else if (j == 1) { velocity = u_velocities[1]; position = u_points[1]; }
                         else if (j == 2) { velocity = u_velocities[2]; position = u_points[2]; }
                         else if (j == 3) { velocity = u_velocities[3]; position = u_points[3]; }
                         else if (j == 4) { velocity = u_velocities[4]; position = u_points[4]; }
                         else if (j == 5) { velocity = u_velocities[5]; position = u_points[5]; }
                         else if (j == 6) { velocity = u_velocities[6]; position = u_points[6]; }
                         else if (j == 7) { velocity = u_velocities[7]; position = u_points[7]; }
                         else if (j == 8) { velocity = u_velocities[8]; position = u_points[8]; }
                         else if (j == 9) { velocity = u_velocities[9]; position = u_points[9]; }
                         else if (j == 10) { velocity = u_velocities[10]; position = u_points[10]; }
                         else if (j == 11) { velocity = u_velocities[11]; position = u_points[11]; }
                         else if (j == 12) { velocity = u_velocities[12]; position = u_points[12]; }
                         else if (j == 13) { velocity = u_velocities[13]; position = u_points[13]; }
                         else if (j == 14) { velocity = u_velocities[14]; position = u_points[14]; }
                         else if (j == 15) { velocity = u_velocities[15]; position = u_points[15]; }
                         else if (j == 16) { velocity = u_velocities[16]; position = u_points[16]; }
                         else if (j == 17) { velocity = u_velocities[17]; position = u_points[17]; }
                         else if (j == 18) { velocity = u_velocities[18]; position = u_points[18]; }
                         else if (j == 19) { velocity = u_velocities[19]; position = u_points[19]; }
                         else if (j == 20) { velocity = u_velocities[20]; position = u_points[20]; }
                         else if (j == 21) { velocity = u_velocities[21]; position = u_points[21]; }
                         else if (j == 22) { velocity = u_velocities[22]; position = u_points[22]; }
                         else if (j == 23) { velocity = u_velocities[23]; position = u_points[23]; }
                         else if (j == 24) { velocity = u_velocities[24]; position = u_points[24]; }
                         else if (j == 25) { velocity = u_velocities[25]; position = u_points[25]; }
                         else if (j == 26) { velocity = u_velocities[26]; position = u_points[26]; }
                         else if (j == 27) { velocity = u_velocities[27]; position = u_points[27]; }
                         else if (j == 28) { velocity = u_velocities[28]; position = u_points[28]; }
                         else if (j == 29) { velocity = u_velocities[29]; position = u_points[29]; }
                         else if (j == 30) { velocity = u_velocities[30]; position = u_points[30]; }
                         else if (j == 31) { velocity = u_velocities[31]; position = u_points[31]; }
                         else if (j == 32) { velocity = u_velocities[32]; position = u_points[32]; }
                         else if (j == 33) { velocity = u_velocities[33]; position = u_points[33]; }
                         else if (j == 34) { velocity = u_velocities[34]; position = u_points[34]; }
                         else if (j == 35) { velocity = u_velocities[35]; position = u_points[35]; }
                         else if (j == 36) { velocity = u_velocities[36]; position = u_points[36]; }
                         else if (j == 37) { velocity = u_velocities[37]; position = u_points[37]; }
                         else if (j == 38) { velocity = u_velocities[38]; position = u_points[38]; }
                         else if (j == 39) { velocity = u_velocities[39]; position = u_points[39]; }
                         else if (j == 40) { velocity = u_velocities[40]; position = u_points[40]; }
                         else if (j == 41) { velocity = u_velocities[41]; position = u_points[41]; }
                         else if (j == 42) { velocity = u_velocities[42]; position = u_points[42]; }
                         else if (j == 43) { velocity = u_velocities[43]; position = u_points[43]; }
                         else if (j == 44) { velocity = u_velocities[44]; position = u_points[44]; }
                         else if (j == 45) { velocity = u_velocities[45]; position = u_points[45]; }
                         else if (j == 46) { velocity = u_velocities[46]; position = u_points[46]; }
                         else if (j == 47) { velocity = u_velocities[47]; position = u_points[47]; }
                         else if (j == 48) { velocity = u_velocities[48]; position = u_points[48]; }
                         else if (j == 49) { velocity = u_velocities[49]; position = u_points[49]; }
                         
                         distance = calculateWeightedDistance(neighborPos, pointPos, velocity, position);
                     } else {
                         distance = calculateDistance(neighborPos, pointPos, u_distanceMetric);
                     }
                    
                    if (distance < neighborMinDist) {
                        neighborMinDist = distance;
                        neighborClosest = j;
                    }
                }
            }
            
            // If neighbor belongs to different cell, we're on an edge
            if (neighborClosest != closestPoint) {
                isEdge = true;
                break;
            }
        }
        
        if (isEdge) {
            color = vec3(1.0, 1.0, 1.0); // White edges
        }
    }
    
    // Draw white circles at point locations if enabled
    if (u_showDots) {
        float radius = 6.0; // Circle radius in pixels
        float radiusSquared = radius * radius;
        
        // Check each point with constant indexing
        vec2 diff;
        if (u_pointCount > 0) {
            diff = pos - u_points[0] * u_resolution;
            if (dot(diff, diff) < radiusSquared) color = vec3(1.0);
        }
        if (u_pointCount > 1) {
            diff = pos - u_points[1] * u_resolution;
            if (dot(diff, diff) < radiusSquared) color = vec3(1.0);
        }
        if (u_pointCount > 2) {
            diff = pos - u_points[2] * u_resolution;
            if (dot(diff, diff) < radiusSquared) color = vec3(1.0);
        }
        if (u_pointCount > 3) {
            diff = pos - u_points[3] * u_resolution;
            if (dot(diff, diff) < radiusSquared) color = vec3(1.0);
        }
        if (u_pointCount > 4) {
            diff = pos - u_points[4] * u_resolution;
            if (dot(diff, diff) < radiusSquared) color = vec3(1.0);
        }
        if (u_pointCount > 5) {
            diff = pos - u_points[5] * u_resolution;
            if (dot(diff, diff) < radiusSquared) color = vec3(1.0);
        }
        if (u_pointCount > 6) {
            diff = pos - u_points[6] * u_resolution;
            if (dot(diff, diff) < radiusSquared) color = vec3(1.0);
        }
        if (u_pointCount > 7) {
            diff = pos - u_points[7] * u_resolution;
            if (dot(diff, diff) < radiusSquared) color = vec3(1.0);
        }
        if (u_pointCount > 8) {
            diff = pos - u_points[8] * u_resolution;
            if (dot(diff, diff) < radiusSquared) color = vec3(1.0);
        }
        if (u_pointCount > 9) {
            diff = pos - u_points[9] * u_resolution;
            if (dot(diff, diff) < radiusSquared) color = vec3(1.0);
        }
        if (u_pointCount > 10) {
            diff = pos - u_points[10] * u_resolution;
            if (dot(diff, diff) < radiusSquared) color = vec3(1.0);
        }
        if (u_pointCount > 11) {
            diff = pos - u_points[11] * u_resolution;
            if (dot(diff, diff) < radiusSquared) color = vec3(1.0);
        }
        if (u_pointCount > 12) {
            diff = pos - u_points[12] * u_resolution;
            if (dot(diff, diff) < radiusSquared) color = vec3(1.0);
        }
        if (u_pointCount > 13) {
            diff = pos - u_points[13] * u_resolution;
            if (dot(diff, diff) < radiusSquared) color = vec3(1.0);
        }
        if (u_pointCount > 14) {
            diff = pos - u_points[14] * u_resolution;
            if (dot(diff, diff) < radiusSquared) color = vec3(1.0);
        }
        if (u_pointCount > 15) {
            diff = pos - u_points[15] * u_resolution;
            if (dot(diff, diff) < radiusSquared) color = vec3(1.0);
        }
        if (u_pointCount > 16) {
            diff = pos - u_points[16] * u_resolution;
            if (dot(diff, diff) < radiusSquared) color = vec3(1.0);
        }
        if (u_pointCount > 17) {
            diff = pos - u_points[17] * u_resolution;
            if (dot(diff, diff) < radiusSquared) color = vec3(1.0);
        }
        if (u_pointCount > 18) {
            diff = pos - u_points[18] * u_resolution;
            if (dot(diff, diff) < radiusSquared) color = vec3(1.0);
        }
        if (u_pointCount > 19) {
            diff = pos - u_points[19] * u_resolution;
            if (dot(diff, diff) < radiusSquared) color = vec3(1.0);
        }
        if (u_pointCount > 20) {
            diff = pos - u_points[20] * u_resolution;
            if (dot(diff, diff) < radiusSquared) color = vec3(1.0);
        }
        if (u_pointCount > 21) {
            diff = pos - u_points[21] * u_resolution;
            if (dot(diff, diff) < radiusSquared) color = vec3(1.0);
        }
        if (u_pointCount > 22) {
            diff = pos - u_points[22] * u_resolution;
            if (dot(diff, diff) < radiusSquared) color = vec3(1.0);
        }
        if (u_pointCount > 23) {
            diff = pos - u_points[23] * u_resolution;
            if (dot(diff, diff) < radiusSquared) color = vec3(1.0);
        }
        if (u_pointCount > 24) {
            diff = pos - u_points[24] * u_resolution;
            if (dot(diff, diff) < radiusSquared) color = vec3(1.0);
        }
        if (u_pointCount > 25) {
            diff = pos - u_points[25] * u_resolution;
            if (dot(diff, diff) < radiusSquared) color = vec3(1.0);
        }
        if (u_pointCount > 26) {
            diff = pos - u_points[26] * u_resolution;
            if (dot(diff, diff) < radiusSquared) color = vec3(1.0);
        }
        if (u_pointCount > 27) {
            diff = pos - u_points[27] * u_resolution;
            if (dot(diff, diff) < radiusSquared) color = vec3(1.0);
        }
        if (u_pointCount > 28) {
            diff = pos - u_points[28] * u_resolution;
            if (dot(diff, diff) < radiusSquared) color = vec3(1.0);
        }
        if (u_pointCount > 29) {
            diff = pos - u_points[29] * u_resolution;
            if (dot(diff, diff) < radiusSquared) color = vec3(1.0);
        }
        if (u_pointCount > 30) {
            diff = pos - u_points[30] * u_resolution;
            if (dot(diff, diff) < radiusSquared) color = vec3(1.0);
        }
        if (u_pointCount > 31) {
            diff = pos - u_points[31] * u_resolution;
            if (dot(diff, diff) < radiusSquared) color = vec3(1.0);
        }
        if (u_pointCount > 32) {
            diff = pos - u_points[32] * u_resolution;
            if (dot(diff, diff) < radiusSquared) color = vec3(1.0);
        }
        if (u_pointCount > 33) {
            diff = pos - u_points[33] * u_resolution;
            if (dot(diff, diff) < radiusSquared) color = vec3(1.0);
        }
        if (u_pointCount > 34) {
            diff = pos - u_points[34] * u_resolution;
            if (dot(diff, diff) < radiusSquared) color = vec3(1.0);
        }
        if (u_pointCount > 35) {
            diff = pos - u_points[35] * u_resolution;
            if (dot(diff, diff) < radiusSquared) color = vec3(1.0);
        }
        if (u_pointCount > 36) {
            diff = pos - u_points[36] * u_resolution;
            if (dot(diff, diff) < radiusSquared) color = vec3(1.0);
        }
        if (u_pointCount > 37) {
            diff = pos - u_points[37] * u_resolution;
            if (dot(diff, diff) < radiusSquared) color = vec3(1.0);
        }
        if (u_pointCount > 38) {
            diff = pos - u_points[38] * u_resolution;
            if (dot(diff, diff) < radiusSquared) color = vec3(1.0);
        }
        if (u_pointCount > 39) {
            diff = pos - u_points[39] * u_resolution;
            if (dot(diff, diff) < radiusSquared) color = vec3(1.0);
        }
        if (u_pointCount > 40) {
            diff = pos - u_points[40] * u_resolution;
            if (dot(diff, diff) < radiusSquared) color = vec3(1.0);
        }
        if (u_pointCount > 41) {
            diff = pos - u_points[41] * u_resolution;
            if (dot(diff, diff) < radiusSquared) color = vec3(1.0);
        }
        if (u_pointCount > 42) {
            diff = pos - u_points[42] * u_resolution;
            if (dot(diff, diff) < radiusSquared) color = vec3(1.0);
        }
        if (u_pointCount > 43) {
            diff = pos - u_points[43] * u_resolution;
            if (dot(diff, diff) < radiusSquared) color = vec3(1.0);
        }
        if (u_pointCount > 44) {
            diff = pos - u_points[44] * u_resolution;
            if (dot(diff, diff) < radiusSquared) color = vec3(1.0);
        }
        if (u_pointCount > 45) {
            diff = pos - u_points[45] * u_resolution;
            if (dot(diff, diff) < radiusSquared) color = vec3(1.0);
        }
        if (u_pointCount > 46) {
            diff = pos - u_points[46] * u_resolution;
            if (dot(diff, diff) < radiusSquared) color = vec3(1.0);
        }
        if (u_pointCount > 47) {
            diff = pos - u_points[47] * u_resolution;
            if (dot(diff, diff) < radiusSquared) color = vec3(1.0);
        }
        if (u_pointCount > 48) {
            diff = pos - u_points[48] * u_resolution;
            if (dot(diff, diff) < radiusSquared) color = vec3(1.0);
        }
        if (u_pointCount > 49) {
            diff = pos - u_points[49] * u_resolution;
            if (dot(diff, diff) < radiusSquared) color = vec3(1.0);
        }
    }
    
    gl_FragColor = vec4(color, 1.0);
} 