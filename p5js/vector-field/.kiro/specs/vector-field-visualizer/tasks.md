# Implementation Plan

- [ ] 1. Set up project structure and dependencies
  - Create HTML file with canvas element and UI container
  - Create CSS file for styling (fullscreen canvas, floating UI)
  - Create main JavaScript file with module structure
  - Install math.js and MathLive via CDN or npm
  - Set up basic HTML structure with canvas and UI panels
  - _Requirements: 11.1, 11.2, 11.4, 12.1, 12.3_

- [ ] 2. Implement coordinate system module
- [ ] 2.1 Create CoordinateSystem class with zoom and pan state
  - Implement constructor with canvas dimensions
  - Add zoom level and offset properties
  - _Requirements: 3.1, 4.1_

- [ ] 2.2 Implement coordinate transformation methods
  - Write mathToPixel(x, y) method with y-axis inversion
  - Write pixelToMath(x, y) method with y-axis inversion
  - _Requirements: 6.5_

- [ ] 2.3 Write property test for coordinate transformation
  - **Property 19: Coordinate transformation y-axis inversion**
  - **Validates: Requirements 6.5**

- [ ] 2.4 Implement zoom functionality
  - Write zoom(factor, centerPixelX, centerPixelY) method
  - Ensure mathematical point under cursor stays fixed
  - _Requirements: 3.1_

- [ ] 2.5 Write property test for zoom invariant
  - **Property 8: Zoom preserves point under cursor**
  - **Validates: Requirements 3.1**

- [ ] 2.6 Implement pan functionality
  - Write pan(deltaPixelX, deltaPixelY) method
  - Ensure zoom level remains unchanged
  - _Requirements: 4.1, 4.3_

- [ ] 2.7 Write property test for pan preserves zoom
  - **Property 13: Pan preserves zoom**
  - **Validates: Requirements 4.3**

- [ ] 2.8 Write property test for pan direction
  - **Property 12: Pan direction correctness**
  - **Validates: Requirements 4.1**

- [ ] 2.9 Implement grid spacing calculation
  - Write getGridSpacing() method using (1, 2, 5) × 10^n sequence
  - Calculate spacing based on zoom to maintain bounded grid line count
  - _Requirements: 5.1, 5.2_

- [ ] 2.10 Write property test for grid spacing sequence
  - **Property 9: Grid spacing follows allowed sequence**
  - **Validates: Requirements 5.1**

- [ ] 2.11 Write property test for grid density bounds
  - **Property 10: Grid density stays bounded**
  - **Validates: Requirements 3.2, 5.2**

- [ ] 2.12 Write property test for grid spacing progression
  - **Property 11: Grid spacing progression**
  - **Validates: Requirements 3.2**

- [ ] 2.13 Implement visible bounds calculation
  - Write getVisibleBounds() method returning {minX, maxX, minY, maxY}
  - _Requirements: 2.1, 5.3, 5.4_

- [ ] 3. Implement expression evaluator module
- [ ] 3.1 Create ExpressionEvaluator class wrapping math.js
  - Implement constructor accepting xExpression and yExpression strings
  - Set up math.js parser with allowed functions
  - Restrict variables to only x and y
  - _Requirements: 1.2, 1.4_

- [ ] 3.2 Implement expression evaluation
  - Write evaluate(x, y) method returning {x: number, y: number}
  - Handle NaN and Infinity gracefully (return null for invalid points)
  - _Requirements: 1.2, 1.5_

- [ ] 3.3 Write property test for valid expression evaluation
  - **Property 1: Valid expression evaluation produces numeric results**
  - **Validates: Requirements 1.2**

- [ ] 3.4 Implement expression validation
  - Write isValid() method checking syntax
  - Write getError() method returning error messages
  - Detect syntax errors and undefined variables
  - _Requirements: 1.3, 1.4_

- [ ] 3.5 Write property test for syntax error detection
  - **Property 2: Syntax errors are detected**
  - **Validates: Requirements 1.3**

- [ ] 3.6 Write property test for undefined variable detection
  - **Property 3: Undefined variables are detected**
  - **Validates: Requirements 1.4**

- [ ] 3.7 Write property test for graceful NaN/Infinity handling
  - **Property 4: Invalid values are handled gracefully**
  - **Validates: Requirements 1.5**

- [ ] 4. Implement grid renderer module
- [ ] 4.1 Create GridRenderer class
  - Implement constructor accepting coordinateSystem and canvas
  - _Requirements: 5.1, 5.2_

- [ ] 4.2 Implement grid line rendering
  - Write render(ctx) method
  - Calculate and draw horizontal and vertical grid lines
  - Draw axes with distinct styling
  - _Requirements: 5.1, 5.2_

- [ ] 4.3 Implement axis label rendering
  - Calculate label positions at grid-axis intersections
  - Handle off-screen axes by placing labels at screen edges
  - Format numbers appropriately for current scale
  - _Requirements: 5.3, 5.4_

- [ ] 4.4 Write property test for axis labels at intersections
  - **Property 14: Axis labels at intersections**
  - **Validates: Requirements 5.3**

- [ ] 4.5 Write property test for off-screen axis labels
  - **Property 15: Off-screen axis labels at edges**
  - **Validates: Requirements 5.4**

- [ ] 5. Implement vector renderer module
- [ ] 5.1 Create VectorRenderer class
  - Implement constructor accepting coordinateSystem and expressionEvaluator
  - _Requirements: 2.1_

- [ ] 5.2 Implement vector field rendering
  - Write render(ctx) method
  - Calculate vectors at all visible grid intersections
  - Skip points that evaluate to NaN/Infinity
  - _Requirements: 2.1, 1.5_

- [ ] 5.3 Write property test for arrows at grid intersections
  - **Property 5: Arrows exist at all grid intersections**
  - **Validates: Requirements 2.1**

- [ ] 5.4 Implement arrow length normalization
  - Scale arrows so maximum length equals grid spacing
  - _Requirements: 2.2_

- [ ] 5.5 Write property test for arrow length constraint
  - **Property 6: Arrow length constraint**
  - **Validates: Requirements 2.2**

- [ ] 5.6 Implement arrow thickness scaling
  - Calculate relative magnitudes of all visible vectors
  - Scale thickness proportionally to magnitude
  - _Requirements: 2.3_

- [ ] 5.7 Write property test for arrow thickness
  - **Property 7: Arrow thickness reflects relative magnitude**
  - **Validates: Requirements 2.3**

- [ ] 5.8 Implement arrow drawing helper
  - Write drawArrow(ctx, x, y, dx, dy, thickness) method
  - Draw arrow with proper direction and styling
  - _Requirements: 2.1, 2.2, 2.3_

- [ ] 6. Implement particle system module
- [ ] 6.1 Create Particle class
  - Implement constructor with position, maxLifetime
  - Add properties: position, trail, age, maxLifetime
  - Write isAlive() method
  - _Requirements: 6.1, 6.4_

- [ ] 6.2 Create ParticleSystem class
  - Implement constructor accepting coordinateSystem and expressionEvaluator
  - Initialize particle array and configuration (max particles, trail length)
  - _Requirements: 6.1, 10.1_

- [ ] 6.3 Implement particle creation methods
  - Write addParticle(mathX, mathY) method
  - Enforce maximum particle count
  - _Requirements: 6.1, 10.1_

- [ ] 6.4 Write property test for particle count limit
  - **Property 23: Particle count stays within limit**
  - **Validates: Requirements 10.1**

- [ ] 6.5 Implement particle physics update
  - Write update(deltaTime, isPaused) method
  - Update positions using Euler integration: pos += velocity × dt
  - Evaluate vector field at particle position for velocity
  - Update particle age and remove dead particles
  - Skip update if paused
  - _Requirements: 6.2, 6.4, 8.1_

- [ ] 6.6 Write property test for particle movement
  - **Property 16: Particle movement follows vector field**
  - **Validates: Requirements 6.2**

- [ ] 6.7 Write property test for particle despawn
  - **Property 18: Particles despawn after lifetime**
  - **Validates: Requirements 6.4**

- [ ] 6.8 Implement particle trail management
  - Update trail array in update method
  - Limit trail length to prevent memory issues
  - _Requirements: 6.3_

- [ ] 6.9 Write property test for particle trails
  - **Property 17: Particles have trails**
  - **Validates: Requirements 6.3**

- [ ] 6.10 Implement particle rendering
  - Write render(ctx) method
  - Draw trails with decreasing opacity
  - Draw particles as circles
  - _Requirements: 6.3_

- [ ] 6.11 Implement grid spawn functionality
  - Write addParticleGrid(bounds, density) method
  - Create evenly spaced particles across visible area
  - _Requirements: 7.1_

- [ ] 6.12 Write property test for grid spawn distribution
  - **Property 20: Grid spawn distribution**
  - **Validates: Requirements 7.1**

- [ ] 6.13 Implement brush spawn functionality
  - Write addParticlesInBrush(mathX, mathY, thickness, density) method
  - Spawn particles within brush radius
  - Respect thickness and density parameters
  - _Requirements: 9.1, 9.2, 9.3_

- [ ] 6.14 Write property test for brush thickness
  - **Property 21: Brush respects thickness**
  - **Validates: Requirements 9.2**

- [ ] 6.15 Write property test for brush density
  - **Property 22: Brush respects density**
  - **Validates: Requirements 9.3**

- [ ] 6.16 Implement clear particles method
  - Write clear() method to remove all particles
  - _Requirements: 6.1_

- [ ] 7. Implement input handler module
- [ ] 7.1 Create InputHandler class
  - Implement constructor accepting canvas, coordinateSystem, particleSystem
  - Store interaction state (mouse position, dragging, brush mode)
  - _Requirements: 3.1, 4.1, 6.1, 9.1_

- [ ] 7.2 Implement mouse event handlers
  - Write handleMouseDown, handleMouseMove, handleMouseUp
  - Track mouse position in pixel coordinates
  - Distinguish between click and drag
  - _Requirements: 6.1, 9.1_

- [ ] 7.3 Implement click particle creation
  - In handleMouseDown, create particle at click location (when not in brush mode)
  - Convert pixel coordinates to math coordinates
  - _Requirements: 6.1_

- [ ] 7.4 Implement brush drawing
  - In handleMouseMove during drag (when in brush mode), spawn particles along path
  - Use brush thickness and density settings
  - _Requirements: 9.1, 9.2, 9.3_

- [ ] 7.5 Implement wheel/scroll handler for zoom
  - Write handleWheel(event) method
  - Calculate zoom factor from wheel delta
  - Call coordinateSystem.zoom() centered on mouse position
  - Prevent default scroll behavior
  - _Requirements: 3.1_

- [ ] 7.6 Implement touch/pinch gesture handler
  - Write handlePinch(event) method for touch devices
  - Calculate zoom from pinch gesture
  - _Requirements: 3.1_

- [ ] 7.7 Implement scroll/drag panning
  - Add pan mode detection (e.g., shift+drag or middle mouse)
  - Call coordinateSystem.pan() with drag delta
  - _Requirements: 4.1_

- [ ] 8. Implement UI controller module
- [ ] 8.1 Create UIController class
  - Implement constructor accepting particleSystem and expressionEvaluator
  - Store references to UI elements
  - _Requirements: 1.1, 8.1, 9.4_

- [ ] 8.2 Create HTML UI structure
  - Add expression input panel with MathLive fields
  - Add control panel with pause, spawn grid, clear buttons
  - Add brush controls (mode toggle, thickness slider, density slider)
  - Style as floating panels over canvas
  - _Requirements: 1.1, 8.1, 9.4, 11.3_

- [ ] 8.3 Initialize MathLive math fields
  - Write setupMathFields() method
  - Create MathLive editors for vx and vy expressions
  - Configure MathLive options (virtual keyboard, etc.)
  - _Requirements: 1.1_

- [ ] 8.4 Implement expression update handler
  - Write updateExpressions(xExpr, yExpr) method
  - Convert LaTeX from MathLive to math.js format
  - Update expressionEvaluator
  - Display validation errors if invalid
  - Trigger re-render
  - _Requirements: 1.2, 1.3, 1.4, 1.6_

- [ ] 8.5 Implement pause/resume control
  - Write togglePause() method
  - Update button state
  - Pass pause state to particle system
  - _Requirements: 8.1_

- [ ] 8.6 Implement spawn grid button
  - Write spawnParticleGrid() method
  - Get visible bounds from coordinateSystem
  - Call particleSystem.addParticleGrid()
  - _Requirements: 7.1_

- [ ] 8.7 Implement clear button
  - Call particleSystem.clear()
  - _Requirements: 6.1_

- [ ] 8.8 Implement brush controls
  - Write setBrushThickness(value) and setBrushDensity(value) methods
  - Update brush mode toggle
  - Connect sliders to settings
  - _Requirements: 9.2, 9.3, 9.4_

- [ ] 9. Implement main application loop
- [ ] 9.1 Create main App class
  - Initialize canvas and get 2D context
  - Create instances of all modules
  - Set up default vector field (e.g., vx = -y, vy = x)
  - _Requirements: 1.1, 11.1_

- [ ] 9.2 Implement canvas resize handling
  - Write handleResize() method
  - Update canvas dimensions to match viewport
  - Update coordinateSystem dimensions
  - _Requirements: 11.2_

- [ ] 9.3 Implement render loop
  - Write render() method using requestAnimationFrame
  - Clear canvas
  - Call gridRenderer.render()
  - Call vectorRenderer.render()
  - Call particleSystem.render()
  - _Requirements: 2.1, 2.4, 5.1, 5.2_

- [ ] 9.4 Implement update loop
  - Write update(deltaTime) method
  - Call particleSystem.update()
  - Track time for physics integration
  - _Requirements: 6.2, 10.2_

- [ ] 9.5 Implement main loop
  - Write animate() method combining update and render
  - Calculate deltaTime between frames
  - Call requestAnimationFrame recursively
  - _Requirements: 10.2_

- [ ] 9.6 Initialize application on page load
  - Set up event listeners for window resize
  - Start animation loop
  - Initialize UI
  - _Requirements: 1.1, 11.1, 11.2_

- [ ] 10. Checkpoint - Ensure all tests pass
  - Ensure all tests pass, ask the user if questions arise.

- [ ] 11. Polish and refinement
- [ ] 11.1 Add visual polish
  - Refine colors, line widths, and styling
  - Add subtle animations or transitions
  - Ensure UI is visually appealing
  - _Requirements: 11.3_

- [ ] 11.2 Optimize performance
  - Profile rendering and identify bottlenecks
  - Optimize particle rendering (batch operations)
  - Consider using offscreen canvas for grid if needed
  - _Requirements: 10.2_

- [ ] 11.3 Add keyboard shortcuts
  - Space bar for pause/resume
  - Escape to clear particles
  - Plus/minus for zoom
  - _Requirements: 8.1_

- [ ] 11.4 Add example vector fields
  - Create preset buttons for common fields (rotation, saddle, source, sink)
  - _Requirements: 1.1_

- [ ] 12. Final checkpoint - Ensure all tests pass
  - Ensure all tests pass, ask the user if questions arise.
