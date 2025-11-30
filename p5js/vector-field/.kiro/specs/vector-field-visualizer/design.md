# Design Document

## Overview

The Vector Field Visualizer is a web-based application that allows users to visualize 2D vector fields through interactive graphics. The system consists of several key components: a mathematical expression parser/evaluator, a coordinate transformation system, a rendering engine using HTML Canvas, and a particle simulation system. The application provides real-time visualization with zoom, pan, and particle interaction capabilities.

## UI Mockup

```
┌─────────────────────────────────────────────────────────────────────┐
│                                                                     │
│  ┌──────────────────────────────────────────────────────────┐     │
│  │ Vector Field:  vx = [  -y  ]  vy = [   x  ]  [Update]   │     │
│  └──────────────────────────────────────────────────────────┘     │
│                                                                     │
│         5 │                    ↑                                   │
│           │              ↗     ↑     ↖                             │
│           │                                                         │
│           │         →    →     •     ←    ←                        │
│           │                                                         │
│           │              ↘     ↓     ↙                             │
│  ─────────┼─────────────────────────────────────────── 5          │
│         0 │                    ↓                                   │
│           │                                                         │
│           │         Grid lines with arrows at intersections        │
│           │         Particles (•) with transparent trails          │
│           │                                                         │
│        -5 │                                                         │
│           0                                                         │
│                                                                     │
│  ┌────────────────────────────────────────────────────────┐       │
│  │  [⏸ Pause]  [⊞ Spawn Grid]  [Clear]                   │       │
│  │                                                         │       │
│  │  Brush Mode: [○ Click] [● Draw]                        │       │
│  │  Thickness: [━━━━━●────] 5.0                           │       │
│  │  Density:   [━━━●──────] 3.0                           │       │
│  └────────────────────────────────────────────────────────┘       │
│                                                                     │
└─────────────────────────────────────────────────────────────────────┘

Interactions:
- Pinch/Scroll wheel: Zoom in/out from mouse position
- Scroll/Drag: Pan the view
- Click: Add single particle (Click mode)
- Drag: Draw particles with brush (Draw mode)
- Grid lines adapt spacing: 0.1, 0.2, 0.5, 1, 2, 5, 10, 20, 50...
- Axis labels appear at grid intersections or screen edges
```

## Architecture

The application follows a modular architecture with clear separation of concerns:

1. **Expression Module**: Wraps math.js library to parse and evaluate mathematical expressions for vector field components
2. **Math Input Module**: Integrates MathLive for LaTeX-style math expression input, converts to math.js format
3. **Coordinate System Module**: Handles transformations between math space and pixel space, manages zoom and pan state
4. **Grid Renderer**: Draws grid lines, axes, and labels with adaptive spacing
5. **Vector Renderer**: Calculates and draws vector arrows at grid intersections
6. **Particle System**: Manages particle lifecycle, physics simulation, and trail rendering
7. **Input Handler**: Processes user interactions (mouse, touch, keyboard) on canvas
8. **UI Controller**: Manages floating UI elements, MathLive editors, and their state
9. **Main Application Loop**: Coordinates updates and rendering

The architecture uses a single-threaded event loop with requestAnimationFrame for smooth rendering.

**Dependencies:**
- **math.js** (or expr-eval): Lightweight mathematical expression parser and evaluator
- **MathLive**: Modern math input editor with LaTeX-style rendering (no jQuery, actively maintained)
- **fast-check**: Property-based testing library (dev dependency)

## Components and Interfaces

### Expression Evaluator

```javascript
class ExpressionEvaluator {
  constructor(xExpression, yExpression)
  evaluate(x, y) // Returns {x: number, y: number}
  isValid() // Returns boolean
  getError() // Returns string or null
}
```

Responsibilities:
- Parse mathematical expressions using **math.js** or similar lightweight library
- Evaluate vector field at any point (x, y)
- Validate expressions and provide error messages
- Support common mathematical functions (sin, cos, exp, sqrt, etc.)
- Provide safe evaluation (no arbitrary code execution)

### Coordinate System

```javascript
class CoordinateSystem {
  constructor(canvasWidth, canvasHeight)
  
  // Transform methods
  mathToPixel(mathX, mathY) // Returns {x: number, y: number}
  pixelToMath(pixelX, pixelY) // Returns {x: number, y: number}
  
  // View manipulation
  zoom(factor, centerPixelX, centerPixelY)
  pan(deltaPixelX, deltaPixelY)
  
  // Grid calculation
  getGridSpacing() // Returns number (in math units)
  getVisibleBounds() // Returns {minX, maxX, minY, maxY}
}
```

Responsibilities:
- Maintain zoom level and pan offset
- Convert between math space and pixel space (handling y-axis inversion)
- Calculate appropriate grid spacing based on zoom level
- Determine visible mathematical bounds

### Grid Renderer

```javascript
class GridRenderer {
  constructor(coordinateSystem, canvas)
  
  render(ctx)
  // Draws grid lines, axes, and numeric labels
}
```

Responsibilities:
- Calculate grid line positions using (1, 2, 5) × 10^n spacing
- Draw horizontal and vertical grid lines
- Draw x and y axes with distinct styling
- Render numeric labels at axis intersections or screen edges
- Maintain consistent visual density of grid lines

### Vector Renderer

```javascript
class VectorRenderer {
  constructor(coordinateSystem, expressionEvaluator)
  
  render(ctx)
  // Draws vector arrows at grid intersections
}
```

Responsibilities:
- Calculate vector field values at all visible grid intersections
- Normalize arrow lengths to not exceed grid spacing
- Calculate relative magnitudes for thickness scaling
- Draw arrows with appropriate direction, length, and thickness

### Particle System

```javascript
class Particle {
  constructor(x, y, maxLifetime)
  
  position // {x: number, y: number} in math space
  trail // Array of positions
  age // number
  maxLifetime // number
  isAlive() // Returns boolean
}

class ParticleSystem {
  constructor(coordinateSystem, expressionEvaluator)
  
  addParticle(mathX, mathY)
  addParticleGrid(bounds, density)
  addParticlesInBrush(mathX, mathY, thickness, density)
  
  update(deltaTime, isPaused)
  render(ctx)
  
  clear()
}
```

Responsibilities:
- Manage collection of active particles
- Update particle positions based on vector field (Euler integration)
- Maintain particle trails with transparency
- Remove particles after lifetime expires
- Render particles and trails to canvas

### Input Handler

```javascript
class InputHandler {
  constructor(canvas, coordinateSystem, particleSystem)
  
  // Event handlers registered internally
  handleMouseDown(event)
  handleMouseMove(event)
  handleMouseUp(event)
  handleWheel(event)
  handlePinch(event)
}
```

Responsibilities:
- Process mouse and touch events
- Distinguish between click, drag, and brush modes
- Handle zoom (pinch) and pan (scroll) gestures
- Coordinate with particle system for particle creation
- Prevent default browser behaviors where appropriate

### UI Controller

```javascript
class UIController {
  constructor(particleSystem, expressionEvaluator)
  
  setupUI()
  setupMathFields() // Initialize MathLive editors
  updateExpressions(xExpr, yExpr)
  togglePause()
  spawnParticleGrid()
  setBrushThickness(value)
  setBrushDensity(value)
}
```

Responsibilities:
- Create and position floating UI elements
- Initialize MathLive math field editors for expression input
- Handle UI interactions (buttons, inputs, sliders)
- Convert LaTeX from MathLive to math.js compatible expressions
- Update application state based on user input
- Display error messages for invalid expressions

## Data Models

### Vector Field State

```javascript
{
  xExpression: string,  // e.g., "-y"
  yExpression: string,  // e.g., "x"
  evaluator: ExpressionEvaluator
}
```

### View State

```javascript
{
  zoom: number,          // Scale factor (pixels per math unit)
  offsetX: number,       // Pan offset in pixels
  offsetY: number,       // Pan offset in pixels
  canvasWidth: number,
  canvasHeight: number
}
```

### Particle Data

```javascript
{
  position: {x: number, y: number},  // Math space coordinates
  trail: [{x: number, y: number}],   // Array of past positions
  age: number,                        // Time alive in seconds
  maxLifetime: number                 // Time before despawn in seconds
}
```

### Grid Configuration

```javascript
{
  spacing: number,           // Current grid spacing in math units
  maxGridLines: number,      // Maximum grid lines to display
  spacingSequence: [1, 2, 5] // Multipliers for 10^n
}
```


## Correctness Properties

*A property is a characteristic or behavior that should hold true across all valid executions of a system—essentially, a formal statement about what the system should do. Properties serve as the bridge between human-readable specifications and machine-verifiable correctness guarantees.*

### Property 1: Valid expression evaluation produces numeric results

*For any* valid mathematical expression and any point (x, y), evaluating the expression should produce finite numeric values for both x and y components of the vector.

**Validates: Requirements 1.2**

### Property 2: Syntax errors are detected

*For any* expression with invalid mathematical syntax, the expression evaluator should return a syntax error and prevent evaluation.

**Validates: Requirements 1.3**

### Property 3: Undefined variables are detected

*For any* expression containing variables other than x and y, the expression evaluator should return an undefined variable error and prevent evaluation.

**Validates: Requirements 1.4**

### Property 4: Invalid values are handled gracefully

*For any* point (x, y) where the expression evaluates to infinity or NaN, the system should skip rendering a vector at that point without affecting other valid vectors.

**Validates: Requirements 1.5**

### Property 5: Arrows exist at all grid intersections

*For any* viewport configuration and grid spacing, the number of rendered arrows should equal the number of visible grid line intersections.

**Validates: Requirements 2.1**

### Property 6: Arrow length constraint

*For any* rendered arrow, its length in pixel space should not exceed the grid spacing in pixel space.

**Validates: Requirements 2.2**

### Property 7: Arrow thickness reflects relative magnitude

*For any* set of visible arrows, arrows with larger vector magnitudes should have proportionally greater thickness than arrows with smaller magnitudes.

**Validates: Requirements 2.3**

### Property 8: Zoom preserves point under cursor

*For any* zoom operation centered at pixel coordinates (px, py), the mathematical point at those coordinates before zoom should remain at the same pixel coordinates after zoom.

**Validates: Requirements 3.1**

### Property 9: Grid spacing follows allowed sequence

*For any* zoom level, the grid spacing should be of the form (1, 2, or 5) × 10^n where n is an integer.

**Validates: Requirements 5.1**

### Property 10: Grid density stays bounded

*For any* zoom level, the number of visible grid lines (horizontal + vertical) should not exceed the configured maximum.

**Validates: Requirements 3.2, 5.2**

### Property 11: Grid spacing progression

*For any* sequence of zoom operations, grid spacing should progress through the sequence (..., 0.1, 0.2, 0.5, 1, 2, 5, 10, 20, 50, ...) in the appropriate direction.

**Validates: Requirements 3.2**

### Property 12: Pan direction correctness

*For any* scroll/pan operation with delta (dx, dy), the view offset should change in the corresponding direction such that content appears to move with the scroll direction.

**Validates: Requirements 4.1**

### Property 13: Pan preserves zoom

*For any* pan operation, the zoom level before and after panning should remain identical.

**Validates: Requirements 4.3**

### Property 14: Axis labels at intersections

*For any* viewport where an axis is visible, numeric labels should appear at the intersections of that axis with grid lines.

**Validates: Requirements 5.3**

### Property 15: Off-screen axis labels at edges

*For any* viewport where an axis is not visible, numeric labels should appear at the screen edge closest to that axis, aligned with grid lines.

**Validates: Requirements 5.4**

### Property 16: Particle movement follows vector field

*For any* particle at position (x, y), after a time step dt, the particle's new position should be approximately (x + vx×dt, y + vy×dt) where (vx, vy) is the vector field at (x, y).

**Validates: Requirements 6.2**

### Property 17: Particles have trails

*For any* particle that has moved, its trail array should contain previous positions with decreasing opacity.

**Validates: Requirements 6.3**

### Property 18: Particles despawn after lifetime

*For any* particle, when its age exceeds its maximum lifetime, it should be removed from the active particle list.

**Validates: Requirements 6.4**

### Property 19: Coordinate transformation y-axis inversion

*For any* point (mx, my) in math space, converting to pixel space and back should yield the original point, with the y-axis correctly inverted (positive y in math space corresponds to negative y in pixel space).

**Validates: Requirements 6.5**

### Property 20: Grid spawn distribution

*For any* grid spawn operation, particles should be distributed with approximately equal spacing across the visible viewport.

**Validates: Requirements 7.1**

### Property 21: Brush respects thickness

*For any* brush stroke with configured thickness T, particles should be spawned within distance T/2 from the brush path centerline.

**Validates: Requirements 9.2**

### Property 22: Brush respects density

*For any* brush stroke with configured density D, the number of particles spawned per unit length should be proportional to D.

**Validates: Requirements 9.3**

### Property 23: Particle count stays within limit

*For any* state of the system, the total number of active particles should not exceed the configured maximum particle limit.

**Validates: Requirements 10.1**

## Error Handling

### Expression Evaluation Errors

- **Invalid Syntax**: Display clear error message indicating the syntax problem and prevent rendering
- **Undefined Variables**: Only allow x and y as variables; reject others and prevent rendering
- **NaN/Infinity at Points**: Skip rendering vectors at individual points that evaluate to NaN or Infinity, but continue rendering valid vectors elsewhere (allows fields with asymptotes)

### Coordinate Transformation Errors

- **Extreme Zoom Levels**: Clamp zoom to reasonable bounds (e.g., 0.001x to 1000x)
- **Numerical Precision**: Handle floating-point precision issues in grid calculations
- **Canvas Bounds**: Ensure all rendering stays within canvas dimensions

### Particle System Errors

- **Too Many Particles**: Implement maximum particle count to prevent performance degradation
- **Invalid Positions**: Validate particle positions are finite numbers
- **Memory Management**: Properly clean up particle trails to prevent memory leaks

### Input Handling Errors

- **Rapid Input**: Debounce or throttle high-frequency events
- **Touch vs Mouse**: Properly distinguish and handle both input types
- **Event Conflicts**: Prevent default browser behaviors that interfere with app

## Testing Strategy

The Vector Field Visualizer will employ a dual testing approach combining unit tests and property-based tests to ensure comprehensive correctness.

### Unit Testing Approach

Unit tests will verify specific examples, edge cases, and integration points:

- **Expression Parser**: Test parsing of common expressions (e.g., "sin(x)", "x*y", "-y")
- **Coordinate Transformations**: Test specific conversions at known zoom/pan states
- **Grid Spacing Calculation**: Test spacing at specific zoom levels
- **UI Integration**: Test that UI controls trigger correct state changes
- **Edge Cases**: Test behavior at zoom extremes, empty particle lists, zero vectors

Unit tests will use a standard JavaScript testing framework (Jest or similar) and will focus on concrete scenarios that demonstrate correct behavior.

### Property-Based Testing Approach

Property-based tests will verify universal properties across randomly generated inputs using **fast-check** (a property-based testing library for JavaScript).

**Configuration**:
- Each property test will run a minimum of 100 iterations
- Tests will use custom generators for domain-specific types (expressions, coordinates, zoom levels)
- Each test will be tagged with a comment referencing the design document property

**Property Test Tags**:
Each property-based test will include a comment in this format:
```javascript
// Feature: vector-field-visualizer, Property 1: Valid expression evaluation produces numeric results
```

**Test Coverage**:
- **Expression Evaluation** (Properties 1-2): Generate random valid/invalid expressions and test evaluation
- **Rendering Constraints** (Properties 3-5): Generate random viewport states and verify arrow properties
- **Coordinate Transformations** (Properties 6, 11, 17): Generate random zoom/pan operations and verify invariants
- **Grid System** (Properties 7-9, 12-13): Generate random zoom levels and verify grid properties
- **Particle Physics** (Properties 14-16): Generate random particle states and verify movement/lifecycle
- **User Interactions** (Properties 18-20): Generate random brush/spawn parameters and verify behavior

**Generators**:
Custom generators will be created for:
- Valid mathematical expressions (using allowed operators and functions)
- Invalid expressions (malformed syntax, undefined variables)
- Coordinate pairs (x, y) within reasonable bounds
- Zoom levels (positive numbers in valid range)
- Pan offsets (pixel coordinates)
- Particle configurations (position, age, lifetime)
- Brush parameters (thickness, density)

### Integration Testing

While not property-based, integration tests will verify:
- Complete user workflows (input expression → see visualization → add particles → observe motion)
- Canvas rendering produces expected visual output
- UI state synchronization across components
- Performance under load (many particles, rapid zoom/pan)

### Test Execution

- Unit tests and property tests will run on every code change
- Property tests will use deterministic random seeds for reproducibility
- Failed property tests will provide minimal failing examples for debugging
- Test coverage will aim for >90% of core logic (excluding UI boilerplate)

