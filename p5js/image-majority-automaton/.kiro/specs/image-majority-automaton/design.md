# Design Document: Image Dithering Majority Automaton

## Overview

The Image Dithering Majority Automaton is a web-based application that combines image processing with cellular automaton simulation. Users upload images, apply GPU-accelerated ordered dithering to reduce the color palette, and then run a majority-rule cellular automaton on the quantized result. The application emphasizes pixel-perfect rendering, real-time performance through WebGL, and a clean dark-themed interface.

The system is built using vanilla HTML, CSS, and JavaScript with WebGL/GLSL for GPU acceleration. No external frameworks or build tools are required—the application runs directly in the browser.

## Architecture

### High-Level Architecture

The application follows a modular architecture with clear separation of concerns:

```
┌─────────────────────────────────────────────────────────┐
│                     User Interface                       │
│  (HTML/CSS - Controls, Canvas, Error Display)           │
└────────────────────┬────────────────────────────────────┘
                     │
┌────────────────────▼────────────────────────────────────┐
│              Application Controller                      │
│  (app.js - State Management, Event Handling)            │
└────────────────────┬────────────────────────────────────┘
                     │
┌────────────────────▼────────────────────────────────────┐
│              WebGL Renderer                              │
│  (webgl-renderer.js - GPU Operations, Shader Mgmt)      │
└────────────────────┬────────────────────────────────────┘
                     │
┌────────────────────▼────────────────────────────────────┐
│                  GPU Shaders                             │
│  (GLSL - Dithering, Automaton, Copy Operations)         │
└──────────────────────────────────────────────────────────┘
```

**Design Rationale**: This layered architecture separates UI concerns from rendering logic and GPU operations. The Application Controller manages state and user interactions, delegating all graphics operations to the WebGL Renderer. This makes the codebase maintainable and allows the renderer to be tested independently.

### Component Responsibilities

1. **Application Controller** (`app.js`)
   - Manages application state (uploaded image, palette size, animation state)
   - Handles user input events (file upload, button clicks, slider changes)
   - Validates image formats and dimensions
   - Coordinates between UI and renderer
   - Displays error and warning messages

2. **WebGL Renderer** (`webgl-renderer.js`)
   - Initializes WebGL context and checks for support
   - Loads and compiles GLSL shaders from separate files
   - Manages textures and framebuffers for ping-pong rendering
   - Executes dithering and automaton operations on GPU
   - Provides utility methods for texture creation and rendering

3. **GPU Shaders** (GLSL files in `shaders/` directory)
   - `dither.frag`: Implements ordered dithering using Bayer matrix
   - `automaton.frag`: Implements majority rule cellular automaton
   - `copy.frag`: Simple texture copy for initialization and reset
   - `vertex.vert`: Shared vertex shader for full-screen quad rendering

## Components and Interfaces

### Application Controller Interface

```javascript
class App {
  // State
  state: {
    originalImage: ImageData | null,
    paletteSize: number,           // 2-256
    isRunning: boolean,
    animationFrameId: number | null
  }
  
  // Public Methods
  init(): Promise<void>
  handleImageUpload(file: File): Promise<void>
  updatePaletteSize(size: number): void
  applyDither(): void
  startAutomaton(): void
  pauseAutomaton(): void
  resetCanvas(): void
  
  // Private Methods
  loadImage(file: File): Promise<HTMLImageElement>
  initializeCanvas(image: HTMLImageElement): Promise<void>
  animationLoop(): void
  enableControls(): void
  showError(message: string): void
  hideError(): void
  showWarning(message: string): void
  hideWarning(): void
}
```

### WebGL Renderer Interface

```javascript
class WebGLRenderer {
  // WebGL Resources
  gl: WebGLRenderingContext
  canvas: HTMLCanvasElement
  
  // Shader Programs
  ditherProgram: WebGLProgram
  automatonProgram: WebGLProgram
  copyProgram: WebGLProgram
  
  // Textures and Framebuffers (Ping-Pong)
  pingTexture: WebGLTexture
  pongTexture: WebGLTexture
  pingFramebuffer: WebGLFramebuffer
  pongFramebuffer: WebGLFramebuffer
  currentBuffer: 'ping' | 'pong'
  
  // Current working texture
  currentTexture: WebGLTexture
  
  // Public Methods
  constructor(canvas: HTMLCanvasElement)
  initDitherShader(): Promise<void>
  initAutomatonShader(): Promise<void>
  initCopyShader(): Promise<void>
  executeDither(paletteSize: number): void
  executeAutomatonStep(paletteSize: number): void
  setupPingPongBuffers(width: number, height: number): void
  createTexture(source: HTMLImageElement | ImageData): WebGLTexture
  getCanvasImageData(): ImageData
  renderQuad(program: WebGLProgram): void
  
  // Private Methods
  loadShader(url: string): Promise<string>
  compileShader(source: string, type: GLenum): WebGLShader
  createProgram(vertexSource: string, fragmentSource: string): WebGLProgram
  setupQuadBuffer(program: WebGLProgram): void
}
```

### Shader Interfaces

**Dithering Shader** (`shaders/dither.frag`)
```glsl
// Inputs
uniform sampler2D u_image;      // Source image texture
uniform vec2 u_resolution;      // Canvas dimensions
uniform float u_paletteSize;    // Number of colors (2-256)
varying vec2 v_texCoord;        // Texture coordinates from vertex shader

// Output
gl_FragColor                    // Dithered RGB color
```

**Automaton Shader** (`shaders/automaton.frag`)
```glsl
// Inputs
uniform sampler2D u_state;      // Current automaton state texture
uniform vec2 u_resolution;      // Canvas dimensions
uniform float u_paletteSize;    // Number of colors for tie-breaking
uniform float u_random;         // Random seed for tie-breaking
varying vec2 v_texCoord;        // Texture coordinates

// Output
gl_FragColor                    // Next generation RGB color
```

**Copy Shader** (`shaders/copy.frag`)
```glsl
// Inputs
uniform sampler2D u_image;      // Source texture
uniform vec2 u_resolution;      // Canvas dimensions
varying vec2 v_texCoord;        // Texture coordinates

// Output
gl_FragColor                    // Copied RGB color
```

## Data Models

### Application State

```javascript
{
  originalImage: ImageData | null,  // Stored for reset functionality
  paletteSize: number,               // Current palette size (2-256)
  isRunning: boolean,                // Automaton animation state
  animationFrameId: number | null    // For canceling requestAnimationFrame
}
```

### Image Data Flow

1. **Upload**: User uploads image → File → HTMLImageElement
2. **Initialize**: HTMLImageElement → WebGL Texture → Framebuffer
3. **Store Original**: Framebuffer → ImageData (for reset)
4. **Dither**: Input Texture → Dither Shader → Output Texture → Canvas
5. **Automaton**: Current State Texture → Automaton Shader → Next State Texture → Canvas (ping-pong)
6. **Reset**: ImageData → WebGL Texture → Framebuffer → Canvas

### Texture and Framebuffer Management

The renderer uses a **ping-pong buffering** strategy for the automaton:

```
Iteration N:   Read from Ping  → Write to Pong → Display Pong
Iteration N+1: Read from Pong  → Write to Ping → Display Ping
Iteration N+2: Read from Ping  → Write to Pong → Display Pong
...
```

**Design Rationale**: Ping-pong buffering is essential for cellular automata because we need to read the current generation while writing the next generation. Without this, we would have race conditions where some cells see the new state while others see the old state, breaking the automaton rules.

### Color Representation and Palette

**Palette Definition**:
The palette size (2-256) determines how many distinct colors are available after dithering. For example:
- Palette size 2: Black and white only
- Palette size 8: 8 distinct grayscale levels
- Palette size 16+: Distributed across RGB color space

**In Dithering Shader**:
- Input: Original RGB values (0.0-1.0 per channel)
- Processing: 
  - For small palettes (≤8): Convert to grayscale, quantize to N levels using Bayer matrix
  - For larger palettes: Quantize each RGB channel independently to create color palette
- Output: Quantized RGB values representing one of N palette colors

**Palette Generation Strategy**:
- **Grayscale mode** (palette ≤ 8): Evenly spaced gray levels from black to white
- **Color mode** (palette > 8): Per-channel quantization where each channel gets ⌈∛N⌉ levels
  - Example: 16 colors → ~2.5 levels per channel → 3×3×3 = 27 possible colors, use closest 16
  - Example: 64 colors → 4 levels per channel → 4×4×4 = 64 colors exactly

**In Automaton Shader**:
- Input: Quantized RGB values from dithered image (limited to palette colors)
- Processing: 
  - Read 8 neighbor colors (RGB values)
  - Count occurrences of each unique RGB color
  - Find the most common color(s)
  - If tie, randomly select from tied colors
- Output: RGB color of majority neighbor (guaranteed to be a palette color)

**Design Rationale**: The system operates on RGB values throughout rather than palette indices. This simplifies the implementation because:
1. No need to maintain a separate palette array or lookup table
2. Shaders can directly compare RGB values for equality
3. The dithering shader naturally produces only palette colors through quantization
4. The automaton preserves palette colors since it only outputs colors from its neighbors

The trade-off is that we don't explicitly track which palette colors are "valid" - instead, the dithering process naturally constrains colors to the palette through quantization, and the automaton can only produce colors that already exist in the image.

## Correctness Properties

*A property is a characteristic or behavior that should hold true across all valid executions of a system—essentially, a formal statement about what the system should do. Properties serve as the bridge between human-readable specifications and machine-verifiable correctness guarantees.*

### Property 1: Image Format Validation
*For any* uploaded file, if the file type is not in the set {PNG, JPG, JPEG, GIF, WebP}, the system should reject the upload and display an error message without modifying the canvas.

**Validates: Requirements 1.1, 1.3**

### Property 2: Canvas Dimension Matching
*For any* uploaded image with dimensions W×H, the canvas dimensions should be set to exactly W×H pixels, ensuring pixel-perfect rendering.

**Validates: Requirements 1.2, 6.1, 6.2**

### Property 3: Palette Size Bounds
*For any* palette size input value V, the system should clamp V to the range [2, 256] before applying dithering.

**Validates: Requirements 2.3**

### Property 4: Dithering Produces Palette Colors
*For any* dithered image with palette size N, every pixel should have an RGB value that corresponds to one of the N quantized palette colors.

**Validates: Requirements 3.5, 3.6**

### Property 5: Control State Consistency
*For any* application state, if no image has been uploaded, all controls (palette slider, dither button, automaton buttons) should remain disabled.

**Validates: Requirements 1.4**

### Property 6: Control State After Dithering
*For any* application state after dithering has been applied, the automaton controls (start, pause, reset) should be enabled.

**Validates: Requirements 3.7, 4.2**

### Property 7: Dithering Idempotence
*For any* dithered image, applying the same dithering operation again with the same palette size should produce a visually identical result (allowing for minor floating-point differences).

**Validates: Requirements 3.2**

### Property 8: Moore Neighborhood Completeness
*For any* pixel at position (x, y) that is not on the image boundary, the automaton should consider exactly 8 neighbors: (x±1, y±1), (x±1, y), (x, y±1).

**Validates: Requirements 4.6**

### Property 9: Majority Rule Correctness
*For any* pixel in the automaton, if there exists a unique color C that appears most frequently among the 8 neighbors, the pixel should become color C in the next generation.

**Validates: Requirements 4.5, 4.7**

### Property 10: Automaton Preserves Palette
*For any* automaton generation, all pixel colors should remain within the set of palette colors from the dithered image (no new colors introduced).

**Validates: Requirements 4.8**

### Property 11: Tie-Breaking Consistency
*For any* pixel where multiple colors tie for the majority, the system should select one of the tied colors using a deterministic random selection based on pixel position and frame number.

**Validates: Requirements 4.9**

### Property 12: Reset Restoration
*For any* canvas state after dithering or automaton execution, clicking reset should restore the canvas to display the exact original uploaded image.

**Validates: Requirements 4.11**

### Property 13: WebGL Availability Check
*For any* browser environment, if WebGL is not available, the system should display an error message and prevent further initialization.

**Validates: Requirements 5.6**

### Property 14: Performance Threshold
*For any* image with dimensions up to 1024×1024 pixels, the automaton should maintain at least 30 FPS during continuous execution.

**Validates: Requirements 5.7, 8.2**

### Property 15: Large Image Warning
*For any* uploaded image with width or height exceeding 2048 pixels, the system should display a warning message about potential performance impact.

**Validates: Requirements 8.3**

### Property 16: Animation State Consistency
*For any* automaton execution, if the pause button is clicked, the animation should stop within one frame, and clicking start should resume from the current state.

**Validates: Requirements 4.1**

### Property 17: Shader File Separation
*For any* shader program (dithering, automaton, copy), the GLSL source code should be loaded from a separate `.frag` or `.vert` file, not embedded in JavaScript strings.

**Validates: Requirements 3.4, 5.2, 5.4, 9.3**

### Property 18: No External Dependencies
*For any* file in the project, there should be no import statements or script tags referencing external libraries or frameworks (React, Vue, jQuery, etc.).

**Validates: Requirements 9.1, 9.4**

## Error Handling

### Error Categories

1. **User Input Errors**
   - Invalid file format → Display error message, reject upload
   - Image too large (>4096×4096) → Display error message, reject upload
   - Invalid palette size → Clamp to valid range [2, 256]

2. **WebGL Errors**
   - WebGL not supported → Display error message, disable all functionality
   - Shader compilation failure → Log to console, display error message
   - Texture creation failure → Display error message, prevent further operations

3. **Runtime Errors**
   - Animation loop error → Pause automaton, display error message
   - File read error → Display error message, reject upload

### Error Display Strategy

- **Error Messages**: Red background, displayed prominently above canvas
- **Warning Messages**: Yellow background, for non-critical issues (large images)
- **Console Logging**: All errors logged to console for debugging
- **Graceful Degradation**: Errors should not crash the application; disable affected features only

### Error Recovery

- **Upload Errors**: User can retry with a different file
- **Dithering Errors**: User can adjust palette size and retry
- **Automaton Errors**: Pause animation, allow reset to recover
- **WebGL Errors**: No recovery possible; display error and disable app

## Testing Strategy

### Dual Testing Approach

The application will use both **unit tests** and **property-based tests** to ensure comprehensive coverage:

- **Unit tests**: Verify specific examples, edge cases, and error conditions
- **Property tests**: Verify universal properties across all inputs

Both types of tests are complementary and necessary for comprehensive coverage. Unit tests catch concrete bugs in specific scenarios, while property tests verify general correctness across many inputs.

### Unit Testing

**Focus Areas**:
- Image format validation with specific file types
- Palette size clamping with boundary values (1, 2, 256, 257)
- Control state transitions (disabled → enabled → disabled)
- Error message display and hiding
- Canvas dimension setting with specific image sizes
- WebGL context initialization and error handling

**Example Unit Tests**:
```javascript
// Test image format validation
test('rejects invalid image format', () => {
  const file = new File([''], 'test.txt', { type: 'text/plain' });
  app.handleImageUpload(file);
  expect(errorMessage.textContent).toContain('Invalid file type');
});

// Test palette size clamping
test('clamps palette size to minimum', () => {
  app.updatePaletteSize(1);
  expect(app.state.paletteSize).toBe(2);
});

// Test canvas dimensions
test('sets canvas dimensions to match image', () => {
  const image = createTestImage(800, 600);
  app.initializeCanvas(image);
  expect(canvas.width).toBe(800);
  expect(canvas.height).toBe(600);
});
```

### Property-Based Testing

**Testing Framework**: Use **fast-check** for JavaScript property-based testing.

**Configuration**: Each property test should run a minimum of 100 iterations to ensure comprehensive input coverage.

**Property Test Implementation**:

Each correctness property from the design document should be implemented as a property-based test with the following tag format:

```javascript
// Feature: image-majority-automaton, Property 1: Image Format Validation
fc.assert(
  fc.property(fc.string(), fc.string(), (filename, mimeType) => {
    // Test implementation
  }),
  { numRuns: 100 }
);
```

**Focus Areas**:
- Image format validation across all possible MIME types
- Palette size handling across full range [2, 256] and beyond
- Canvas dimension matching for various image sizes
- Dithering consistency across multiple applications
- Automaton majority rule with random neighbor configurations
- Reset functionality preserving original image data
- Performance characteristics with varying image sizes

**Example Property Tests**:
```javascript
// Property 1: Image Format Validation
fc.assert(
  fc.property(
    fc.constantFrom('image/png', 'image/jpeg', 'image/gif', 'image/webp', 'text/plain', 'application/pdf'),
    (mimeType) => {
      const validTypes = ['image/png', 'image/jpeg', 'image/gif', 'image/webp'];
      const file = new File([''], 'test', { type: mimeType });
      const result = app.isValidImageType(file);
      return result === validTypes.includes(mimeType);
    }
  ),
  { numRuns: 100 }
);

// Property 3: Palette Size Bounds
fc.assert(
  fc.property(
    fc.integer({ min: -100, max: 500 }),
    (inputSize) => {
      app.updatePaletteSize(inputSize);
      return app.state.paletteSize >= 2 && app.state.paletteSize <= 256;
    }
  ),
  { numRuns: 100 }
);

// Property 7: Majority Rule Correctness
fc.assert(
  fc.property(
    fc.array(fc.record({ r: fc.float(), g: fc.float(), b: fc.float() }), { minLength: 8, maxLength: 8 }),
    (neighbors) => {
      const colorCounts = countColors(neighbors);
      const maxCount = Math.max(...Object.values(colorCounts));
      const majorityColors = Object.keys(colorCounts).filter(c => colorCounts[c] === maxCount);
      
      const result = calculateMajority(neighbors);
      return majorityColors.some(c => colorsEqual(result, parseColor(c)));
    }
  ),
  { numRuns: 100 }
);
```

### Integration Testing

**Manual Testing Checklist**:
1. Upload various image formats (PNG, JPG, GIF, WebP)
2. Test with images of different sizes (small, medium, large, very large)
3. Apply dithering with different palette sizes (2, 8, 16, 64, 256)
4. Run automaton and verify visual evolution
5. Test pause/resume functionality
6. Test reset functionality
7. Verify error messages for invalid inputs
8. Test on different browsers (Chrome, Firefox, Safari, Edge)
9. Verify performance with large images (measure FPS)
10. Test with WebGL disabled (error handling)

### Performance Testing

**Metrics to Measure**:
- Frame rate (FPS) during automaton execution
- Time to apply dithering for various image sizes
- Memory usage during operation
- Shader compilation time

**Performance Targets**:
- 30+ FPS for images up to 1024×1024
- Dithering should complete in <500ms for 1024×1024 images
- No memory leaks during extended automaton execution
- Shader compilation should complete in <100ms

### Testing Tools

- **Unit Testing**: Jest or Mocha
- **Property-Based Testing**: fast-check
- **Performance Testing**: Chrome DevTools Performance profiler
- **Visual Testing**: Manual inspection and screenshots
- **Browser Testing**: BrowserStack or manual testing on multiple browsers

