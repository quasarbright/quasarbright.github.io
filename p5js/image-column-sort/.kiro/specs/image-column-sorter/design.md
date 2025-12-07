# Design Document

## Overview

The Image Column Sorter is a browser-based web application that visualizes sorting algorithms by treating image columns as sortable elements. The application uses HTML5 Canvas for rendering, Web Audio API for sound generation, and implements multiple sorting algorithms with step-by-step visualization capabilities.

The core concept involves:
1. Loading an image and extracting it as individual vertical columns
2. Scrambling these columns randomly while maintaining a reference to their original positions
3. Applying sorting algorithms that compare columns based on original positions
4. Animating each comparison and swap operation with visual and audio feedback
5. Providing user controls for algorithm selection, speed adjustment, and playback control

## Architecture

The application follows a component-based architecture with clear separation of concerns:

```
┌─────────────────────────────────────────────────────────────┐
│                         UI Layer                             │
│  (Controls, Canvas Rendering, Event Handlers)               │
└─────────────────┬───────────────────────────────────────────┘
                  │
┌─────────────────▼───────────────────────────────────────────┐
│                    Application State                         │
│  (Image Data, Column Array, Visualization State)            │
└─────────────────┬───────────────────────────────────────────┘
                  │
┌─────────────────▼───────────────────────────────────────────┐
│                   Core Services                              │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐     │
│  │   Image      │  │   Sorting    │  │    Audio     │     │
│  │  Processor   │  │  Algorithms  │  │  Generator   │     │
│  └──────────────┘  └──────────────┘  └──────────────┘     │
└─────────────────────────────────────────────────────────────┘
```

### Technology Stack

- **Frontend Framework**: Vanilla JavaScript (ES6+ modules)
- **Rendering**: HTML5 Canvas API
- **Audio**: Web Audio API
- **Build Tool**: None - pure HTML/JS served directly
- **Language**: JavaScript with JSDoc comments for type documentation
- **Module System**: ES6 modules (type="module" in script tags)

## Components and Interfaces

### ImageProcessor

Handles image loading, column extraction, and efficient canvas rendering.

**Performance Optimization**: Instead of storing individual ImageData objects per column (memory intensive), we store the original image and render columns by copying from source positions. This approach:
- **Memory Efficient**: Single source image instead of width × ImageData objects
- **Fast Rendering**: Direct pixel copying using `drawImage()` with source rectangles
- **Scalable**: Handles large images (1000+ columns) smoothly

```typescript
interface ImageProcessor {
  loadImage(file: File): Promise<HTMLImageElement>;
  scaleImage(image: HTMLImageElement, maxWidth: number, maxHeight: number): HTMLImageElement;
  renderAllColumns(canvas: HTMLCanvasElement, sourceImage: HTMLImageElement, columnOrder: number[]): void;
  renderColumnsAt(canvas: HTMLCanvasElement, sourceImage: HTMLImageElement, columnOrder: number[], positions: number[]): void;
  // columnOrder is array like [5, 2, 8, 1, ...] indicating which source column to draw at each position
  // positions is array of display positions to redraw (for lazy rendering during swaps)
}
```

### SortingAlgorithm

Defines the interface for sorting algorithms and implements specific algorithms.

**Key Design Decision**: Sorting operates purely on arrays of numbers (indices), completely independent of image data. This separation provides:
- **Performance**: Sorting logic doesn't touch heavy image data
- **Testability**: Easy to test with simple number arrays
- **Reusability**: Same sorting code works for any visualization

Sorting algorithms work on a simple number array representing the scrambled order. They log each comparison and swap operation as a step:

1. **Pure Number Sorting**: Algorithms sort arrays of numbers (0 to n-1)
2. **Step Logging**: Every comparison and swap is logged to a steps array as it occurs
3. **Comparison Tracking**: Every comparison between elements must be recorded as a step
4. **Swap Recording**: Every swap operation must be recorded with the indices involved
5. **Careful Ordering**: Implementations must be careful to log all steps in the exact order they occur

**Algorithm Support**: This design works for algorithms that can be implemented purely in terms of comparisons and swaps on a single array:
- **Bubble Sort, Insertion Sort, Selection Sort**: Naturally in-place, straightforward to implement
- **Quick Sort**: In-place partitioning with recursive calls, logs steps during partition operations
- **Merge Sort**: Requires special in-place merge implementation. Instead of merging into an auxiliary array, the in-place merge rotates/shifts elements into position, generating many swap operations. This is more complex than the standard merge sort but produces excellent visualizations showing the divide-and-conquer pattern.

The key challenge for merge sort is implementing the merge step in-place by finding correct positions and rotating elements, rather than copying to a temporary array. This generates the swap operations needed for visualization.

```typescript
type SortingStep = 
  | { type: 'compare'; indices: [number, number] }  // Compare two positions
  | { type: 'swap'; indices: [number, number] }     // Swap two positions
  | { type: 'complete' };                            // Sorting finished

interface SortingAlgorithm {
  name: string;
  description: string;
  sort(arr: number[]): SortingStep[];  // Sorts array of numbers in-place, returns steps
}

// Initial implementations: BubbleSort, QuickSort, InsertionSort, SelectionSort, MergeSort (in-place)
```

### VisualizationEngine

Manages the animation loop and coordinates rendering with audio.

```typescript
interface VisualizationEngine {
  start(steps: SortingStep[], columns: ImageColumn[]): void;
  pause(): void;
  resume(): void;
  reset(): void;
  setSpeed(speed: number): void;  // Speed multiplier
  onStepComplete: (step: SortingStep) => void;
}

interface VisualizationState {
  isPlaying: boolean;
  isPaused: boolean;
  currentStepIndex: number;
  speed: number;  // Delay between steps in milliseconds
}
```

### AudioGenerator

Generates tones based on column positions during sorting operations.

```typescript
interface AudioGenerator {
  initialize(): void;
  playTone(frequency: number, duration: number): void;
  setEnabled(enabled: boolean): void;
  calculateFrequency(columnIndex: number, totalColumns: number): number;
}
```

### HighlightManager

Manages visual highlights for columns during operations.

```typescript
interface HighlightManager {
  setComparisonHighlight(indices: number[]): void;  // Green highlight
  setOperationHighlight(indices: number[]): void;   // Red highlight
  clearHighlights(): void;
  renderHighlights(canvas: HTMLCanvasElement, columns: ImageColumn[]): void;
}

interface Highlight {
  indices: number[];
  color: string;  // rgba color with transparency
  type: 'comparison' | 'operation';
}
```

### ApplicationController

Coordinates all components and manages application state.

```typescript
interface ApplicationController {
  loadImage(file: File): Promise<void>;
  scrambleColumns(): void;
  selectAlgorithm(algorithmName: string): void;
  startSorting(): void;
  pauseSorting(): void;
  resumeSorting(): void;
  resetSorting(): void;
  setSpeed(speed: number): void;
  toggleAudio(): void;
}
```

## Data Models

### Column Order Array

Instead of storing individual column objects, we use a simple number array to represent column ordering:

```typescript
// columnOrder[displayPosition] = sourceColumnIndex
// Example: [5, 2, 8, 1, ...] means:
//   - Display position 0 shows source column 5
//   - Display position 1 shows source column 2
//   - Display position 2 shows source column 8
//   - etc.

type ColumnOrder = number[];  // Array of indices into source image columns
```

This approach is:
- **Memory efficient**: Just an array of integers
- **Fast to manipulate**: Swapping is just array element swaps
- **Easy to reason about**: Clear mapping from display to source

### SortingStep

Represents a single operation in the sorting algorithm as a discriminated union.

```typescript
type SortingStep = 
  | { type: 'compare'; indices: [number, number] }  // Compare two columns at given indices
  | { type: 'swap'; indices: [number, number] }     // Swap two columns at given indices
  | { type: 'complete' };                            // Sorting has finished
```

### ApplicationState

Central state management for the application.

```typescript
interface ApplicationState {
  sourceImage: HTMLImageElement | null;  // Original loaded image
  imageWidth: number;                     // Number of columns in image
  columnOrder: number[];                  // Current display order [displayPos] = sourceCol
  scrambledOrder: number[];               // Saved scrambled state for reset
  selectedAlgorithm: SortingAlgorithm | null;
  sortingSteps: SortingStep[];
  visualizationState: VisualizationState;
  audioEnabled: boolean;
  highlights: Highlight[];
}
```

## Correctness Properties

*A property is a characteristic or behavior that should hold true across all valid executions of a system—essentially, a formal statement about what the system should do. Properties serve as the bridge between human-readable specifications and machine-verifiable correctness guarantees.*


### Property 1: Image format support
*For any* valid image file in PNG, JPEG, or GIF format, the image processor should successfully load and extract columns from the image.
**Validates: Requirements 1.2**

### Property 2: Aspect ratio preservation during scaling
*For any* image that exceeds display dimensions, scaling the image should preserve the original aspect ratio within a reasonable tolerance.
**Validates: Requirements 1.3**

### Property 3: Invalid file rejection
*For any* file that is not a valid image format, the system should reject the file and display an error without crashing.
**Validates: Requirements 1.4**

### Property 4: Scrambling produces permutation
*For any* array of image columns, scrambling should produce a permutation where all original columns are present exactly once.
**Validates: Requirements 2.1**

### Property 5: Column integrity during scramble
*For any* image column, scrambling should not modify the pixel data of individual columns, only their positions in the array.
**Validates: Requirements 2.2**

### Property 6: Original index preservation
*For any* scrambled column array, each column should retain its originalIndex field unchanged from before scrambling.
**Validates: Requirements 2.3**

### Property 7: Re-scrambling produces different order
*For any* already-scrambled column array, re-scrambling should produce a different permutation with high probability (>99% for arrays with more than 3 elements).
**Validates: Requirements 2.5**

### Property 8: Algorithm step generation correctness
*For any* sorting algorithm and column array, the generated steps should correctly sort the array when executed, with comparisons based on originalIndex.
**Validates: Requirements 4.1**

### Property 9: Sorting restores original image
*For any* scrambled column array, applying the sorting algorithm steps should restore the columns to their original order (sorted by originalIndex), recreating the exact original image.
**Validates: Requirements 4.5, 9.3, 9.4**

### Property 10: Pause preserves state
*For any* visualization state, pausing should not modify the current column arrangement, step index, or highlight states.
**Validates: Requirements 6.2**

### Property 11: Reset restores scrambled state
*For any* sorting visualization in progress or completed, resetting should restore the exact scrambled state that existed before sorting began.
**Validates: Requirements 7.1**

### Property 12: Audio frequency generation
*For any* column comparison during sorting with audio enabled, the generated tone frequency should correspond to the column positions being compared, with higher positions producing higher frequencies.
**Validates: Requirements 8.3**

### Property 13: Comparison consistency and correctness
*For any* pair of columns, the comparison function should always return the same result based on their originalIndex values, and should satisfy the properties of a total order (reflexive, antisymmetric, transitive).
**Validates: Requirements 9.1, 9.2**

## Error Handling

### Image Loading Errors
- Invalid file formats: Display user-friendly error message, prevent further processing
- Corrupted image files: Catch decoding errors, display error message
- File too large: Implement size limits, display warning before processing
- Network errors (if loading from URL): Retry logic with timeout

### Runtime Errors
- Canvas rendering failures: Fallback to error state, allow user to retry
- Audio context errors: Gracefully disable audio, continue with visual-only mode
- Memory constraints: Limit maximum image dimensions, warn user before processing large images

### State Management Errors
- Invalid state transitions: Validate state before operations, prevent invalid actions
- Race conditions: Use proper async handling, prevent concurrent sorting operations
- Lost references: Maintain immutable original state, allow recovery

## Testing Strategy

The application will use a dual testing approach combining unit tests and property-based tests to ensure correctness.

### Unit Testing

Unit tests will cover:
- Specific examples of image loading with known test images
- UI state transitions (button enable/disable states)
- Specific algorithm step generation for small, known inputs
- Audio toggle functionality
- Pause/resume/reset controls
- Highlight rendering for specific scenarios

Test framework: Simple HTML test runner with assertions, or browser-based test framework

### Property-Based Testing

Property-based tests will verify universal properties across many randomly generated inputs:

**Framework**: fast-check (JavaScript property-based testing library, loaded via CDN or npm for testing only)

**Configuration**: Each property test should run a minimum of 100 iterations to ensure thorough coverage of the input space.

**Test Tagging**: Each property-based test must include a comment tag in this exact format:
```javascript
// **Feature: image-column-sorter, Property {number}: {property_text}**
```

**Key Properties to Test**:

1. **Image format support** - Generate various valid image formats and verify loading
2. **Aspect ratio preservation** - Generate random dimensions and verify scaling maintains ratio
3. **Invalid file rejection** - Generate invalid file types and verify error handling
4. **Scrambling produces permutation** - Generate random column arrays, verify scrambling preserves all elements
5. **Column integrity** - Generate random columns, verify pixel data unchanged after scramble
6. **Original index preservation** - Verify originalIndex fields remain constant through scrambling
7. **Re-scrambling difference** - Verify multiple scrambles produce different results
8. **Algorithm correctness** - Generate random scrambled arrays, verify sorting restores order
9. **Sorting restores original** - The ultimate correctness test - verify sorted result matches original
10. **Pause preserves state** - Verify pausing doesn't modify state
11. **Reset restores scrambled** - Verify reset returns to pre-sort state
12. **Audio frequency mapping** - Verify frequency calculation is monotonic with position
13. **Comparison consistency** - Verify comparison function satisfies total order properties

**Generator Strategy**:
- Create smart generators for ImageColumn arrays with realistic dimensions
- Generate edge cases: empty arrays, single column, very wide images
- Generate various scrambled states to test sorting algorithms
- Use shrinking to find minimal failing cases when tests fail

### Integration Testing

- End-to-end flow: Load image → Scramble → Sort → Verify restoration
- Multiple algorithm comparison: Verify all algorithms produce same final result
- Speed control during active sorting
- Audio enable/disable during active sorting

## Implementation Notes

### File Structure

The application will be organized as follows:
```
index.html              # Main HTML file
js/
  types.js             # JSDoc type definitions and data structures
  imageProcessor.js    # Image loading and column extraction
  sortingAlgorithms.js # All sorting algorithm implementations
  visualizationEngine.js # Animation and step execution
  audioGenerator.js    # Web Audio API integration
  highlightManager.js  # Visual highlight management
  app.js              # Main application controller
tests/
  test.html           # Test runner HTML
  tests.js            # Test suite
```

### Performance Considerations

1. **Efficient Column Rendering**: Use `ctx.drawImage(img, sx, sy, sw, sh, dx, dy, dw, dh)` to copy individual columns from source image. This is much faster than storing/manipulating individual ImageData objects.
2. **Lazy Rendering During Sorting**: Only redraw the columns affected by each swap (typically 2 columns), not the entire canvas. This makes sorting large images (1000+ columns) extremely smooth.
3. **Full Rendering on Load/Scramble**: Render all columns when initially loading or scrambling the image.
4. **Memory Optimization**: Store only the source image and a number array for column order, not individual column pixel data. For a 1000px wide image, this saves ~1000 ImageData objects.
5. **Canvas Rendering**: Use requestAnimationFrame for smooth animations
6. **Large Images**: Can handle images up to 2000+ columns efficiently with the optimized rendering approach
7. **Audio Generation**: Reuse AudioContext and oscillator nodes to avoid memory leaks
8. **Step Generation**: Pre-generate all sorting steps before visualization to avoid computation during animation
9. **ES6 Modules**: Use native ES6 module imports for clean code organization without build step

**Rendering Implementation**:
```javascript
// Full render (used on load and scramble)
function renderAllColumns(canvas, sourceImage, columnOrder) {
  const ctx = canvas.getContext('2d');
  const colWidth = sourceImage.width / columnOrder.length;
  
  for (let displayPos = 0; displayPos < columnOrder.length; displayPos++) {
    const sourceCol = columnOrder[displayPos];
    ctx.drawImage(
      sourceImage,
      sourceCol * colWidth, 0, colWidth, sourceImage.height,  // source rect
      displayPos * colWidth, 0, colWidth, sourceImage.height  // dest rect
    );
  }
}

// Lazy render (used during sorting - only redraws affected columns)
function renderColumnsAt(canvas, sourceImage, columnOrder, positions) {
  const ctx = canvas.getContext('2d');
  const colWidth = sourceImage.width / columnOrder.length;
  
  for (const displayPos of positions) {
    const sourceCol = columnOrder[displayPos];
    ctx.drawImage(
      sourceImage,
      sourceCol * colWidth, 0, colWidth, sourceImage.height,  // source rect
      displayPos * colWidth, 0, colWidth, sourceImage.height  // dest rect
    );
  }
}

// During swap visualization: only redraw the two swapped columns
// This makes sorting 1000+ column images smooth
```

### Browser Compatibility

- Target modern browsers with Canvas, Web Audio API, and ES6 module support
- Provide fallback messaging for unsupported browsers
- Test on Chrome, Firefox, Safari, Edge
- Requires browser support for `<script type="module">`

### Accessibility

- Provide keyboard controls for all operations
- Include ARIA labels for screen readers
- Ensure color highlights have sufficient contrast
- Provide text descriptions of algorithm progress for screen readers

### Future Enhancements

- Additional sorting algorithms (Heap Sort, Radix Sort)
- Support for algorithms with auxiliary arrays (Merge Sort) by adding step types for copy/merge operations
- Step-by-step mode with manual advance
- Algorithm comparison mode (split screen)
- Export sorted animation as video
- Custom color schemes for highlights
- Statistics display (comparisons, swaps, time complexity)
