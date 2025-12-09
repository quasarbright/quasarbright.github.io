# Implementation Plan

- [x] 1. Set up project structure
  - Create index.html with canvas element and UI controls
  - Create js/ directory for ES6 modules
  - Create tests/ directory for test files
  - Set up basic HTML structure with script type="module"
  - _Requirements: All_

- [x] 2. Implement core data models and types
  - [x] 2.1 Create types.js with JSDoc type definitions
    - Define JSDoc types for SortingStep, ApplicationState, ColumnOrder
    - Document discriminated union pattern for SortingStep type
    - Document that sorting operates on pure number arrays
    - Export utility functions for creating/manipulating column orders
    - _Requirements: 9.1, 9.2_
  
  - [x] 2.2 Write property test for comparison consistency
    - **Property 13: Comparison consistency and correctness**
    - **Validates: Requirements 9.1, 9.2**

- [x] 3. Implement ImageProcessor module
  - [x] 3.1 Create js/imageProcessor.js with image loading functionality
    - Implement file input handling
    - Load image from File object
    - Handle image format validation (PNG, JPEG, GIF)
    - Export as ES6 module
    - _Requirements: 1.1, 1.2, 1.4_
  
  - [x] 3.2 Implement image scaling
    - Scale oversized images to fit display constraints
    - Maintain aspect ratio during scaling
    - _Requirements: 1.3_
  
  - [x] 3.3 Implement efficient column rendering
    - Implement renderAllColumns() for full canvas render (load/scramble)
    - Implement renderColumnsAt() for lazy partial render (swap operations)
    - Use ctx.drawImage() with source rectangles to copy columns
    - Take sourceImage and columnOrder array as inputs
    - Optimize for performance with large images
    - _Requirements: 4.1, 4.5_
  
  - [x] 3.4 Write property tests for ImageProcessor
    - **Property 1: Image format support**
    - **Validates: Requirements 1.2**
  
  - [x] 3.5 Write property test for aspect ratio preservation
    - **Property 2: Aspect ratio preservation during scaling**
    - **Validates: Requirements 1.3**
  
  - [x] 3.6 Write property test for invalid file rejection
    - **Property 3: Invalid file rejection**
    - **Validates: Requirements 1.4**

- [x] 4. Implement column order scrambling functionality
  - [x] 4.1 Add scramble function to types.js or separate utils.js
    - Implement Fisher-Yates shuffle algorithm on number array
    - Takes array length, returns shuffled array [0, 1, 2, ...n] in random order
    - Pure function, no image data involved
    - _Requirements: 2.1, 2.2, 2.3_
  
  - [x] 4.2 Write property test for scrambling produces permutation
    - **Property 4: Scrambling produces permutation**
    - **Validates: Requirements 2.1**
  
  - [x] 4.3 Write property test for column integrity
    - **Property 5: Column integrity during scramble**
    - **Validates: Requirements 2.2**
  
  - [x] 4.4 Write property test for original index preservation
    - **Property 6: Original index preservation**
    - **Validates: Requirements 2.3**
  
  - [x] 4.5 Write property test for re-scrambling
    - **Property 7: Re-scrambling produces different order**
    - **Validates: Requirements 2.5**

- [x] 5. Implement sorting algorithms with step logging
  - [x] 5.1 Create js/sortingAlgorithms.js with base comparison function
    - Algorithms work on pure number arrays (no image data)
    - Standard numeric comparison (a < b)
    - Ensure consistent ordering
    - Export as ES6 module
    - _Requirements: 9.1, 9.2_
  
  - [x] 5.2 Implement Bubble Sort in sortingAlgorithms.js
    - Sort number array in-place with step logging
    - Log all comparisons and swaps
    - Export algorithm object with name, description, sort function
    - _Requirements: 3.1, 4.1, 9.3, 9.4_
  
  - [x] 5.3 Implement Selection Sort in sortingAlgorithms.js
    - Sort number array in-place with step logging
    - Log all comparisons and swaps
    - Export algorithm object
    - _Requirements: 3.1, 4.1, 9.3, 9.4_
  
  - [x] 5.4 Implement Insertion Sort in sortingAlgorithms.js
    - Sort number array in-place with step logging
    - Log all comparisons and swaps
    - Export algorithm object
    - _Requirements: 3.1, 4.1, 9.3, 9.4_
  
  - [x] 5.5 Implement Quick Sort in sortingAlgorithms.js
    - Sort number array in-place with step logging
    - Log all comparisons and swaps during partitioning
    - Handle recursive calls properly
    - Export algorithm object
    - _Requirements: 3.1, 4.1, 9.3, 9.4_
  
  - [x] 5.6 Implement Merge Sort with in-place merge in sortingAlgorithms.js
    - Sort number array in-place with step logging
    - Implement in-place merge using rotation/shifting
    - Log all comparisons and swaps
    - Carefully maintain step order during recursive calls
    - Export algorithm object
    - _Requirements: 3.1, 4.1, 9.3, 9.4_
  
  - [x] 5.7 Write property test for algorithm correctness
    - **Property 8: Algorithm step generation correctness**
    - **Validates: Requirements 4.1**
  
  - [x] 5.8 Write property test for sorting restores original
    - **Property 9: Sorting restores original image**
    - **Validates: Requirements 4.5, 9.3, 9.4**

- [ ] 6. Checkpoint - Ensure all tests pass
  - Ensure all tests pass, ask the user if questions arise.

- [x] 7. Implement HighlightManager
  - [x] 7.1 Create js/highlightManager.js with highlight state management
    - Track comparison highlights (green)
    - Track operation highlights (red)
    - Implement highlight clearing
    - Export as ES6 module
    - _Requirements: 4.2, 4.3_
  
  - [x] 7.2 Implement highlight rendering in highlightManager.js
    - Render semi-transparent overlays on canvas
    - Apply correct colors for different highlight types
    - _Requirements: 4.2, 4.3, 4.5_

- [x] 8. Implement AudioGenerator
  - [x] 8.1 Create js/audioGenerator.js with Web Audio API initialization
    - Create AudioContext
    - Set up oscillator nodes
    - Handle browser autoplay policies
    - Export as ES6 module
    - _Requirements: 8.1, 8.2_
  
  - [x] 8.2 Implement tone generation in audioGenerator.js
    - Calculate frequency based on column position
    - Play tones with appropriate duration
    - Map column indices to frequency range
    - _Requirements: 8.3_
  
  - [x] 8.3 Implement audio enable/disable in audioGenerator.js
    - Toggle audio on/off
    - Stop sounds immediately when disabled
    - _Requirements: 8.2, 8.5_
  
  - [x] 8.4 Write property test for audio frequency generation
    - **Property 12: Audio frequency generation**
    - **Validates: Requirements 8.3**

- [x] 9. Implement VisualizationEngine
  - [x] 9.1 Create js/visualizationEngine.js with animation loop
    - Use requestAnimationFrame for smooth rendering
    - Process sorting steps sequentially
    - Apply speed multiplier to control pace
    - Export as ES6 module
    - _Requirements: 4.1, 4.6, 5.1_
  
  - [x] 9.2 Implement step execution in visualizationEngine.js
    - Execute compare steps (update highlights, play audio)
    - Execute swap steps (swap elements in columnOrder array, update highlights, play audio)
    - Execute complete step (clear highlights)
    - Use imageProcessor.renderColumnsAt() to lazily redraw only the swapped columns
    - Coordinate with AudioGenerator for sound
    - _Requirements: 4.1, 4.2, 4.3, 4.4, 4.5, 8.3_
  
  - [x] 9.3 Implement pause/resume functionality in visualizationEngine.js
    - Pause animation at current step
    - Resume from paused state
    - Maintain state during pause
    - _Requirements: 6.1, 6.2, 6.3, 6.4_
  
  - [x] 9.4 Implement reset functionality in visualizationEngine.js
    - Stop ongoing animation
    - Restore scrambled state
    - Clear highlights
    - _Requirements: 7.1, 7.2, 7.3, 7.4_
  
  - [x] 9.5 Implement speed control in visualizationEngine.js
    - Adjust animation speed dynamically
    - Apply speed changes immediately
    - _Requirements: 5.1, 5.4_
  
  - [x] 9.6 Write property test for pause preserves state
    - **Property 10: Pause preserves state**
    - **Validates: Requirements 6.2**
  
  - [x] 9.7 Write property test for reset restores scrambled state
    - **Property 11: Reset restores scrambled state**
    - **Validates: Requirements 7.1**

- [x] 10. Implement ApplicationController and UI
  - [x] 10.1 Add UI controls to index.html
    - File upload button
    - Scramble button
    - Algorithm selection dropdown
    - Start/pause/resume button
    - Reset button
    - Speed slider
    - Audio toggle button
    - _Requirements: 1.1, 2.1, 3.1, 3.2, 4.1, 5.1, 6.1, 6.3, 7.1, 8.2_
  
  - [x] 10.2 Create js/app.js with ApplicationController
    - Import all modules
    - Coordinate all modules
    - Manage application state
    - Handle UI events
    - Update UI based on state changes
    - _Requirements: All_
  
  - [x] 10.3 Wire up event handlers in app.js
    - Connect UI controls to controller methods
    - Handle state transitions
    - Enable/disable controls based on state
    - _Requirements: 1.5, 2.4, 3.3, 6.4, 7.3_
  
  - [x] 10.4 Write unit tests for UI state transitions
    - Test button enable/disable logic
    - Test state transitions
    - Test control interactions

- [x] 11. Implement error handling
  - [x] 11.1 Add error handling for image loading
    - Handle invalid file formats
    - Handle corrupted images
    - Display user-friendly error messages
    - _Requirements: 1.4_
  
  - [x] 11.2 Add error handling for runtime errors
    - Handle canvas rendering failures
    - Handle audio context errors
    - Gracefully degrade functionality
    - _Requirements: All_

- [x] 12. Polish and styling
  - [x] 12.1 Style the UI
    - Create clean, modern interface
    - Style controls and canvas
    - Add responsive layout
    - Ensure sufficient contrast for accessibility
    - _Requirements: All_
  
  - [x] 12.2 Add algorithm descriptions
    - Display information about selected algorithm
    - Show algorithm characteristics
    - _Requirements: 3.4_

- [x] 13. Implement endless background mode
  - [x] 13.1 Create endless-config.js with algorithm configurations
    - Define ENDLESS_MODE_CONFIG object with columnCount and algorithms array
    - Include algorithm names and their corresponding stepsPerSecond values
    - Export configuration as ES6 module
    - _Requirements: 10.3, 10.4_
  
  - [x] 13.2 Add image generation for endless mode
    - Implement procedural image generation (gradient or pattern)
    - Generate image with width from endless-config.js
    - Create canvas-based image generation
    - _Requirements: 10.2_
  
  - [x] 13.3 Create endlessModeController.js
    - Implement initialization with algorithm configs
    - Implement random algorithm selection (excluding current)
    - Implement sort completion handler
    - Implement automatic rescramble and restart logic
    - Export as ES6 module
    - _Requirements: 10.3, 10.5, 10.6_
  
  - [x] 13.4 Add URL parameter detection and fullscreen mode
    - Detect ?background=true query parameter on page load
    - Enter fullscreen mode automatically
    - Hide UI controls in endless mode
    - Scale canvas to fullscreen dimensions
    - _Requirements: 10.1, 10.2, 10.7_
  
  - [x] 13.5 Integrate endless mode with visualization engine
    - Connect sort completion callback to endless mode controller
    - Apply algorithm-specific speed from configuration
    - Implement continuous loop (sort → wait → rescramble → select new algorithm → repeat)
    - _Requirements: 10.4, 10.5, 10.6_
  
  - [x] 13.6 Write property test for endless mode speed configuration
    - **Property 14: Endless mode speed configuration**
    - **Validates: Requirements 10.4**
  
  - [x] 13.7 Write property test for algorithm rotation
    - **Property 15: Endless mode algorithm rotation**
    - **Validates: Requirements 10.5**
  
  - [x] 13.8 Write unit tests for endless mode
    - Test URL parameter detection
    - Test fullscreen activation
    - Test UI hiding
    - Test image generation with configured column count
    - Test algorithm selection excludes current algorithm
    - _Requirements: 10.1, 10.2, 10.3, 10.5, 10.7_

- [ ] 14. Final checkpoint - Ensure all tests pass
  - Ensure all tests pass, ask the user if questions arise.
