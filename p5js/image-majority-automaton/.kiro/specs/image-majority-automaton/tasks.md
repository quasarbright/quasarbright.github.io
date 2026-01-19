# Implementation Plan: Image Dithering Majority Automaton

## Overview

This implementation plan bridges the gap between the current codebase and the complete design. The application currently has the HTML structure, app controller, and dithering shader implemented. The remaining work focuses on implementing the WebGL renderer module, automaton shader, supporting shaders, CSS styling, and comprehensive testing.

## Tasks

- [x] 1. Create CSS styling for dark theme interface
  - Implement dark background colors and light text
  - Style all controls (buttons, sliders, file upload)
  - Style canvas container and placeholder text
  - Style error and warning message displays
  - Ensure sufficient contrast for accessibility
  - _Requirements: 7.1, 7.2, 7.3, 7.4_

- [x] 2. Implement WebGL Renderer module
  - [x] 2.1 Create `js/webgl-renderer.js` with WebGLRenderer class
    - Initialize WebGL context with error checking
    - Implement shader loading from external files
    - Implement shader compilation and program creation
    - Set up texture creation utilities
    - Implement ping-pong buffer setup
    - Implement quad rendering utilities
    - _Requirements: 5.1, 5.6, 9.1, 9.2_

- [x] 3. Create vertex shader for full-screen quad rendering
  - Create `shaders/vertex.vert` with vertex shader code
  - Transform clip space to texture coordinates
  - Share across all fragment shaders
  - _Requirements: 5.2, 9.3_

- [x] 4. Implement automaton fragment shader
  - [x] 4.1 Create `shaders/automaton.frag` with majority rule logic
    - use `automaton-reference/main.js` as a reference
    - Implement Moore neighborhood sampling (8 neighbors)
    - Implement color counting and majority detection
    - Implement tie-breaking with deterministic randomization
    - Ensure palette colors are preserved
    - _Requirements: 4.5, 4.6, 4.7, 4.8, 4.9_

- [x] 5. Create copy shader for texture operations
  - Create `shaders/copy.frag` for simple texture copying
  - Use for initialization and reset operations
  - _Requirements: 5.2, 9.3_

- [x] 6. Implement dithering execution in renderer
  - [x] 6.1 Implement `executeDither()` method in WebGLRenderer
    - Bind dither shader program
    - Set up uniforms (image, resolution, palette size)
    - Render to ping buffer
    - Copy result to canvas
    - _Requirements: 3.2, 3.3, 5.3_

- [x] 7. Implement automaton execution in renderer
  - [x] 7.1 Implement `executeAutomatonStep()` method in WebGLRenderer
    - Bind automaton shader program
    - Set up uniforms (state texture, resolution, palette size, random seed)
    - Implement ping-pong rendering (read from current, write to next)
    - Swap buffers after each step
    - Copy result to canvas
    - _Requirements: 4.5, 5.1, 5.5_

- [ ] 8. Checkpoint - Ensure all core functionality works
  - Verify image upload and display
  - Verify dithering produces correct output
  - Verify automaton runs and evolves
  - Verify reset restores original image
  - Ensure all tests pass, ask the user if questions arise.

- [ ] 9. Implement control state management
  - [ ] 9.1 Update `enableControls()` to properly manage button states
    - Disable automaton controls until dithering is applied
    - Enable dither button after image upload
    - Enable automaton controls after dithering
    - _Requirements: 1.4, 3.7, 4.2_

- [ ] 10. Implement validation and error handling
  - [ ] 10.1 Add image format validation
    - Validate MIME types against allowed list
    - Display error for invalid formats
    - _Requirements: 1.1, 1.3_

  - [ ] 10.2 Add palette size validation and clamping
    - Clamp input to [2, 256] range
    - Update display with clamped value
    - _Requirements: 2.3_

  - [ ] 10.3 Add large image warning
    - Check dimensions on upload
    - Display warning for images > 2048px
    - Display error and reject images > 4096px
    - _Requirements: 8.3_

- [ ] 11. Implement pixel-perfect rendering
  - [ ] 11.1 Ensure canvas dimensions match image exactly
    - Set canvas.width and canvas.height to image dimensions
    - Disable image smoothing on canvas context
    - Update viewport to match canvas size
    - _Requirements: 6.1, 6.2, 6.3, 6.4_

- [ ] 12. Implement reset functionality
  - [ ] 12.1 Complete `resetCanvas()` method
    - Stop automaton if running
    - Restore original image from stored ImageData
    - Reset ping-pong buffers to initial state
    - Update canvas display
    - _Requirements: 4.11_

- [ ] 13. Performance optimization
  - [ ] 13.1 Implement FPS monitoring
    - Add frame time tracking
    - Calculate and log FPS
    - Display warning if FPS drops below 30
    - _Requirements: 5.7, 8.2_

- [ ] 14. Verify no external dependencies
  - [ ] 14.1 Audit all files for external imports
    - Check HTML for external script tags
    - Check JavaScript for import statements
    - Verify only vanilla web technologies used
    - _Requirements: 9.1, 9.4, 9.5, 9.6_

- [ ] 15. Verify shader file separation
  - [ ] 15.1 Ensure all shaders are in separate files
    - Verify dither.frag exists and is loaded
    - Verify automaton.frag exists and is loaded
    - Verify copy.frag exists and is loaded
    - Verify vertex.vert exists and is loaded
    - No shader code embedded in JavaScript strings
    - _Requirements: 3.4, 5.2, 5.4, 9.3_

- [ ] 16. Final integration testing
  - [ ] 16.1 Manual end-to-end testing
    - Test complete workflow: upload → dither → automaton → reset
    - Test with various image formats (PNG, JPG, GIF, WebP)
    - Test with various palette sizes (2, 8, 16, 64, 256)
    - Test pause/resume functionality
    - Test error handling with invalid inputs
    - _Requirements: All_

- [ ] 17. Final checkpoint - Complete verification
  - Verify all requirements are met
  - Test on multiple browsers (Chrome, Firefox, Safari, Edge)
  - Verify performance meets targets
  - Ensure all functionality works correctly, ask the user if questions arise.

## Notes

- Each task references specific requirements for traceability
- Checkpoints ensure incremental validation at key milestones
- The WebGL renderer is the core missing piece that needs to be implemented first
- All shaders must be in separate files per the design requirements
