# Requirements Document

## Introduction

The Image Dithering Majority Automaton is a web-based interactive application that allows users to upload images, apply color quantization through dithering, and visualize the majority automaton cellular automaton running on the dithered result. The application provides pixel-perfect rendering where the canvas size matches the uploaded image dimensions, ensuring no scaling artifacts. Users can control the color palette size and observe how the majority rule evolves the image over time. Both dithering and the automaton run on the GPU for maximum performance.

## Glossary

- **System**: The Image Dithering Majority Automaton web application
- **Dithering**: A technique for reducing the color palette of an image while maintaining visual quality through spatial distribution of colors
- **Ordered_Dithering**: A GPU-friendly dithering technique using a Bayer threshold matrix
- **Majority_Automaton**: A cellular automaton where each cell takes the most common color among its neighbors in the next generation
- **Canvas**: The HTML5 canvas element used for rendering the image
- **Palette**: The set of colors available after quantization
- **Color_Quantization**: The process of reducing the number of distinct colors in an image
- **WebGL**: Web Graphics Library, used for GPU-accelerated rendering and computation
- **Fragment_Shader**: A GPU program that processes each pixel in parallel
- **Bayer_Matrix**: A threshold matrix used for ordered dithering

## Requirements

### Requirement 1: Image Upload

**User Story:** As a user, I want to upload an image file, so that I can apply dithering and run the automaton on it.

#### Acceptance Criteria

1. WHEN a user clicks the upload button or drag-and-drops a file, THE System SHALL accept common image formats (PNG, JPG, JPEG, GIF, WebP)
2. WHEN an image is uploaded, THE System SHALL load and display it on the canvas at its original dimensions
3. IF an invalid file type is provided, THEN THE System SHALL display an error message and reject the upload
4. WHEN an image is successfully loaded, THE System SHALL enable the dithering and automaton controls

### Requirement 2: Color Palette Control

**User Story:** As a user, I want to control the number of colors in the palette, so that I can experiment with different levels of color quantization.

#### Acceptance Criteria

1. THE System SHALL provide a control (slider or input) for selecting the number of colors in the palette
2. WHEN the palette size is changed, THE System SHALL update the palette size for subsequent dithering operations
3. THE System SHALL support palette sizes from 2 to 256 colors
4. WHEN the palette size is set to a value, THE System SHALL display the current value to the user

### Requirement 3: Image Dithering

**User Story:** As a user, I want to dither the uploaded image with the selected palette, so that I can prepare it for the majority automaton simulation.

#### Acceptance Criteria

1. THE System SHALL provide a "Dither" button that applies dithering to the uploaded image
2. WHEN the dither button is clicked, THE System SHALL apply ordered dithering using a Bayer matrix on the GPU
3. THE System SHALL use WebGL with a dedicated fragment shader for dithering operations
4. THE System SHALL store the dithering shader in a separate shader file
5. WHEN dithering is applied, THE System SHALL output palette indices (integers from 0 to palette_size-1) for each pixel
6. WHEN dithering is applied, THE System SHALL generate a palette as an array of RGB colors
7. WHEN dithering is applied, THE System SHALL enable the automaton controls
8. THE System SHALL store both the palette indices and the palette colors for use by the automaton

### Requirement 4: Majority Automaton Simulation

**User Story:** As a user, I want to run the majority automaton on the dithered image, so that I can observe how the cellular automaton evolves the quantized colors.

#### Acceptance Criteria

1. THE System SHALL provide controls to start, pause, and reset the automaton simulation
2. THE System SHALL disable automaton controls until dithering has been applied
3. WHEN the automaton runs, THE System SHALL receive palette indices as integers from the dithered image
4. WHEN the automaton runs, THE System SHALL receive the palette as an array of RGB colors
5. WHEN the automaton runs, THE System SHALL apply the majority rule to each pixel based on its neighbors using GPU acceleration
6. WHEN calculating the majority, THE System SHALL consider the 8 surrounding neighbors (Moore neighborhood)
7. WHEN calculating the majority, THE System SHALL operate on palette indices (not RGB values)
8. WHEN rendering, THE System SHALL map palette indices to RGB colors using the palette array
9. WHEN there is a tie in neighbor palette indices, THE System SHALL use a consistent tie-breaking rule
10. THE System SHALL update the canvas display after each generation
11. WHEN the reset button is clicked, THE System SHALL restore the canvas to the dithered image state

### Requirement 5: GPU Acceleration

**User Story:** As a user, I want both the majority automaton and dithering to run efficiently on large images, so that I can process high-resolution images smoothly.

#### Acceptance Criteria

1. THE System SHALL implement the majority automaton using WebGL for GPU acceleration
2. THE System SHALL use a dedicated fragment shader for the majority automaton, stored in a separate shader file
3. THE System SHALL implement ordered dithering using WebGL for GPU acceleration
4. THE System SHALL use a dedicated fragment shader for ordered dithering, stored in a separate shader file
5. THE System SHALL use fragment shaders to compute operations in parallel for all pixels
6. WHEN WebGL is not available, THE System SHALL display an error message
7. THE System SHALL achieve real-time performance (at least 30 FPS) for images up to 1024x1024 pixels

### Requirement 6: Pixel-Perfect Rendering

**User Story:** As a user, I want the canvas to match the exact dimensions of my uploaded image, so that I can see pixel-perfect results without scaling artifacts.

#### Acceptance Criteria

1. WHEN an image is loaded, THE System SHALL set the canvas dimensions to match the image width and height exactly
2. THE System SHALL render each pixel of the image to exactly one pixel on the canvas
3. THE System SHALL disable image smoothing and interpolation on the canvas
4. WHEN the image dimensions change, THE System SHALL update the canvas dimensions accordingly

### Requirement 7: Dark Theme Interface

**User Story:** As a user, I want the application to use a dark theme, so that I have a comfortable viewing experience.

#### Acceptance Criteria

1. THE System SHALL use a dark background color for the page
2. THE System SHALL use light-colored text for readability against the dark background
3. THE System SHALL style controls (buttons, sliders, inputs) with dark theme colors
4. THE System SHALL ensure sufficient contrast between UI elements and the background

### Requirement 8: Performance and Responsiveness

**User Story:** As a user, I want the application to run smoothly, so that I can interact with it without lag or freezing.

#### Acceptance Criteria

1. WHEN processing large images, THE System SHALL remain responsive to user input
2. THE System SHALL update the automaton at a reasonable frame rate (at least 30 FPS for typical images)
3. IF an image is too large to process efficiently, THEN THE System SHALL display a warning or limit the image size
4. THE System SHALL use efficient GPU-based algorithms for dithering and automaton updates

### Requirement 9: Technology Stack

**User Story:** As a developer, I want the application to use only vanilla web technologies, so that it remains simple, lightweight, and has no external dependencies.

#### Acceptance Criteria

1. THE System SHALL be implemented using only HTML, CSS, and JavaScript
2. THE System SHALL use WebGL for GPU acceleration with GLSL shaders
3. THE System SHALL store shader code in separate files for maintainability
4. THE System SHALL NOT use any JavaScript frameworks or libraries (no React, Vue, jQuery, etc.)
5. THE System SHALL NOT require any build tools or compilation steps
6. THE System SHALL run directly in a web browser by opening the HTML file
