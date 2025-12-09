# Requirements Document

## Introduction

The Image Column Sorter is a web-based sorting algorithm visualizer that provides an engaging way to observe sorting algorithms in action. Users upload an image, which is then divided into vertical columns. These columns are scrambled and subsequently sorted back into their original order using various sorting algorithms, creating a visual representation of how different sorting algorithms work.

## Glossary

- **System**: The Image Column Sorter application
- **User**: A person interacting with the application through a web browser
- **Image Column**: A single vertical slice of pixels from the uploaded image, one pixel wide and spanning the full height
- **Scrambled State**: The state where image columns have been randomly reordered from their original positions
- **Sorting Algorithm**: A computational procedure that reorders the scrambled columns back to their original positions
- **Visualization**: The animated display showing columns being compared and swapped during the sorting process
- **Column Comparison**: The process of determining the relative order of two columns based on their original positions
- **Animation Frame**: A single step in the visualization showing the current state of column arrangement
- **Audio Feedback**: Sound generated during sorting operations that corresponds to column positions or comparisons
- **Column Highlight**: A semi-transparent visual overlay indicating the current operation being performed on specific columns

## Requirements

### Requirement 1

**User Story:** As a user, I want to upload an image file, so that I can visualize sorting algorithms operating on its columns.

#### Acceptance Criteria

1. WHEN a user selects an image file from their device THEN the System SHALL load and display the image in the application
2. WHEN an image is uploaded THEN the System SHALL support common image formats including PNG, JPEG, and GIF
3. WHEN an image exceeds reasonable dimensions THEN the System SHALL scale the image to fit within display constraints while maintaining aspect ratio
4. WHEN an invalid file is selected THEN the System SHALL display an error message and prevent further processing
5. WHEN an image is successfully loaded THEN the System SHALL enable the scramble functionality

### Requirement 2

**User Story:** As a user, I want to scramble the image columns, so that I can see the sorting algorithm restore the original image.

#### Acceptance Criteria

1. WHEN a user triggers the scramble action THEN the System SHALL randomly reorder all image columns
2. WHEN columns are scrambled THEN the System SHALL maintain the visual integrity of each individual column
3. WHEN scrambling occurs THEN the System SHALL store the mapping between scrambled positions and original positions
4. WHEN scrambling completes THEN the System SHALL display the scrambled image and enable sorting controls
5. WHEN an image is already scrambled THEN the System SHALL allow re-scrambling with a new random order

### Requirement 3

**User Story:** As a user, I want to select different sorting algorithms, so that I can compare how different algorithms work visually.

#### Acceptance Criteria

1. WHEN a user views the algorithm selection interface THEN the System SHALL display available sorting algorithms including Bubble Sort, Quick Sort, Insertion Sort, Selection Sort, and Merge Sort
2. WHEN a user selects a sorting algorithm THEN the System SHALL prepare that algorithm for execution on the scrambled columns
3. WHEN no algorithm is selected THEN the System SHALL prevent the start of sorting visualization
4. WHEN an algorithm is selected THEN the System SHALL display information about the selected algorithm's characteristics

### Requirement 4

**User Story:** As a user, I want to watch the sorting algorithm animate the column reordering, so that I can understand how the algorithm works.

#### Acceptance Criteria

1. WHEN a user starts the sorting visualization THEN the System SHALL animate the column comparisons and swaps according to the selected algorithm
2. WHEN columns are being compared THEN the System SHALL apply a semi-transparent green highlight overlay to the columns being compared
3. WHEN the algorithm is actively operating on specific columns THEN the System SHALL apply a semi-transparent red highlight overlay to those columns
4. WHEN columns are swapped THEN the System SHALL animate the swap with smooth transitions
5. WHEN the sorting completes THEN the System SHALL display the restored original image and remove all highlights
6. WHEN sorting is in progress THEN the System SHALL update the display at a frame rate that makes individual operations visible

### Requirement 5

**User Story:** As a user, I want to control the visualization speed, so that I can observe the algorithm at a pace that suits my learning needs.

#### Acceptance Criteria

1. WHEN a user adjusts the speed control THEN the System SHALL modify the delay between animation frames accordingly
2. WHEN the speed is set to minimum THEN the System SHALL provide sufficient delay to observe each operation clearly
3. WHEN the speed is set to maximum THEN the System SHALL execute operations as quickly as the display can render
4. WHEN speed is adjusted during sorting THEN the System SHALL apply the new speed to subsequent operations immediately

### Requirement 6

**User Story:** As a user, I want to pause and resume the sorting visualization, so that I can examine specific states of the algorithm.

#### Acceptance Criteria

1. WHEN a user triggers pause during sorting THEN the System SHALL halt the visualization at its current state
2. WHEN the visualization is paused THEN the System SHALL maintain the current column arrangement and highlight states
3. WHEN a user triggers resume THEN the System SHALL continue the sorting visualization from the paused state
4. WHEN sorting completes THEN the System SHALL disable the pause control

### Requirement 7

**User Story:** As a user, I want to reset the visualization, so that I can start over with a new scramble or different algorithm.

#### Acceptance Criteria

1. WHEN a user triggers reset during or after sorting THEN the System SHALL return to the scrambled state
2. WHEN reset occurs THEN the System SHALL clear any visualization highlights or indicators
3. WHEN reset completes THEN the System SHALL enable algorithm selection and sorting controls
4. WHEN reset is triggered THEN the System SHALL stop any ongoing sorting animation

### Requirement 8

**User Story:** As a user, I want to hear audio feedback during sorting operations, so that I can experience the sorting process through sound as well as visuals.

#### Acceptance Criteria

1. WHEN the application loads THEN the System SHALL have audio muted by default
2. WHEN a user toggles the audio control THEN the System SHALL enable or disable audio feedback accordingly
3. WHEN columns are compared during sorting with audio enabled THEN the System SHALL generate a tone that corresponds to the positions or values of the columns being compared
4. WHEN audio is enabled during active sorting THEN the System SHALL play sounds synchronized with visual operations
5. WHEN audio is disabled THEN the System SHALL immediately stop generating sounds

### Requirement 9

**User Story:** As a developer, I want the column comparison logic to be based on original positions, so that the sorting algorithm can correctly restore the image.

#### Acceptance Criteria

1. WHEN the System compares two columns THEN the System SHALL determine order based on the columns' original positions in the source image
2. WHEN a sorting algorithm requests comparison THEN the System SHALL return a consistent ordering relationship for any pair of columns
3. WHEN all columns are sorted THEN the System SHALL have arranged columns in ascending order of their original positions
4. WHEN the sorting algorithm completes THEN the System SHALL have restored the exact original image

### Requirement 10

**User Story:** As a user, I want an endless background mode that continuously demonstrates sorting algorithms, so that I can use the visualization as an ambient display.

#### Acceptance Criteria

1. WHEN the URL contains the query parameter "background=true" THEN the System SHALL enter fullscreen endless mode automatically
2. WHEN in endless mode THEN the System SHALL generate a 500-pixel wide image and scale the canvas to fullscreen dimensions
3. WHEN endless mode starts THEN the System SHALL select a random sorting algorithm from a predefined list with algorithm-specific speed configurations
4. WHEN endless mode starts THEN the System SHALL execute the selected algorithm at its configured steps-per-second rate
5. WHEN a sorting algorithm completes in endless mode THEN the System SHALL rescramble the columns and select a different random algorithm with its corresponding speed
6. WHEN in endless mode THEN the System SHALL continue the cycle of sort-rescramble-sort indefinitely without user interaction
7. WHEN in endless mode THEN the System SHALL hide all UI controls to provide an unobstructed visualization experience
