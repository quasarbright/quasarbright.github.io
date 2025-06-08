# GPU-Accelerated Majority Automaton

This project is a GPU-accelerated implementation of the [majority cellular automaton](https://quasarbright.github.io/p5js/majority-automaton/) using WebGL fragment shaders. The original implementation uses p5.js and runs on the CPU, while this version runs the automaton rules directly on the GPU for significantly better performance.

## How It Works

### Majority Automaton Rules

The majority automaton is a cellular automaton where each cell takes on the state that is most common among its neighbors. If there's a tie, a random choice is made among the tied states.

In this implementation:
- There are 4 possible states (colors)
- Each cell looks at its 8 surrounding neighbors
- The cell adopts the state that appears most frequently among its neighbors
- If multiple states tie for the most frequent, one is chosen randomly

### GPU Implementation

The automaton is implemented using WebGL and fragment shaders:

1. **Ping-Pong Rendering**: Two textures are used to store the current and next state of the grid. Each rendering pass reads from one texture and writes to the other.

2. **Fragment Shader**: The majority rule is implemented in a fragment shader that:
   - Samples the states of the 8 neighboring cells
   - Counts the occurrences of each state
   - Determines the majority state
   - Handles ties with a pseudo-random number generator

3. **Rendering**: The current state is rendered to the canvas using a separate fragment shader that converts the state values to colors.

## Performance Benefits

The GPU implementation offers several advantages over the CPU version:

- **Parallelism**: All cells are updated simultaneously on the GPU
- **No Data Transfer**: The grid data stays on the GPU between updates
- **Hardware Acceleration**: Takes advantage of the GPU's specialized hardware for texture operations

## Usage

Simply open `index.html` in a web browser that supports WebGL. The automaton will start running automatically.

## Credits

- Original majority automaton: [quasarbright.github.io/p5js/majority-automaton/](https://quasarbright.github.io/p5js/majority-automaton/)
