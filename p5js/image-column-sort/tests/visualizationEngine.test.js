import { describe, test, expect, beforeEach, beforeAll, afterAll } from '@jest/globals';
import * as fc from 'fast-check';
import { VisualizationEngine } from '../js/visualizationEngine.js';
import { HighlightManager } from '../js/highlightManager.js';
import { AudioGenerator } from '../js/audioGenerator.js';
import { createIdentityOrder, scrambleColumns } from '../js/types.js';
import { BubbleSort } from '../js/sortingAlgorithms.js';

// Mock requestAnimationFrame and cancelAnimationFrame for Node.js environment
let animationFrameId = 0;
const animationFrameCallbacks = new Map();

beforeAll(() => {
    global.requestAnimationFrame = (callback) => {
        const id = ++animationFrameId;
        animationFrameCallbacks.set(id, callback);
        // Don't actually execute the callback in tests to avoid infinite loops
        return id;
    };
    
    global.cancelAnimationFrame = (id) => {
        animationFrameCallbacks.delete(id);
    };
    
    global.performance = global.performance || {};
    global.performance.now = global.performance.now || (() => Date.now());
});

afterAll(() => {
    delete global.requestAnimationFrame;
    delete global.cancelAnimationFrame;
});

/**
 * Helper function to create a mock canvas
 */
function createMockCanvas() {
    const canvas = {
        width: 100,
        height: 100,
        getContext: () => ({
            clearRect: () => {},
            drawImage: () => {},
            fillRect: () => {},
            fillStyle: ''
        })
    };
    return canvas;
}

/**
 * Helper function to create a mock image
 */
function createMockImage(width = 100, height = 100) {
    return {
        width,
        height,
        src: ''
    };
}

// **Feature: image-column-sorter, Property 10: Pause preserves state**
// **Validates: Requirements 6.2**
describe('Property 10: Pause Preserves State', () => {
    let canvas, sourceImage, highlightManager, audioGenerator;

    beforeEach(() => {
        canvas = createMockCanvas();
        sourceImage = createMockImage();
        highlightManager = new HighlightManager();
        audioGenerator = new AudioGenerator();
    });

    test('Pausing preserves column order', () => {
        fc.assert(
            fc.property(
                fc.integer({ min: 5, max: 50 }),
                (length) => {
                    const engine = new VisualizationEngine(canvas, sourceImage, highlightManager, audioGenerator);
                    
                    const scrambled = scrambleColumns(length);
                    const columnOrder = [...scrambled];
                    const steps = BubbleSort.sort([...columnOrder]);
                    
                    // Start the visualization
                    engine.start(steps, columnOrder, scrambled);
                    
                    // Capture state before pause
                    const orderBeforePause = [...columnOrder];
                    
                    // Pause
                    engine.pause();
                    
                    // Verify column order hasn't changed
                    for (let i = 0; i < length; i++) {
                        if (columnOrder[i] !== orderBeforePause[i]) {
                            return false;
                        }
                    }
                    
                    return true;
                }
            ),
            { numRuns: 100 }
        );
    });

    test('Pausing preserves step index', () => {
        fc.assert(
            fc.property(
                fc.integer({ min: 5, max: 50 }),
                (length) => {
                    const engine = new VisualizationEngine(canvas, sourceImage, highlightManager, audioGenerator);
                    
                    const scrambled = scrambleColumns(length);
                    const columnOrder = [...scrambled];
                    const steps = BubbleSort.sort([...columnOrder]);
                    
                    // Start the visualization
                    engine.start(steps, columnOrder, scrambled);
                    
                    // Capture step index before pause
                    const stepIndexBeforePause = engine.currentStepIndex;
                    
                    // Pause
                    engine.pause();
                    
                    // Verify step index hasn't changed
                    return engine.currentStepIndex === stepIndexBeforePause;
                }
            ),
            { numRuns: 100 }
        );
    });

    test('Pausing sets isPaused flag correctly', () => {
        fc.assert(
            fc.property(
                fc.integer({ min: 5, max: 50 }),
                (length) => {
                    const engine = new VisualizationEngine(canvas, sourceImage, highlightManager, audioGenerator);
                    
                    const scrambled = scrambleColumns(length);
                    const columnOrder = [...scrambled];
                    const steps = BubbleSort.sort([...columnOrder]);
                    
                    // Start the visualization
                    engine.start(steps, columnOrder, scrambled);
                    
                    // Verify not paused initially
                    if (engine.isPausedState()) {
                        return false;
                    }
                    
                    // Pause
                    engine.pause();
                    
                    // Verify paused flag is set
                    if (!engine.isPausedState()) {
                        return false;
                    }
                    
                    // Verify still playing
                    return engine.isPlayingState();
                }
            ),
            { numRuns: 100 }
        );
    });

    test('Pausing when not playing has no effect', () => {
        fc.assert(
            fc.property(
                fc.integer({ min: 5, max: 50 }),
                (length) => {
                    const engine = new VisualizationEngine(canvas, sourceImage, highlightManager, audioGenerator);
                    
                    // Try to pause without starting
                    engine.pause();
                    
                    // Should not be paused or playing
                    return !engine.isPausedState() && !engine.isPlayingState();
                }
            ),
            { numRuns: 100 }
        );
    });

    test('Resuming from pause restores animation', () => {
        fc.assert(
            fc.property(
                fc.integer({ min: 5, max: 50 }),
                (length) => {
                    const engine = new VisualizationEngine(canvas, sourceImage, highlightManager, audioGenerator);
                    
                    const scrambled = scrambleColumns(length);
                    const columnOrder = [...scrambled];
                    const steps = BubbleSort.sort([...columnOrder]);
                    
                    // Start the visualization
                    engine.start(steps, columnOrder, scrambled);
                    
                    // Pause
                    engine.pause();
                    
                    // Verify paused
                    if (!engine.isPausedState()) {
                        return false;
                    }
                    
                    // Resume
                    engine.resume();
                    
                    // Verify not paused but still playing
                    return !engine.isPausedState() && engine.isPlayingState();
                }
            ),
            { numRuns: 100 }
        );
    });

    test('Multiple pause/resume cycles preserve state', () => {
        fc.assert(
            fc.property(
                fc.integer({ min: 5, max: 50 }),
                fc.integer({ min: 1, max: 5 }),
                (length, cycles) => {
                    const engine = new VisualizationEngine(canvas, sourceImage, highlightManager, audioGenerator);
                    
                    const scrambled = scrambleColumns(length);
                    const columnOrder = [...scrambled];
                    const steps = BubbleSort.sort([...columnOrder]);
                    
                    // Start the visualization
                    engine.start(steps, columnOrder, scrambled);
                    
                    const initialOrder = [...columnOrder];
                    const initialStepIndex = engine.currentStepIndex;
                    
                    // Perform multiple pause/resume cycles
                    for (let i = 0; i < cycles; i++) {
                        engine.pause();
                        engine.resume();
                    }
                    
                    // Verify state is preserved (no steps executed during pause/resume)
                    for (let i = 0; i < length; i++) {
                        if (columnOrder[i] !== initialOrder[i]) {
                            return false;
                        }
                    }
                    
                    return engine.currentStepIndex === initialStepIndex;
                }
            ),
            { numRuns: 100 }
        );
    });
});

// **Feature: image-column-sorter, Property 11: Reset restores scrambled state**
// **Validates: Requirements 7.1**
describe('Property 11: Reset Restores Scrambled State', () => {
    let canvas, sourceImage, highlightManager, audioGenerator;

    beforeEach(() => {
        canvas = createMockCanvas();
        sourceImage = createMockImage();
        highlightManager = new HighlightManager();
        audioGenerator = new AudioGenerator();
    });

    test('Reset restores exact scrambled column order', () => {
        fc.assert(
            fc.property(
                fc.integer({ min: 5, max: 50 }),
                (length) => {
                    const engine = new VisualizationEngine(canvas, sourceImage, highlightManager, audioGenerator);
                    
                    const scrambled = scrambleColumns(length);
                    const columnOrder = [...scrambled];
                    const steps = BubbleSort.sort([...columnOrder]);
                    
                    // Start the visualization
                    engine.start(steps, columnOrder, scrambled);
                    
                    // Let some steps execute (simulate by manually advancing)
                    // In a real scenario, steps would execute via animation loop
                    // For testing, we'll just modify the columnOrder to simulate progress
                    if (columnOrder.length > 1) {
                        [columnOrder[0], columnOrder[1]] = [columnOrder[1], columnOrder[0]];
                    }
                    
                    // Reset
                    const restoredOrder = engine.reset();
                    
                    // Verify restored order matches original scrambled state
                    for (let i = 0; i < length; i++) {
                        if (restoredOrder[i] !== scrambled[i]) {
                            return false;
                        }
                    }
                    
                    return true;
                }
            ),
            { numRuns: 100 }
        );
    });

    test('Reset clears highlights', () => {
        fc.assert(
            fc.property(
                fc.integer({ min: 5, max: 50 }),
                (length) => {
                    const engine = new VisualizationEngine(canvas, sourceImage, highlightManager, audioGenerator);
                    
                    const scrambled = scrambleColumns(length);
                    const columnOrder = [...scrambled];
                    const steps = BubbleSort.sort([...columnOrder]);
                    
                    // Start the visualization
                    engine.start(steps, columnOrder, scrambled);
                    
                    // Set some highlights
                    highlightManager.setComparisonHighlight([0, 1]);
                    highlightManager.setOperationHighlight([2, 3]);
                    
                    // Verify highlights are set
                    if (!highlightManager.hasHighlights()) {
                        return false;
                    }
                    
                    // Reset
                    engine.reset();
                    
                    // Verify highlights are cleared
                    return !highlightManager.hasHighlights();
                }
            ),
            { numRuns: 100 }
        );
    });

    test('Reset stops ongoing animation', () => {
        fc.assert(
            fc.property(
                fc.integer({ min: 5, max: 50 }),
                (length) => {
                    const engine = new VisualizationEngine(canvas, sourceImage, highlightManager, audioGenerator);
                    
                    const scrambled = scrambleColumns(length);
                    const columnOrder = [...scrambled];
                    const steps = BubbleSort.sort([...columnOrder]);
                    
                    // Start the visualization
                    engine.start(steps, columnOrder, scrambled);
                    
                    // Verify playing
                    if (!engine.isPlayingState()) {
                        return false;
                    }
                    
                    // Reset
                    engine.reset();
                    
                    // Verify not playing and not paused
                    return !engine.isPlayingState() && !engine.isPausedState();
                }
            ),
            { numRuns: 100 }
        );
    });

    test('Reset resets step index to 0', () => {
        fc.assert(
            fc.property(
                fc.integer({ min: 5, max: 50 }),
                (length) => {
                    const engine = new VisualizationEngine(canvas, sourceImage, highlightManager, audioGenerator);
                    
                    const scrambled = scrambleColumns(length);
                    const columnOrder = [...scrambled];
                    const steps = BubbleSort.sort([...columnOrder]);
                    
                    // Start the visualization
                    engine.start(steps, columnOrder, scrambled);
                    
                    // Manually advance step index to simulate progress
                    engine.currentStepIndex = 10;
                    
                    // Reset
                    engine.reset();
                    
                    // Verify step index is reset to 0
                    return engine.currentStepIndex === 0;
                }
            ),
            { numRuns: 100 }
        );
    });

    test('Reset can be called multiple times safely', () => {
        fc.assert(
            fc.property(
                fc.integer({ min: 5, max: 50 }),
                fc.integer({ min: 1, max: 5 }),
                (length, resetCount) => {
                    const engine = new VisualizationEngine(canvas, sourceImage, highlightManager, audioGenerator);
                    
                    const scrambled = scrambleColumns(length);
                    const columnOrder = [...scrambled];
                    const steps = BubbleSort.sort([...columnOrder]);
                    
                    // Start the visualization
                    engine.start(steps, columnOrder, scrambled);
                    
                    // Call reset multiple times
                    for (let i = 0; i < resetCount; i++) {
                        engine.reset();
                    }
                    
                    // Verify final state matches scrambled
                    for (let i = 0; i < length; i++) {
                        if (columnOrder[i] !== scrambled[i]) {
                            return false;
                        }
                    }
                    
                    return engine.currentStepIndex === 0 && !engine.isPlayingState();
                }
            ),
            { numRuns: 100 }
        );
    });

    test('Reset works when called during paused state', () => {
        fc.assert(
            fc.property(
                fc.integer({ min: 5, max: 50 }),
                (length) => {
                    const engine = new VisualizationEngine(canvas, sourceImage, highlightManager, audioGenerator);
                    
                    const scrambled = scrambleColumns(length);
                    const columnOrder = [...scrambled];
                    const steps = BubbleSort.sort([...columnOrder]);
                    
                    // Start the visualization
                    engine.start(steps, columnOrder, scrambled);
                    
                    // Pause
                    engine.pause();
                    
                    // Modify state to simulate progress
                    if (columnOrder.length > 1) {
                        [columnOrder[0], columnOrder[1]] = [columnOrder[1], columnOrder[0]];
                    }
                    
                    // Reset while paused
                    const restoredOrder = engine.reset();
                    
                    // Verify restored order matches scrambled
                    for (let i = 0; i < length; i++) {
                        if (restoredOrder[i] !== scrambled[i]) {
                            return false;
                        }
                    }
                    
                    // Verify not playing or paused
                    return !engine.isPlayingState() && !engine.isPausedState();
                }
            ),
            { numRuns: 100 }
        );
    });

    test('Reset preserves scrambled order reference for future resets', () => {
        fc.assert(
            fc.property(
                fc.integer({ min: 5, max: 50 }),
                (length) => {
                    const engine = new VisualizationEngine(canvas, sourceImage, highlightManager, audioGenerator);
                    
                    const scrambled = scrambleColumns(length);
                    const columnOrder = [...scrambled];
                    const steps = BubbleSort.sort([...columnOrder]);
                    
                    // Start the visualization
                    engine.start(steps, columnOrder, scrambled);
                    
                    // Reset first time
                    const firstReset = engine.reset();
                    
                    // Modify column order
                    if (columnOrder.length > 1) {
                        [columnOrder[0], columnOrder[1]] = [columnOrder[1], columnOrder[0]];
                    }
                    
                    // Reset second time
                    const secondReset = engine.reset();
                    
                    // Both resets should produce the same scrambled state
                    for (let i = 0; i < length; i++) {
                        if (firstReset[i] !== secondReset[i] || firstReset[i] !== scrambled[i]) {
                            return false;
                        }
                    }
                    
                    return true;
                }
            ),
            { numRuns: 100 }
        );
    });
});
