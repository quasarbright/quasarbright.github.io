/**
 * @fileoverview Configuration for endless background mode.
 * 
 * This module defines the settings for the endless background visualization mode,
 * including the number of columns for the generated image and algorithm-specific
 * speed configurations.
 */

import { 
    BubbleSort, 
    CocktailShakerSort, 
    CombSort, 
    InsertionSort, 
    SelectionSort, 
    GnomeSort, 
    ShellSort, 
    OddEvenSort, 
    HeapSort, 
    QuickSort, 
    MergeSort 
} from './sortingAlgorithms.js';

/**
 * Configuration object for endless mode.
 * 
 * @typedef {Object} EndlessModeConfig
 * @property {number} columnCount - Number of columns for generated image
 * @property {AlgorithmSpeedConfig[]} algorithms - Array of algorithm configurations
 */

/**
 * Algorithm speed configuration.
 * 
 * @typedef {Object} AlgorithmSpeedConfig
 * @property {Object} algorithm - Algorithm object with name, description, and sort function
 * @property {number} stepsPerSecond - Target steps per second for this algorithm
 */

/**
 * Endless mode configuration.
 * Different algorithms have different performance characteristics, so each
 * is configured with an appropriate speed to ensure interesting visualizations.
 * 
 * @type {EndlessModeConfig}
 */
export const ENDLESS_MODE_CONFIG = {
    columnCount: 500,
    algorithms: [
        { algorithm: BubbleSort, stepsPerSecond: 8000 },
        { algorithm: CocktailShakerSort, stepsPerSecond: 8000 },
        { algorithm: CombSort, stepsPerSecond: 1000 },
        { algorithm: InsertionSort, stepsPerSecond: 3000 },
        { algorithm: SelectionSort, stepsPerSecond: 3000 },
        { algorithm: GnomeSort, stepsPerSecond: 3000 },
        { algorithm: ShellSort, stepsPerSecond: 1000 },
        { algorithm: OddEvenSort, stepsPerSecond: 3000 },
        { algorithm: HeapSort, stepsPerSecond: 1000 },
        { algorithm: QuickSort, stepsPerSecond: 1000 },
        { algorithm: MergeSort, stepsPerSecond: 2000 },
    ]
};
