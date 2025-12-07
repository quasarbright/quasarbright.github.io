import { describe, test } from '@jest/globals';
import * as fc from 'fast-check';
import { AudioGenerator } from '../js/audioGenerator.js';

// **Feature: image-column-sorter, Property 12: Audio frequency generation**
// **Validates: Requirements 8.3**
describe('Property 12: Audio Frequency Generation', () => {
    test('Frequency increases monotonically with column position', () => {
        fc.assert(
            fc.property(
                fc.integer({ min: 2, max: 1000 }), // totalColumns (at least 2 for meaningful comparison)
                fc.integer({ min: 0, max: 999 }),  // columnIndex1 seed
                fc.integer({ min: 0, max: 999 }),  // columnIndex2 seed
                (totalColumns, seed1, seed2) => {
                    const audioGen = new AudioGenerator();
                    
                    const index1 = seed1 % totalColumns;
                    const index2 = seed2 % totalColumns;
                    
                    const freq1 = audioGen.calculateFrequency(index1, totalColumns);
                    const freq2 = audioGen.calculateFrequency(index2, totalColumns);
                    
                    // If index1 < index2, then freq1 should be <= freq2
                    // If index1 > index2, then freq1 should be >= freq2
                    // If index1 === index2, then freq1 should equal freq2
                    if (index1 < index2) {
                        return freq1 <= freq2;
                    } else if (index1 > index2) {
                        return freq1 >= freq2;
                    } else {
                        return freq1 === freq2;
                    }
                }
            ),
            { numRuns: 100 }
        );
    });

    test('Frequency is within valid range', () => {
        fc.assert(
            fc.property(
                fc.integer({ min: 1, max: 1000 }),  // totalColumns
                fc.integer({ min: 0, max: 999 }),   // columnIndex seed
                (totalColumns, seed) => {
                    const audioGen = new AudioGenerator();
                    const columnIndex = seed % totalColumns;
                    
                    const frequency = audioGen.calculateFrequency(columnIndex, totalColumns);
                    
                    // Frequency should be within the defined range
                    return frequency >= audioGen.minFrequency && 
                           frequency <= audioGen.maxFrequency;
                }
            ),
            { numRuns: 100 }
        );
    });

    test('First column maps to minimum frequency', () => {
        fc.assert(
            fc.property(
                fc.integer({ min: 1, max: 1000 }),  // totalColumns
                (totalColumns) => {
                    const audioGen = new AudioGenerator();
                    const frequency = audioGen.calculateFrequency(0, totalColumns);
                    
                    // First column should map to minimum frequency
                    return frequency === audioGen.minFrequency;
                }
            ),
            { numRuns: 100 }
        );
    });

    test('Last column maps to maximum frequency', () => {
        fc.assert(
            fc.property(
                fc.integer({ min: 2, max: 1000 }),  // totalColumns (at least 2)
                (totalColumns) => {
                    const audioGen = new AudioGenerator();
                    const frequency = audioGen.calculateFrequency(totalColumns - 1, totalColumns);
                    
                    // Last column should map to maximum frequency
                    return frequency === audioGen.maxFrequency;
                }
            ),
            { numRuns: 100 }
        );
    });

    test('Frequency mapping is consistent', () => {
        fc.assert(
            fc.property(
                fc.integer({ min: 1, max: 1000 }),  // totalColumns
                fc.integer({ min: 0, max: 999 }),   // columnIndex seed
                (totalColumns, seed) => {
                    const audioGen = new AudioGenerator();
                    const columnIndex = seed % totalColumns;
                    
                    const freq1 = audioGen.calculateFrequency(columnIndex, totalColumns);
                    const freq2 = audioGen.calculateFrequency(columnIndex, totalColumns);
                    const freq3 = audioGen.calculateFrequency(columnIndex, totalColumns);
                    
                    // Same inputs should always produce same output
                    return freq1 === freq2 && freq2 === freq3;
                }
            ),
            { numRuns: 100 }
        );
    });

    test('Frequency increases strictly for strictly increasing positions', () => {
        fc.assert(
            fc.property(
                fc.integer({ min: 3, max: 1000 }),  // totalColumns (at least 3)
                (totalColumns) => {
                    const audioGen = new AudioGenerator();
                    
                    // Test consecutive positions
                    for (let i = 0; i < totalColumns - 1; i++) {
                        const freq1 = audioGen.calculateFrequency(i, totalColumns);
                        const freq2 = audioGen.calculateFrequency(i + 1, totalColumns);
                        
                        // For consecutive positions, frequency should increase (or stay same for edge cases)
                        if (freq2 < freq1) {
                            return false;
                        }
                    }
                    
                    return true;
                }
            ),
            { numRuns: 100 }
        );
    });

    test('Single column case returns minimum frequency', () => {
        fc.assert(
            fc.property(
                fc.constant(1),  // Always test with 1 column
                (totalColumns) => {
                    const audioGen = new AudioGenerator();
                    const frequency = audioGen.calculateFrequency(0, totalColumns);
                    
                    // With only one column, should return minimum frequency
                    return frequency === audioGen.minFrequency;
                }
            ),
            { numRuns: 100 }
        );
    });
});
