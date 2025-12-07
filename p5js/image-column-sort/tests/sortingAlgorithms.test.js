import { describe, test, expect } from '@jest/globals';
import * as fc from 'fast-check';
import { BubbleSort, SelectionSort, InsertionSort, QuickSort, MergeSort, RadixSort } from '../js/sortingAlgorithms.js';
import { createIdentityOrder, isValidPermutation, scrambleColumns, compareColumns } from '../js/types.js';

// **Feature: image-column-sorter, Property 8: Algorithm step generation correctness**
// **Validates: Requirements 4.1**
describe('Property 8: Algorithm Step Generation Correctness', () => {
    const algorithms = [
        { algo: BubbleSort, name: 'Bubble Sort' },
        { algo: SelectionSort, name: 'Selection Sort' },
        { algo: InsertionSort, name: 'Insertion Sort' },
        { algo: QuickSort, name: 'Quick Sort' },
        { algo: MergeSort, name: 'Merge Sort' },
        { algo: RadixSort, name: 'Radix Sort' }
    ];

    test.each(algorithms)('$name generates correct steps that sort the array', ({ algo }) => {
        fc.assert(
            fc.property(
                fc.array(fc.integer({ min: 0, max: 999 }), { minLength: 1, maxLength: 100 }),
                (arr) => {
                    // Create a scrambled permutation
                    const order = createIdentityOrder(arr.length);
                    for (let i = order.length - 1; i > 0; i--) {
                        const j = Math.floor(Math.random() * (i + 1));
                        [order[i], order[j]] = [order[j], order[i]];
                    }
                    
                    const toSort = [...order];
                    const steps = algo.sort(toSort);
                    
                    // Verify array is sorted
                    for (let i = 0; i < toSort.length - 1; i++) {
                        if (toSort[i] > toSort[i + 1]) {
                            return false;
                        }
                    }
                    
                    // Verify steps end with complete
                    if (steps.length === 0 || steps[steps.length - 1].type !== 'complete') {
                        return false;
                    }
                    
                    // Verify all steps have correct structure
                    for (const step of steps) {
                        if (step.type === 'compare' || step.type === 'swap') {
                            if (!Array.isArray(step.indices) || step.indices.length !== 2) {
                                return false;
                            }
                        }
                    }
                    
                    return true;
                }
            ),
            { numRuns: 100 }
        );
    });

    test('All algorithms produce valid permutations', () => {
        fc.assert(
            fc.property(
                fc.array(fc.integer({ min: 0, max: 999 }), { minLength: 1, maxLength: 50 }),
                (arr) => {
                    const order = createIdentityOrder(arr.length);
                    for (let i = order.length - 1; i > 0; i--) {
                        const j = Math.floor(Math.random() * (i + 1));
                        [order[i], order[j]] = [order[j], order[i]];
                    }
                    
                    // Test each algorithm
                    for (const { algo } of algorithms) {
                        const toSort = [...order];
                        algo.sort(toSort);
                        
                        if (!isValidPermutation(toSort)) {
                            return false;
                        }
                    }
                    
                    return true;
                }
            ),
            { numRuns: 100 }
        );
    });

    test('Comparisons are based on original positions', () => {
        fc.assert(
            fc.property(
                fc.array(fc.integer({ min: 0, max: 999 }), { minLength: 2, maxLength: 50 }),
                (arr) => {
                    const order = createIdentityOrder(arr.length);
                    for (let i = order.length - 1; i > 0; i--) {
                        const j = Math.floor(Math.random() * (i + 1));
                        [order[i], order[j]] = [order[j], order[i]];
                    }
                    
                    const toSort = [...order];
                    BubbleSort.sort(toSort);
                    
                    // After sorting, array should be [0, 1, 2, ..., n-1]
                    for (let i = 0; i < toSort.length; i++) {
                        if (toSort[i] !== i) {
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

// **Feature: image-column-sorter, Property 9: Sorting restores original image**
// **Validates: Requirements 4.5, 9.3, 9.4**
describe('Property 9: Sorting Restores Original Image', () => {
    const algorithms = [
        { algo: BubbleSort, name: 'Bubble Sort' },
        { algo: SelectionSort, name: 'Selection Sort' },
        { algo: InsertionSort, name: 'Insertion Sort' },
        { algo: QuickSort, name: 'Quick Sort' },
        { algo: MergeSort, name: 'Merge Sort' },
        { algo: RadixSort, name: 'Radix Sort' }
    ];

    test.each(algorithms)('$name restores original order from scrambled state', ({ algo }) => {
        fc.assert(
            fc.property(
                fc.integer({ min: 1, max: 100 }),
                (length) => {
                    const original = createIdentityOrder(length);
                    const scrambled = scrambleColumns(length);
                    const toSort = [...scrambled];
                    
                    algo.sort(toSort);
                    
                    for (let i = 0; i < length; i++) {
                        if (toSort[i] !== original[i]) {
                            return false;
                        }
                    }
                    
                    return true;
                }
            ),
            { numRuns: 100 }
        );
    });

    test('All algorithms produce identical sorted results', () => {
        fc.assert(
            fc.property(
                fc.integer({ min: 1, max: 50 }),
                (length) => {
                    const scrambled = scrambleColumns(length);
                    const results = [];
                    
                    for (const { algo } of algorithms) {
                        const toSort = [...scrambled];
                        algo.sort(toSort);
                        results.push(toSort);
                    }
                    
                    // All results should be identical
                    for (let i = 1; i < results.length; i++) {
                        for (let j = 0; j < length; j++) {
                            if (results[0][j] !== results[i][j]) {
                                return false;
                            }
                        }
                    }
                    
                    return true;
                }
            ),
            { numRuns: 100 }
        );
    });

    test('Sorting produces ascending order [0, 1, 2, ..., n-1]', () => {
        fc.assert(
            fc.property(
                fc.integer({ min: 1, max: 100 }),
                (length) => {
                    const scrambled = scrambleColumns(length);
                    
                    for (const { algo } of algorithms) {
                        const toSort = [...scrambled];
                        algo.sort(toSort);
                        
                        for (let i = 0; i < length; i++) {
                            if (toSort[i] !== i) {
                                return false;
                            }
                        }
                    }
                    
                    return true;
                }
            ),
            { numRuns: 100 }
        );
    });

    test('Sorting maintains array length', () => {
        fc.assert(
            fc.property(
                fc.integer({ min: 1, max: 100 }),
                (length) => {
                    const scrambled = scrambleColumns(length);
                    
                    for (const { algo } of algorithms) {
                        const toSort = [...scrambled];
                        algo.sort(toSort);
                        
                        if (toSort.length !== length) {
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

// **Feature: image-column-sorter, Property 13: Comparison consistency and correctness**
// **Validates: Requirements 9.1, 9.2**
describe('Property 13: Comparison Consistency and Correctness', () => {
    test('Comparison function satisfies reflexivity (a == a)', () => {
        fc.assert(
            fc.property(
                fc.array(fc.integer({ min: 0, max: 999 }), { minLength: 1, maxLength: 100 }),
                fc.integer({ min: 0, max: 99 }),
                (arr, indexSeed) => {
                    const order = createIdentityOrder(arr.length);
                    for (let i = order.length - 1; i > 0; i--) {
                        const j = Math.floor(Math.random() * (i + 1));
                        [order[i], order[j]] = [order[j], order[i]];
                    }
                    
                    const i = indexSeed % order.length;
                    const result = compareColumns(order, i, i);
                    
                    return result === 0;
                }
            ),
            { numRuns: 100 }
        );
    });

    test('Comparison function satisfies antisymmetry', () => {
        fc.assert(
            fc.property(
                fc.array(fc.integer({ min: 0, max: 999 }), { minLength: 2, maxLength: 100 }),
                fc.integer({ min: 0, max: 99 }),
                fc.integer({ min: 0, max: 99 }),
                (arr, seed1, seed2) => {
                    const order = createIdentityOrder(arr.length);
                    for (let i = order.length - 1; i > 0; i--) {
                        const j = Math.floor(Math.random() * (i + 1));
                        [order[i], order[j]] = [order[j], order[i]];
                    }
                    
                    const i = seed1 % order.length;
                    const j = seed2 % order.length;
                    
                    const result1 = compareColumns(order, i, j);
                    const result2 = compareColumns(order, j, i);
                    
                    if (result1 < 0) {
                        return result2 > 0;
                    } else if (result1 > 0) {
                        return result2 < 0;
                    } else {
                        return result2 === 0;
                    }
                }
            ),
            { numRuns: 100 }
        );
    });

    test('Comparison function satisfies transitivity', () => {
        fc.assert(
            fc.property(
                fc.array(fc.integer({ min: 0, max: 999 }), { minLength: 3, maxLength: 100 }),
                fc.integer({ min: 0, max: 99 }),
                fc.integer({ min: 0, max: 99 }),
                fc.integer({ min: 0, max: 99 }),
                (arr, seed1, seed2, seed3) => {
                    const order = createIdentityOrder(arr.length);
                    for (let i = order.length - 1; i > 0; i--) {
                        const j = Math.floor(Math.random() * (i + 1));
                        [order[i], order[j]] = [order[j], order[i]];
                    }
                    
                    const i = seed1 % order.length;
                    const j = seed2 % order.length;
                    const k = seed3 % order.length;
                    
                    const ab = compareColumns(order, i, j);
                    const bc = compareColumns(order, j, k);
                    const ac = compareColumns(order, i, k);
                    
                    if (ab < 0 && bc < 0) {
                        return ac < 0;
                    }
                    if (ab > 0 && bc > 0) {
                        return ac > 0;
                    }
                    return true;
                }
            ),
            { numRuns: 100 }
        );
    });

    test('Comparison function is consistent', () => {
        fc.assert(
            fc.property(
                fc.array(fc.integer({ min: 0, max: 999 }), { minLength: 2, maxLength: 100 }),
                fc.integer({ min: 0, max: 99 }),
                fc.integer({ min: 0, max: 99 }),
                (arr, seed1, seed2) => {
                    const order = createIdentityOrder(arr.length);
                    for (let i = order.length - 1; i > 0; i--) {
                        const j = Math.floor(Math.random() * (i + 1));
                        [order[i], order[j]] = [order[j], order[i]];
                    }
                    
                    const i = seed1 % order.length;
                    const j = seed2 % order.length;
                    
                    const result1 = compareColumns(order, i, j);
                    const result2 = compareColumns(order, i, j);
                    const result3 = compareColumns(order, i, j);
                    
                    return result1 === result2 && result2 === result3;
                }
            ),
            { numRuns: 100 }
        );
    });

    test('Comparison correctly orders based on original positions', () => {
        fc.assert(
            fc.property(
                fc.array(fc.integer({ min: 0, max: 999 }), { minLength: 2, maxLength: 100 }),
                fc.integer({ min: 0, max: 99 }),
                fc.integer({ min: 0, max: 99 }),
                (arr, seed1, seed2) => {
                    const order = createIdentityOrder(arr.length);
                    for (let i = order.length - 1; i > 0; i--) {
                        const j = Math.floor(Math.random() * (i + 1));
                        [order[i], order[j]] = [order[j], order[i]];
                    }
                    
                    const i = seed1 % order.length;
                    const j = seed2 % order.length;
                    
                    const result = compareColumns(order, i, j);
                    
                    if (order[i] < order[j]) {
                        return result < 0;
                    } else if (order[i] > order[j]) {
                        return result > 0;
                    } else {
                        return result === 0;
                    }
                }
            ),
            { numRuns: 100 }
        );
    });
});
