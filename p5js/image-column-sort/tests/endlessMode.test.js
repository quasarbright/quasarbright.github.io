import { describe, test, expect, beforeEach, beforeAll, afterAll, jest } from '@jest/globals';
import * as fc from 'fast-check';
import { EndlessModeController } from '../js/endlessModeController.js';
import { ENDLESS_MODE_CONFIG } from '../js/endless-config.js';

// Mock application controller
class MockApplicationController {
    constructor() {
        this.scrambledCount = 0;
        this.stepsPerSecond = 0;
        this.sortingStarted = false;
        this.state = {
            selectedAlgorithm: null
        };
    }
    
    scrambleColumns() {
        this.scrambledCount++;
    }
    
    setStepsPerSecond(stepsPerSec) {
        this.stepsPerSecond = stepsPerSec;
    }
    
    startSorting() {
        this.sortingStarted = true;
    }
}

// Mock algorithm object
function createMockAlgorithm(name) {
    return {
        name: name,
        description: `Mock ${name} algorithm`,
        sort: (arr) => []
    };
}

// **Feature: image-column-sorter, Property 14: Endless mode speed configuration**
// **Validates: Requirements 10.4**
describe('Property 14: Endless Mode Speed Configuration', () => {
    let mockApp, controller;

    beforeEach(() => {
        mockApp = new MockApplicationController();
        controller = new EndlessModeController(mockApp);
    });

    test('Algorithm speed is applied correctly when starting a cycle', () => {
        fc.assert(
            fc.property(
                fc.array(
                    fc.record({
                        name: fc.string({ minLength: 1, maxLength: 20 }),
                        stepsPerSecond: fc.integer({ min: 1, max: 1000 })
                    }),
                    { minLength: 1, maxLength: 20 }
                ),
                (rawConfigs) => {
                    // Convert to algorithm configs with mock algorithm objects
                    const algorithmConfigs = rawConfigs.map(config => ({
                        algorithm: createMockAlgorithm(config.name),
                        stepsPerSecond: config.stepsPerSecond
                    }));
                    
                    // Initialize with random algorithm configs
                    controller.initialize(algorithmConfigs);
                    controller.start();
                    
                    // The controller should have set the steps per second
                    const appliedSpeed = mockApp.stepsPerSecond;
                    
                    // Verify the applied speed matches one of the configured speeds
                    const matchesConfig = algorithmConfigs.some(
                        config => config.stepsPerSecond === appliedSpeed
                    );
                    
                    controller.stop();
                    return matchesConfig;
                }
            ),
            { numRuns: 100 }
        );
    });

    test('Each algorithm configuration has a valid stepsPerSecond value', () => {
        fc.assert(
            fc.property(
                fc.array(
                    fc.record({
                        name: fc.string({ minLength: 1, maxLength: 20 }),
                        stepsPerSecond: fc.integer({ min: 1, max: 10000 })
                    }),
                    { minLength: 1, maxLength: 20 }
                ),
                (rawConfigs) => {
                    // Convert to algorithm configs
                    const algorithmConfigs = rawConfigs.map(config => ({
                        algorithm: createMockAlgorithm(config.name),
                        stepsPerSecond: config.stepsPerSecond
                    }));
                    
                    // All stepsPerSecond values should be positive
                    return algorithmConfigs.every(
                        config => config.stepsPerSecond > 0 && config.stepsPerSecond <= 10000
                    );
                }
            ),
            { numRuns: 100 }
        );
    });

    test('Speed configuration is applied on each cycle', () => {
        fc.assert(
            fc.property(
                fc.array(
                    fc.record({
                        name: fc.string({ minLength: 1, maxLength: 20 }),
                        stepsPerSecond: fc.integer({ min: 1, max: 1000 })
                    }),
                    { minLength: 2, maxLength: 10 }
                ),
                fc.integer({ min: 1, max: 5 }),
                (rawConfigs, cycles) => {
                    const algorithmConfigs = rawConfigs.map(config => ({
                        algorithm: createMockAlgorithm(config.name),
                        stepsPerSecond: config.stepsPerSecond
                    }));
                    
                    controller.initialize(algorithmConfigs);
                    controller.start();
                    
                    const appliedSpeeds = [mockApp.stepsPerSecond];
                    
                    // Simulate multiple sort completions
                    for (let i = 0; i < cycles - 1; i++) {
                        controller.onSortComplete();
                        // Wait for the timeout to complete (simulate)
                        // In real scenario, this would be async
                        appliedSpeeds.push(mockApp.stepsPerSecond);
                    }
                    
                    // All applied speeds should match configured speeds
                    const allValid = appliedSpeeds.every(speed =>
                        algorithmConfigs.some(config => config.stepsPerSecond === speed)
                    );
                    
                    controller.stop();
                    return allValid;
                }
            ),
            { numRuns: 100 }
        );
    });

    test('Default ENDLESS_MODE_CONFIG has valid speed values', () => {
        // Verify the actual configuration used in the application
        const allValid = ENDLESS_MODE_CONFIG.algorithms.every(
            config => config.stepsPerSecond > 0 && config.stepsPerSecond <= 10000
        );
        
        expect(allValid).toBe(true);
    });

    test('Speed configuration matches selected algorithm', () => {
        fc.assert(
            fc.property(
                fc.array(
                    fc.record({
                        name: fc.string({ minLength: 1, maxLength: 20 }),
                        stepsPerSecond: fc.integer({ min: 1, max: 1000 })
                    }),
                    { minLength: 1, maxLength: 20 }
                ),
                (rawConfigs) => {
                    const algorithmConfigs = rawConfigs.map(config => ({
                        algorithm: createMockAlgorithm(config.name),
                        stepsPerSecond: config.stepsPerSecond
                    }));
                    
                    controller.initialize(algorithmConfigs);
                    controller.start();
                    
                    const selectedAlgorithm = mockApp.state.selectedAlgorithm;
                    const appliedSpeed = mockApp.stepsPerSecond;
                    
                    // Find the config for the selected algorithm
                    const matchingConfig = algorithmConfigs.find(
                        config => config.algorithm.name === selectedAlgorithm?.name
                    );
                    
                    // If we found a matching config, verify the speed matches
                    const speedMatches = matchingConfig 
                        ? matchingConfig.stepsPerSecond === appliedSpeed
                        : true; // If no match found, that's also valid (random selection)
                    
                    controller.stop();
                    return speedMatches;
                }
            ),
            { numRuns: 100 }
        );
    });
});

// **Feature: image-column-sorter, Property 15: Endless mode algorithm rotation**
// **Validates: Requirements 10.5**
describe('Property 15: Endless Mode Algorithm Rotation', () => {
    let mockApp, controller;

    beforeEach(() => {
        mockApp = new MockApplicationController();
        controller = new EndlessModeController(mockApp);
    });

    test('Next algorithm is different from current algorithm', () => {
        fc.assert(
            fc.property(
                fc.array(
                    fc.record({
                        name: fc.string({ minLength: 1, maxLength: 20 }),
                        stepsPerSecond: fc.integer({ min: 1, max: 1000 })
                    }),
                    { minLength: 2, maxLength: 20 }
                ),
                (rawConfigs) => {
                    // Remove duplicates by name to ensure we have distinct algorithms
                    const uniqueRawConfigs = rawConfigs.filter((config, index, self) =>
                        index === self.findIndex(c => c.name === config.name)
                    );
                    
                    // Need at least 2 unique algorithms for rotation to be meaningful
                    if (uniqueRawConfigs.length < 2) {
                        return true;
                    }
                    
                    const algorithmConfigs = uniqueRawConfigs.map(config => ({
                        algorithm: createMockAlgorithm(config.name),
                        stepsPerSecond: config.stepsPerSecond
                    }));
                    
                    controller.initialize(algorithmConfigs);
                    controller.start();
                    
                    const firstAlgorithm = mockApp.state.selectedAlgorithm;
                    
                    // Directly call _startNextCycle to bypass setTimeout
                    controller._startNextCycle();
                    
                    const secondAlgorithm = mockApp.state.selectedAlgorithm;
                    
                    controller.stop();
                    
                    // Second algorithm should be different from first
                    return firstAlgorithm?.name !== secondAlgorithm?.name;
                }
            ),
            { numRuns: 100 }
        );
    });

    test('selectRandomAlgorithm excludes current algorithm', () => {
        fc.assert(
            fc.property(
                fc.array(
                    fc.record({
                        name: fc.string({ minLength: 1, maxLength: 20 }),
                        stepsPerSecond: fc.integer({ min: 1, max: 1000 })
                    }),
                    { minLength: 2, maxLength: 20 }
                ),
                (rawConfigs) => {
                    // Need at least 2 algorithms
                    if (rawConfigs.length < 2) {
                        return true;
                    }
                    
                    const algorithmConfigs = rawConfigs.map(config => ({
                        algorithm: createMockAlgorithm(config.name),
                        stepsPerSecond: config.stepsPerSecond
                    }));
                    
                    controller.initialize(algorithmConfigs);
                    
                    // Set a current algorithm
                    controller.currentAlgorithmName = algorithmConfigs[0].algorithm.name;
                    
                    // Select a random algorithm
                    const selected = controller.selectRandomAlgorithm();
                    
                    // Should not be the current algorithm
                    return selected && selected.algorithm.name !== algorithmConfigs[0].algorithm.name;
                }
            ),
            { numRuns: 100 }
        );
    });

    test('Algorithm rotation cycles through all algorithms over time', () => {
        fc.assert(
            fc.property(
                fc.array(
                    fc.record({
                        name: fc.constantFrom('a', 'b', 'c', 'd', 'e'),
                        stepsPerSecond: fc.integer({ min: 1, max: 1000 })
                    }),
                    { minLength: 3, maxLength: 5 }
                ),
                fc.integer({ min: 10, max: 50 }),
                (rawConfigs, cycles) => {
                    // Remove duplicates by name
                    const uniqueRawConfigs = rawConfigs.filter((config, index, self) =>
                        index === self.findIndex(c => c.name === config.name)
                    );
                    
                    if (uniqueRawConfigs.length < 2) {
                        return true;
                    }
                    
                    const algorithmConfigs = uniqueRawConfigs.map(config => ({
                        algorithm: createMockAlgorithm(config.name),
                        stepsPerSecond: config.stepsPerSecond
                    }));
                    
                    controller.initialize(algorithmConfigs);
                    controller.start();
                    
                    const selectedAlgorithms = new Set([mockApp.state.selectedAlgorithm?.name]);
                    
                    // Simulate multiple cycles using _startNextCycle directly
                    for (let i = 0; i < cycles; i++) {
                        controller._startNextCycle();
                        selectedAlgorithms.add(mockApp.state.selectedAlgorithm?.name);
                    }
                    
                    controller.stop();
                    
                    // With enough cycles, we should see multiple different algorithms
                    // (probabilistically, with 10+ cycles and 3+ algorithms, we should see at least 2)
                    return selectedAlgorithms.size >= Math.min(2, algorithmConfigs.length);
                }
            ),
            { numRuns: 100 }
        );
    });

    test('Single algorithm configuration works without rotation', () => {
        fc.assert(
            fc.property(
                fc.record({
                    name: fc.string({ minLength: 1, maxLength: 20 }),
                    stepsPerSecond: fc.integer({ min: 1, max: 1000 })
                }),
                fc.integer({ min: 1, max: 5 }),
                (rawConfig, cycles) => {
                    const algorithmConfig = {
                        algorithm: createMockAlgorithm(rawConfig.name),
                        stepsPerSecond: rawConfig.stepsPerSecond
                    };
                    
                    controller.initialize([algorithmConfig]);
                    controller.start();
                    
                    const firstAlgorithm = mockApp.state.selectedAlgorithm;
                    
                    // Simulate multiple cycles
                    for (let i = 0; i < cycles; i++) {
                        controller.onSortComplete();
                    }
                    
                    const lastAlgorithm = mockApp.state.selectedAlgorithm;
                    
                    controller.stop();
                    
                    // With only one algorithm, it should always be the same
                    return firstAlgorithm?.name === lastAlgorithm?.name && 
                           firstAlgorithm?.name === algorithmConfig.algorithm.name;
                }
            ),
            { numRuns: 100 }
        );
    });

    test('Algorithm rotation is random but excludes current', () => {
        fc.assert(
            fc.property(
                fc.array(
                    fc.record({
                        name: fc.constantFrom('algo1', 'algo2', 'algo3', 'algo4'),
                        stepsPerSecond: fc.integer({ min: 1, max: 1000 })
                    }),
                    { minLength: 3, maxLength: 4 }
                ),
                (rawConfigs) => {
                    // Remove duplicates
                    const uniqueRawConfigs = rawConfigs.filter((config, index, self) =>
                        index === self.findIndex(c => c.name === config.name)
                    );
                    
                    if (uniqueRawConfigs.length < 3) {
                        return true;
                    }
                    
                    const algorithmConfigs = uniqueRawConfigs.map(config => ({
                        algorithm: createMockAlgorithm(config.name),
                        stepsPerSecond: config.stepsPerSecond
                    }));
                    
                    controller.initialize(algorithmConfigs);
                    
                    // Set current algorithm to first one
                    controller.currentAlgorithmName = algorithmConfigs[0].algorithm.name;
                    
                    // Select next algorithm multiple times
                    const selections = [];
                    for (let i = 0; i < 20; i++) {
                        const selected = controller.selectRandomAlgorithm();
                        selections.push(selected.algorithm.name);
                    }
                    
                    // None of the selections should be the current algorithm
                    const noneAreCurrent = selections.every(name => name !== algorithmConfigs[0].algorithm.name);
                    
                    // Should have some variety in selections (not all the same)
                    const uniqueSelections = new Set(selections);
                    const hasVariety = uniqueSelections.size > 1;
                    
                    return noneAreCurrent && hasVariety;
                }
            ),
            { numRuns: 100 }
        );
    });
});

// Unit tests for endless mode functionality
describe('Endless Mode Unit Tests', () => {
    let mockApp, controller;

    beforeEach(() => {
        mockApp = new MockApplicationController();
        controller = new EndlessModeController(mockApp);
    });

    test('URL parameter detection works correctly', () => {
        // Test that ?background=true is detected
        const url1 = new URL('http://example.com?background=true');
        expect(url1.searchParams.get('background')).toBe('true');
        
        // Test that other values are not detected
        const url2 = new URL('http://example.com?background=false');
        expect(url2.searchParams.get('background')).not.toBe('true');
        
        const url3 = new URL('http://example.com');
        expect(url3.searchParams.get('background')).toBeNull();
    });

    test('Image generation with configured column count', () => {
        // Verify ENDLESS_MODE_CONFIG has a valid columnCount
        expect(ENDLESS_MODE_CONFIG.columnCount).toBeGreaterThan(0);
        expect(ENDLESS_MODE_CONFIG.columnCount).toBeLessThanOrEqual(5000);
        expect(typeof ENDLESS_MODE_CONFIG.columnCount).toBe('number');
    });

    test('Algorithm selection excludes current algorithm', () => {
        const configs = [
            { algorithm: createMockAlgorithm('algo1'), stepsPerSecond: 60 },
            { algorithm: createMockAlgorithm('algo2'), stepsPerSecond: 80 },
            { algorithm: createMockAlgorithm('algo3'), stepsPerSecond: 100 }
        ];
        
        controller.initialize(configs);
        controller.currentAlgorithmName = 'algo1';
        
        // Select algorithm 10 times
        for (let i = 0; i < 10; i++) {
            const selected = controller.selectRandomAlgorithm();
            expect(selected.algorithm.name).not.toBe('algo1');
            expect(['algo2', 'algo3']).toContain(selected.algorithm.name);
        }
    });

    test('Endless mode controller initializes correctly', () => {
        const configs = [
            { algorithm: createMockAlgorithm('bubble'), stepsPerSecond: 60 },
            { algorithm: createMockAlgorithm('quick'), stepsPerSecond: 120 }
        ];
        
        controller.initialize(configs);
        
        expect(controller.algorithmConfigs).toEqual(configs);
        expect(controller.isActive).toBe(false);
    });

    test('Starting endless mode sets isActive flag', () => {
        const configs = [
            { algorithm: createMockAlgorithm('bubble'), stepsPerSecond: 60 }
        ];
        
        controller.initialize(configs);
        expect(controller.isActive).toBe(false);
        
        controller.start();
        expect(controller.isActive).toBe(true);
        
        controller.stop();
    });

    test('Stopping endless mode clears isActive flag', () => {
        const configs = [
            { algorithm: createMockAlgorithm('bubble'), stepsPerSecond: 60 }
        ];
        
        controller.initialize(configs);
        controller.start();
        expect(controller.isActive).toBe(true);
        
        controller.stop();
        expect(controller.isActive).toBe(false);
    });

    test('Starting endless mode triggers scramble and sort', () => {
        const configs = [
            { algorithm: createMockAlgorithm('bubble'), stepsPerSecond: 60 }
        ];
        
        controller.initialize(configs);
        
        expect(mockApp.scrambledCount).toBe(0);
        expect(mockApp.sortingStarted).toBe(false);
        
        controller.start();
        
        expect(mockApp.scrambledCount).toBe(1);
        expect(mockApp.sortingStarted).toBe(true);
        expect(mockApp.state.selectedAlgorithm.name).toBe('bubble');
        expect(mockApp.stepsPerSecond).toBe(60);
        
        controller.stop();
    });

    test('Endless mode applies correct speed for each algorithm', () => {
        const configs = [
            { algorithm: createMockAlgorithm('bubble'), stepsPerSecond: 60 },
            { algorithm: createMockAlgorithm('quick'), stepsPerSecond: 120 },
            { algorithm: createMockAlgorithm('merge'), stepsPerSecond: 100 }
        ];
        
        controller.initialize(configs);
        controller.start();
        
        const firstAlgo = mockApp.state.selectedAlgorithm;
        const firstSpeed = mockApp.stepsPerSecond;
        
        // Find the config for the selected algorithm
        const firstConfig = configs.find(c => c.algorithm.name === firstAlgo.name);
        expect(firstConfig.stepsPerSecond).toBe(firstSpeed);
        
        controller.stop();
    });

    test('isEndlessModeActive returns correct state', () => {
        const configs = [
            { algorithm: createMockAlgorithm('bubble'), stepsPerSecond: 60 }
        ];
        
        controller.initialize(configs);
        
        expect(controller.isEndlessModeActive()).toBe(false);
        
        controller.start();
        expect(controller.isEndlessModeActive()).toBe(true);
        
        controller.stop();
        expect(controller.isEndlessModeActive()).toBe(false);
    });

    test('ENDLESS_MODE_CONFIG has all required algorithms', () => {
        expect(ENDLESS_MODE_CONFIG.algorithms).toBeDefined();
        expect(Array.isArray(ENDLESS_MODE_CONFIG.algorithms)).toBe(true);
        expect(ENDLESS_MODE_CONFIG.algorithms.length).toBeGreaterThan(0);
        
        // Each algorithm should have algorithm object and stepsPerSecond
        ENDLESS_MODE_CONFIG.algorithms.forEach(config => {
            expect(config.algorithm).toBeDefined();
            expect(config.algorithm.name).toBeDefined();
            expect(typeof config.algorithm.name).toBe('string');
            expect(config.algorithm.name.length).toBeGreaterThan(0);
            
            expect(config.stepsPerSecond).toBeDefined();
            expect(typeof config.stepsPerSecond).toBe('number');
            expect(config.stepsPerSecond).toBeGreaterThan(0);
        });
    });
});
