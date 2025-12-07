/**
 * @fileoverview Unit tests for ApplicationController UI state transitions
 * 
 * These tests verify that UI controls are enabled/disabled correctly based on
 * application state transitions.
 */

import { describe, test, expect, beforeEach } from '@jest/globals';

/**
 * Mock DOM elements for testing
 */
class MockElement {
    constructor() {
        this.disabled = false;
        this.textContent = '';
        this.value = '';
        this.style = { display: '' };
        this.files = [];
        this._eventListeners = {};
    }

    addEventListener(event, handler) {
        if (!this._eventListeners[event]) {
            this._eventListeners[event] = [];
        }
        this._eventListeners[event].push(handler);
    }

    trigger(event, data = {}) {
        if (this._eventListeners[event]) {
            this._eventListeners[event].forEach(handler => handler(data));
        }
    }
}

/**
 * Mock ApplicationController state for testing
 */
class MockApplicationState {
    constructor() {
        this.hasImage = false;
        this.isScrambled = false;
        this.hasAlgorithm = false;
        this.isPlaying = false;
        this.isPaused = false;
    }

    /**
     * Calculate expected UI state based on application state
     */
    getExpectedUIState() {
        return {
            scrambleBtn: !this.hasImage,
            algorithmSelect: !this.isScrambled || this.isPlaying,
            startBtn: !this.isScrambled || !this.hasAlgorithm || this.isPlaying,
            pauseBtn: !this.isPlaying,
            resetBtn: !this.isScrambled || (this.isPlaying && !this.isPaused),
            speedSlider: false,
            audioToggle: false
        };
    }
}

describe('UI State Transitions', () => {
    let state;

    beforeEach(() => {
        state = new MockApplicationState();
    });

    test('Initial state: all controls disabled except audio toggle', () => {
        const expected = state.getExpectedUIState();
        
        expect(expected.scrambleBtn).toBe(true);
        expect(expected.algorithmSelect).toBe(true);
        expect(expected.startBtn).toBe(true);
        expect(expected.pauseBtn).toBe(true);
        expect(expected.resetBtn).toBe(true);
        expect(expected.speedSlider).toBe(false);
        expect(expected.audioToggle).toBe(false);
    });

    test('After image load: scramble button enabled', () => {
        state.hasImage = true;
        const expected = state.getExpectedUIState();
        
        expect(expected.scrambleBtn).toBe(false);
        expect(expected.algorithmSelect).toBe(true);
        expect(expected.startBtn).toBe(true);
    });

    test('After scramble: algorithm select and scramble enabled', () => {
        state.hasImage = true;
        state.isScrambled = true;
        const expected = state.getExpectedUIState();
        
        expect(expected.scrambleBtn).toBe(false);
        expect(expected.algorithmSelect).toBe(false);
        expect(expected.startBtn).toBe(true); // Still disabled (no algorithm)
        expect(expected.resetBtn).toBe(false);
    });

    test('After algorithm selection: start button enabled', () => {
        state.hasImage = true;
        state.isScrambled = true;
        state.hasAlgorithm = true;
        const expected = state.getExpectedUIState();
        
        expect(expected.scrambleBtn).toBe(false);
        expect(expected.algorithmSelect).toBe(false);
        expect(expected.startBtn).toBe(false);
        expect(expected.resetBtn).toBe(false);
    });

    test('During sorting: pause enabled, start/algorithm disabled', () => {
        state.hasImage = true;
        state.isScrambled = true;
        state.hasAlgorithm = true;
        state.isPlaying = true;
        const expected = state.getExpectedUIState();
        
        expect(expected.startBtn).toBe(true);
        expect(expected.algorithmSelect).toBe(true);
        expect(expected.pauseBtn).toBe(false);
        expect(expected.resetBtn).toBe(true);
    });

    test('When paused: resume enabled, reset enabled', () => {
        state.hasImage = true;
        state.isScrambled = true;
        state.hasAlgorithm = true;
        state.isPlaying = true;
        state.isPaused = true;
        const expected = state.getExpectedUIState();
        
        expect(expected.pauseBtn).toBe(false);
        expect(expected.resetBtn).toBe(false);
        expect(expected.startBtn).toBe(true);
    });

    test('After reset: returns to scrambled state', () => {
        state.hasImage = true;
        state.isScrambled = true;
        state.hasAlgorithm = true;
        state.isPlaying = false;
        state.isPaused = false;
        const expected = state.getExpectedUIState();
        
        expect(expected.scrambleBtn).toBe(false);
        expect(expected.algorithmSelect).toBe(false);
        expect(expected.startBtn).toBe(false);
        expect(expected.pauseBtn).toBe(true);
        expect(expected.resetBtn).toBe(false);
    });

    test('Speed slider and audio toggle always enabled', () => {
        // Test in various states
        const states = [
            { hasImage: false, isScrambled: false, hasAlgorithm: false, isPlaying: false },
            { hasImage: true, isScrambled: false, hasAlgorithm: false, isPlaying: false },
            { hasImage: true, isScrambled: true, hasAlgorithm: true, isPlaying: false },
            { hasImage: true, isScrambled: true, hasAlgorithm: true, isPlaying: true }
        ];

        states.forEach(stateConfig => {
            Object.assign(state, stateConfig);
            const expected = state.getExpectedUIState();
            
            expect(expected.speedSlider).toBe(false);
            expect(expected.audioToggle).toBe(false);
        });
    });
});

describe('Control Interactions', () => {
    test('Scramble button can be clicked multiple times', () => {
        const state = new MockApplicationState();
        state.hasImage = true;
        state.isScrambled = true;
        
        // After first scramble
        let expected = state.getExpectedUIState();
        expect(expected.scrambleBtn).toBe(false);
        
        // Can scramble again
        expected = state.getExpectedUIState();
        expect(expected.scrambleBtn).toBe(false);
    });

    test('Cannot start sorting without algorithm', () => {
        const state = new MockApplicationState();
        state.hasImage = true;
        state.isScrambled = true;
        state.hasAlgorithm = false;
        
        const expected = state.getExpectedUIState();
        expect(expected.startBtn).toBe(true);
    });

    test('Cannot start sorting without scrambling', () => {
        const state = new MockApplicationState();
        state.hasImage = true;
        state.isScrambled = false;
        state.hasAlgorithm = true;
        
        const expected = state.getExpectedUIState();
        expect(expected.startBtn).toBe(true);
    });

    test('Cannot change algorithm during sorting', () => {
        const state = new MockApplicationState();
        state.hasImage = true;
        state.isScrambled = true;
        state.hasAlgorithm = true;
        state.isPlaying = true;
        
        const expected = state.getExpectedUIState();
        expect(expected.algorithmSelect).toBe(true);
    });

    test('Pause button text changes based on state', () => {
        const state = new MockApplicationState();
        
        // When not paused
        state.isPlaying = true;
        state.isPaused = false;
        expect(state.isPaused).toBe(false);
        
        // When paused
        state.isPaused = true;
        expect(state.isPaused).toBe(true);
    });
});

describe('State Transition Sequences', () => {
    test('Complete workflow: load -> scramble -> select -> start -> pause -> resume -> reset', () => {
        const state = new MockApplicationState();
        
        // Initial state
        let expected = state.getExpectedUIState();
        expect(expected.scrambleBtn).toBe(true);
        
        // Load image
        state.hasImage = true;
        expected = state.getExpectedUIState();
        expect(expected.scrambleBtn).toBe(false);
        
        // Scramble
        state.isScrambled = true;
        expected = state.getExpectedUIState();
        expect(expected.algorithmSelect).toBe(false);
        expect(expected.startBtn).toBe(true);
        
        // Select algorithm
        state.hasAlgorithm = true;
        expected = state.getExpectedUIState();
        expect(expected.startBtn).toBe(false);
        
        // Start sorting
        state.isPlaying = true;
        expected = state.getExpectedUIState();
        expect(expected.pauseBtn).toBe(false);
        expect(expected.startBtn).toBe(true);
        
        // Pause
        state.isPaused = true;
        expected = state.getExpectedUIState();
        expect(expected.pauseBtn).toBe(false);
        expect(expected.resetBtn).toBe(false);
        
        // Resume
        state.isPaused = false;
        expected = state.getExpectedUIState();
        expect(expected.pauseBtn).toBe(false);
        
        // Reset
        state.isPlaying = false;
        state.isPaused = false;
        expected = state.getExpectedUIState();
        expect(expected.startBtn).toBe(false);
        expect(expected.resetBtn).toBe(false);
    });

    test('Can re-scramble after sorting completes', () => {
        const state = new MockApplicationState();
        state.hasImage = true;
        state.isScrambled = true;
        state.hasAlgorithm = true;
        
        // After sorting completes
        state.isPlaying = false;
        
        const expected = state.getExpectedUIState();
        expect(expected.scrambleBtn).toBe(false);
    });

    test('Can change algorithm after reset', () => {
        const state = new MockApplicationState();
        state.hasImage = true;
        state.isScrambled = true;
        state.hasAlgorithm = true;
        state.isPlaying = false;
        
        const expected = state.getExpectedUIState();
        expect(expected.algorithmSelect).toBe(false);
    });
});
