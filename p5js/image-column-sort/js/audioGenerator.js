/**
 * @fileoverview AudioGenerator module for generating audio feedback during sorting operations
 * Uses Web Audio API to create tones corresponding to column positions
 */

/**
 * AudioGenerator class for managing audio feedback during sorting visualization
 */
export class AudioGenerator {
    constructor() {
        /** @type {AudioContext | null} */
        this.audioContext = null;
        
        /** @type {boolean} */
        this.enabled = false;
        
        /** @type {GainNode | null} */
        this.gainNode = null;
        
        /** @type {OscillatorNode | null} */
        this.currentOscillator = null;
        
        // Frequency range for audio feedback (in Hz)
        this.minFrequency = 200;  // Low frequency for first column
        this.maxFrequency = 3000;  // High frequency for last column
    }
    
    /**
     * Initialize the Web Audio API context
     * Handles browser autoplay policies by creating context on user interaction
     * @returns {boolean} True if initialization succeeded, false otherwise
     */
    initialize() {
        if (this.audioContext) {
            return true; // Already initialized
        }
        
        try {
            // Check if Web Audio API is supported
            if (!window.AudioContext && !window.webkitAudioContext) {
                console.warn('Web Audio API is not supported in this browser');
                return false;
            }

            // Create AudioContext (handles browser prefixes automatically in modern browsers)
            this.audioContext = new (window.AudioContext || window.webkitAudioContext)();
            
            // Create a gain node for volume control
            this.gainNode = this.audioContext.createGain();
            this.gainNode.connect(this.audioContext.destination);
            this.gainNode.gain.value = 0.1; // Set to 10% volume to avoid harsh sounds
            
            // Resume context if it's suspended (autoplay policy)
            if (this.audioContext.state === 'suspended') {
                this.audioContext.resume().catch(err => {
                    console.warn('Failed to resume audio context:', err);
                });
            }

            return true;
        } catch (error) {
            console.error('Failed to initialize Web Audio API:', error);
            this.audioContext = null;
            this.gainNode = null;
            return false;
        }
    }
    
    /**
     * Enable or disable audio feedback
     * @param {boolean} enabled - Whether audio should be enabled
     * @returns {boolean} True if audio was successfully enabled, false if it failed
     */
    setEnabled(enabled) {
        this.enabled = enabled;
        
        // Initialize audio context on first enable (handles autoplay policies)
        if (enabled && !this.audioContext) {
            const success = this.initialize();
            if (!success) {
                console.warn('Audio could not be enabled: initialization failed');
                this.enabled = false;
                return false;
            }
        }
        
        // Stop any currently playing sound when disabling
        if (!enabled) {
            this.stopCurrentTone();
        }

        return this.enabled;
    }
    
    /**
     * Stop the currently playing tone immediately
     */
    stopCurrentTone() {
        if (this.currentOscillator) {
            try {
                this.currentOscillator.stop();
            } catch (error) {
                // Oscillator may already be stopped, ignore error
            }
            this.currentOscillator = null;
        }
    }
    
    /**
     * Calculate frequency based on column position
     * Maps column indices to a frequency range linearly
     * @param {number} columnIndex - The index of the column (0-based)
     * @param {number} totalColumns - Total number of columns in the image
     * @returns {number} Frequency in Hz
     */
    calculateFrequency(columnIndex, totalColumns) {
        if (totalColumns <= 1) {
            return this.minFrequency;
        }
        
        // Linear mapping from column index to frequency range
        const ratio = columnIndex / (totalColumns - 1);
        return this.minFrequency + (ratio * (this.maxFrequency - this.minFrequency));
    }
    
    /**
     * Play a tone at the specified frequency for the given duration
     * @param {number} frequency - Frequency in Hz
     * @param {number} duration - Duration in milliseconds
     */
    playTone(frequency, duration) {
        if (!this.enabled || !this.audioContext || !this.gainNode) {
            return;
        }
        
        // Stop any currently playing tone
        this.stopCurrentTone();
        
        try {
            // Resume context if suspended (browser autoplay policy)
            if (this.audioContext.state === 'suspended') {
                this.audioContext.resume();
            }
            
            // Create oscillator for this tone
            const oscillator = this.audioContext.createOscillator();
            oscillator.type = 'sine'; // Smooth sine wave
            oscillator.frequency.value = frequency;
            
            // Connect oscillator to gain node
            oscillator.connect(this.gainNode);
            
            // Start the oscillator
            const now = this.audioContext.currentTime;
            oscillator.start(now);
            
            // Schedule stop time
            oscillator.stop(now + duration / 1000);
            
            // Track current oscillator
            this.currentOscillator = oscillator;
            
            // Clear reference when done
            oscillator.onended = () => {
                if (this.currentOscillator === oscillator) {
                    this.currentOscillator = null;
                }
            };
        } catch (error) {
            console.error('Failed to play tone:', error);
        }
    }
    
    /**
     * Check if audio is currently enabled
     * @returns {boolean}
     */
    isEnabled() {
        return this.enabled;
    }
}

// Export a singleton instance
export const audioGenerator = new AudioGenerator();
