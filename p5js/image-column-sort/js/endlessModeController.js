/**
 * @fileoverview Endless Mode Controller for continuous sorting visualization.
 * 
 * This module manages the endless background mode that continuously demonstrates
 * sorting algorithms by automatically rescrambling and selecting new algorithms
 * after each sort completes.
 */

/**
 * EndlessModeController class manages the endless visualization loop.
 */
export class EndlessModeController {
    /**
     * @param {Object} applicationController - Reference to the main application controller
     */
    constructor(applicationController) {
        this.app = applicationController;
        this.algorithmConfigs = [];
        this.currentAlgorithmName = null;
        this.isActive = false;
        this.waitTimeout = null;
    }
    
    /**
     * Initializes the endless mode with algorithm configurations.
     * 
     * @param {Array<{algorithm: Object, stepsPerSecond: number}>} algorithmConfigs - Array of algorithm configurations
     */
    initialize(algorithmConfigs) {
        this.algorithmConfigs = algorithmConfigs;
        console.log('Endless mode initialized with', algorithmConfigs.length, 'algorithms');
    }
    
    /**
     * Starts the endless mode loop.
     */
    start() {
        if (this.isActive) {
            console.warn('Endless mode already active');
            return;
        }
        
        this.isActive = true;
        console.log('Endless mode started');
        
        // Start the first cycle
        this._startNextCycle();
    }
    
    /**
     * Stops the endless mode loop.
     */
    stop() {
        this.isActive = false;
        
        // Clear any pending wait timeout
        if (this.waitTimeout) {
            clearTimeout(this.waitTimeout);
            this.waitTimeout = null;
        }
        
        // Hide the algorithm name display
        const displayElement = document.getElementById('endlessAlgorithmName');
        if (displayElement) {
            displayElement.style.display = 'none';
        }
        
        console.log('Endless mode stopped');
    }
    
    /**
     * Called when a sort completes. Triggers the next cycle.
     */
    onSortComplete() {
        if (!this.isActive) {
            return;
        }
        
        console.log('Sort complete in endless mode, waiting before next cycle...');
        
        // Wait briefly (1.5 seconds) to show the completed sorted image
        this.waitTimeout = setTimeout(() => {
            this.waitTimeout = null;
            
            if (this.isActive) {
                this._startNextCycle();
            }
        }, 1500);
    }
    
    /**
     * Starts the next cycle: rescramble, select new algorithm, and start sorting.
     * @private
     */
    _startNextCycle() {
        if (!this.isActive) {
            return;
        }
        
        // Rescramble the columns
        this.app.scrambleColumns();
        
        // Select a random algorithm (excluding the current one)
        const config = this.selectRandomAlgorithm();
        
        if (!config) {
            console.error('No algorithm configuration available');
            this.stop();
            return;
        }
        
        // Update current algorithm name
        this.currentAlgorithmName = config.algorithm.name;
        
        // Set the selected algorithm directly
        this.app.state.selectedAlgorithm = config.algorithm;
        
        // Update the endless mode algorithm name display
        this._updateAlgorithmDisplay(config.algorithm.name);
        
        // Set the speed for this algorithm
        this.app.setStepsPerSecond(config.stepsPerSecond);
        
        console.log(`Starting ${config.algorithm.name} at ${config.stepsPerSecond} steps/sec`);
        
        // Start sorting
        this.app.startSorting();
    }
    
    /**
     * Updates the algorithm name display in endless mode.
     * @private
     * @param {string} algorithmName - Name of the algorithm to display
     */
    _updateAlgorithmDisplay(algorithmName) {
        const displayElement = document.getElementById('endlessAlgorithmName');
        if (displayElement) {
            displayElement.textContent = algorithmName;
            displayElement.style.display = 'block';
        }
    }
    
    /**
     * Selects a random algorithm configuration, excluding the current one.
     * 
     * @returns {{algorithm: Object, stepsPerSecond: number}|null} Selected algorithm config or null if none available
     */
    selectRandomAlgorithm() {
        if (this.algorithmConfigs.length === 0) {
            return null;
        }
        
        // If only one algorithm, return it
        if (this.algorithmConfigs.length === 1) {
            return this.algorithmConfigs[0];
        }
        
        // Filter out the current algorithm to ensure variety
        const availableConfigs = this.algorithmConfigs.filter(
            config => config.algorithm.name !== this.currentAlgorithmName
        );
        
        // If no other algorithms available (shouldn't happen), use all
        const configs = availableConfigs.length > 0 ? availableConfigs : this.algorithmConfigs;
        
        // Select a random algorithm from available configs
        const randomIndex = Math.floor(Math.random() * configs.length);
        return configs[randomIndex];
    }
    
    /**
     * Checks if endless mode is currently active.
     * 
     * @returns {boolean} True if endless mode is active
     */
    isEndlessModeActive() {
        return this.isActive;
    }
}
