/**
 * @fileoverview Main application controller for the Image Column Sorter.
 * 
 * This module coordinates all other modules and manages the central application state.
 * It handles UI events, state transitions, and orchestrates the sorting visualization.
 */

import { loadImage, renderAllColumns, generateSyntheticImage } from './imageProcessor.js';
import { BubbleSort, SelectionSort, InsertionSort, QuickSort, MergeSort, RadixSort, HeapSort, ShellSort, CocktailShakerSort, CombSort, GnomeSort, CycleSort, OddEvenSort } from './sortingAlgorithms.js';
import { VisualizationEngine } from './visualizationEngine.js';
import { AudioGenerator } from './audioGenerator.js';
import { HighlightManager } from './highlightManager.js';
import { createIdentityOrder, scrambleColumns, copyColumnOrder } from './types.js';
import { EndlessModeController } from './endlessModeController.js';
import { ENDLESS_MODE_CONFIG } from './endless-config.js';

/**
 * ApplicationController class manages the entire application state and coordinates all modules.
 */
class ApplicationController {
    constructor() {
        // Get DOM elements
        this.canvas = document.getElementById('visualizationCanvas');
        this.imageUpload = document.getElementById('imageUpload');
        this.generateBtn = document.getElementById('generateBtn');
        this.generateWidth = document.getElementById('generateWidth');
        this.scrambleBtn = document.getElementById('scrambleBtn');
        this.algorithmSelect = document.getElementById('algorithmSelect');
        this.playPauseBtn = document.getElementById('playPauseBtn');
        this.playIcon = document.getElementById('playIcon');
        this.pauseIcon = document.getElementById('pauseIcon');
        this.stepsPerSecond = document.getElementById('stepsPerSecond');
        this.audioToggle = document.getElementById('audioToggle');
        this.audioOnIcon = document.getElementById('audioOnIcon');
        this.audioOffIcon = document.getElementById('audioOffIcon');
        this.algorithmInfo = document.getElementById('algorithmInfo');
        this.algorithmName = document.getElementById('algorithmName');
        this.algorithmDescription = document.getElementById('algorithmDescription');
        this.algorithmCharacteristics = document.getElementById('algorithmCharacteristics');
        this.errorDisplay = document.getElementById('errorDisplay');

        // Application state
        this.state = {
            sourceImage: null,
            imageWidth: 0,
            columnOrder: [],
            scrambledOrder: [],
            selectedAlgorithm: null,
            sortingSteps: [],
            audioEnabled: false,
            endlessMode: false
        };

        // Initialize modules
        this.highlightManager = new HighlightManager();
        this.audioGenerator = new AudioGenerator();
        this.visualizationEngine = null; // Will be created after image is loaded
        this.endlessModeController = new EndlessModeController(this);

        // Available algorithms
        this.algorithms = {
            'bubble': BubbleSort,
            'cocktail': CocktailShakerSort,
            'comb': CombSort,
            'selection': SelectionSort,
            'insertion': InsertionSort,
            'gnome': GnomeSort,
            'shell': ShellSort,
            'heap': HeapSort,
            'merge': MergeSort,
            'quick': QuickSort,
            'cycle': CycleSort,
            'oddeven': OddEvenSort,
            'radix': RadixSort
        };

        // Check for endless mode URL parameter
        // Accepts ?background or ?background=true
        const urlParams = new URLSearchParams(window.location.search);
        const backgroundParam = urlParams.get('background');
        const backgroundMode = backgroundParam !== null && backgroundParam !== 'false';

        // Initialize the application
        this._initialize();
        
        if (backgroundMode) {
            // Start endless mode
            this._initializeEndlessMode();
        } else {
            // Auto-initialize with 200-column image, scrambled, merge sort selected
            this._autoInitialize();
        }
    }

    /**
     * Initialize the application by setting up event handlers.
     * @private
     */
    _initialize() {
        // Wire up event handlers (will be implemented in next subtask)
        this._setupEventHandlers();

        // Set initial UI state
        this._updateUIState();

        console.log('Image Column Sorter initialized');
    }

    /**
     * Set up all event handlers for UI controls.
     * @private
     */
    _setupEventHandlers() {
        // Image upload
        this.imageUpload.addEventListener('change', (e) => this._handleImageUpload(e));

        // Generate button
        this.generateBtn.addEventListener('click', () => this._handleGenerate());

        // Scramble button
        this.scrambleBtn.addEventListener('click', () => this.scrambleColumns());

        // Algorithm selection
        this.algorithmSelect.addEventListener('change', (e) => this._handleAlgorithmSelect(e));

        // Play/Pause button
        this.playPauseBtn.addEventListener('click', () => this._handlePlayPause());

        // Steps per second input
        this.stepsPerSecond.addEventListener('input', (e) => this._handleStepsPerSecondChange(e));

        // Audio toggle
        this.audioToggle.addEventListener('click', () => this.toggleAudio());
    }

    /**
     * Handle image file upload.
     * @private
     * @param {Event} event - File input change event
     */
    async _handleImageUpload(event) {
        const file = event.target.files[0];
        if (!file) {
            return;
        }

        try {
            const image = await loadImage(file);
            await this.loadImageFromElement(image);
        } catch (error) {
            this._showError(`Failed to load image: ${error.message}`);
            console.error('Image load error:', error);
            // Reset file input so user can try again
            this.imageUpload.value = '';
        }
    }

    /**
     * Handle generate button click.
     * @private
     */
    async _handleGenerate() {
        const width = parseInt(this.generateWidth.value);
        
        if (isNaN(width) || width < 1) {
            this._showError('Please enter a width greater than 0');
            return;
        }

        if (width > 5000) {
            this._showError('Width too large. Please enter a value less than 5000 to avoid performance issues.');
            return;
        }

        try {
            const image = await generateSyntheticImage(width);
            await this.loadImageFromElement(image);
        } catch (error) {
            this._showError(`Failed to generate image: ${error.message}`);
            console.error('Image generation error:', error);
        }
    }

    /**
     * Handle algorithm selection change.
     * @private
     * @param {Event} event - Select change event
     */
    _handleAlgorithmSelect(event) {
        const algorithmKey = event.target.value;
        this.selectAlgorithm(algorithmKey);
    }

    /**
     * Handle play/pause button click.
     * @private
     */
    _handlePlayPause() {
        const isPlaying = this.visualizationEngine?.isPlayingState() || false;
        const isPaused = this.visualizationEngine?.isPausedState() || false;
        
        if (isPlaying && !isPaused) {
            // Currently playing, so pause
            this.pauseSorting();
        } else if (isPaused) {
            // Currently paused, so resume
            this.resumeSorting();
        } else {
            // Not started yet, so start
            this.startSorting();
        }
    }

    /**
     * Auto-initialize the application with a generated image.
     * @private
     */
    async _autoInitialize() {
        try {
            // Generate 200-column image
            const image = await generateSyntheticImage(200);
            await this.loadImageFromElement(image);
            
            // Scramble it
            this.scrambleColumns();
            
            // Select quick sort
            this.algorithmSelect.value = 'quick';
            this.selectAlgorithm('quick');
            
            console.log('Auto-initialized with 200-column image, scrambled, quick sort selected');
        } catch (error) {
            console.error('Auto-initialization failed:', error);
            // Don't show error to user, just log it
        }
    }

    /**
     * Handle steps per second input change.
     * @private
     * @param {Event} event - Input change event
     */
    _handleStepsPerSecondChange(event) {
        const stepsPerSec = parseInt(event.target.value);
        this.setStepsPerSecond(stepsPerSec);
    }

    /**
     * Load an image element and prepare it for visualization.
     * @param {HTMLImageElement} image - Image element to load
     */
    async loadImageFromElement(image) {
        try {
            // Stop any ongoing visualization
            if (this.visualizationEngine) {
                this.visualizationEngine.stop();
            }

            // Clear highlights
            this.highlightManager.clearHighlights();

            // Update state - completely reset
            this.state.sourceImage = image;
            this.state.imageWidth = image.width;
            this.state.columnOrder = createIdentityOrder(image.width);
            this.state.scrambledOrder = [];
            this.state.sortingSteps = [];

            // Create NEW visualization engine with the loaded image
            this.visualizationEngine = new VisualizationEngine(
                this.canvas,
                image,
                this.highlightManager,
                this.audioGenerator
            );

            // Set up visualization callbacks
            this.visualizationEngine.onComplete = () => this._onSortingComplete();

            // Restore steps per second setting if it was previously set
            const currentStepsPerSec = parseInt(this.stepsPerSecond.value);
            if (!isNaN(currentStepsPerSec) && currentStepsPerSec > 0) {
                this.visualizationEngine.setStepsPerSecond(currentStepsPerSec);
            }

            // Render the image
            renderAllColumns(this.canvas, image, this.state.columnOrder);

            // Scale canvas display to fit container
            this._scaleCanvasDisplay();

            // Hide stats display
            const statsInfo = document.getElementById('statsInfo');
            if (statsInfo) {
                statsInfo.style.display = 'none';
            }

            // Update UI state
            this._updateUIState();

            console.log(`Image loaded: ${image.width}x${image.height} (${image.width} columns)`);
        } catch (error) {
            this._showError(`Failed to display image: ${error.message}`);
            console.error('Image display error:', error);
            
            // Reset state on failure
            this.state.sourceImage = null;
            this.state.imageWidth = 0;
            this.state.columnOrder = [];
            this.state.scrambledOrder = [];
            this.state.sortingSteps = [];
            this._updateUIState();
        }
    }

    /**
     * Scales the canvas display using CSS to fit within the container.
     * Does not affect the actual canvas resolution or sorting data.
     * @private
     */
    _scaleCanvasDisplay() {
        if (!this.state.sourceImage) {
            return;
        }

        const canvasWidth = this.canvas.width;
        const canvasHeight = this.canvas.height;
        
        // Container max dimensions
        const maxDisplayWidth = 1200;
        const maxDisplayHeight = 600;
        const minDisplaySize = 400; // Minimum display size for smallest dimension

        // Calculate scale factor
        let scale = 1;
        const smallestDimension = Math.min(canvasWidth, canvasHeight);
        
        // Scale up small images to be more visible
        if (smallestDimension < minDisplaySize) {
            scale = minDisplaySize / smallestDimension;
        }
        
        // Scale down large images to fit in container
        const scaledWidth = canvasWidth * scale;
        const scaledHeight = canvasHeight * scale;
        
        if (scaledWidth > maxDisplayWidth || scaledHeight > maxDisplayHeight) {
            const widthRatio = maxDisplayWidth / scaledWidth;
            const heightRatio = maxDisplayHeight / scaledHeight;
            scale *= Math.min(widthRatio, heightRatio);
        }

        // Apply CSS to scale the canvas display
        this.canvas.style.width = `${Math.floor(canvasWidth * scale)}px`;
        this.canvas.style.height = `${Math.floor(canvasHeight * scale)}px`;
    }

    /**
     * Scramble the image columns randomly.
     */
    scrambleColumns() {
        if (!this.state.sourceImage) {
            return;
        }

        try {
            // Stop any ongoing visualization and fully reset state
            if (this.visualizationEngine) {
                this.visualizationEngine.stop();
                // Reset all visualization engine state
                this.visualizationEngine.isPlaying = false;
                this.visualizationEngine.isPaused = false;
                this.visualizationEngine.currentStepIndex = 0;
                this.visualizationEngine.compareCount = 0;
                this.visualizationEngine.swapCount = 0;
                this.visualizationEngine.steps = [];
                this.visualizationEngine.measurementWindowStart = performance.now();
                this.visualizationEngine.stepsExecutedInWindow = 0;
                this.visualizationEngine.actualStepsPerSecond = 0;
            }

            // Generate scrambled order
            const scrambled = scrambleColumns(this.state.imageWidth);
            this.state.columnOrder = scrambled;
            this.state.scrambledOrder = copyColumnOrder(scrambled);

            // Clear any existing sorting steps
            this.state.sortingSteps = [];

            // Render scrambled image
            renderAllColumns(this.canvas, this.state.sourceImage, this.state.columnOrder);

            // Clear highlights
            this.highlightManager.clearHighlights();
            
            // Hide stats display
            const statsInfo = document.getElementById('statsInfo');
            if (statsInfo) {
                statsInfo.style.display = 'none';
            }

            // Update UI state
            this._updateUIState();

            console.log('Columns scrambled');
        } catch (error) {
            this._showError(`Failed to scramble image: ${error.message}`);
            console.error('Scramble error:', error);
        }
    }

    /**
     * Select a sorting algorithm.
     * @param {string} algorithmKey - Key of the algorithm to select
     */
    selectAlgorithm(algorithmKey) {
        // Stop any ongoing visualization when changing algorithms
        if (this.visualizationEngine && this.visualizationEngine.isPlayingState()) {
            this.visualizationEngine.stop();
            
            // Reset visualization state
            this.visualizationEngine.isPlaying = false;
            this.visualizationEngine.isPaused = false;
            this.visualizationEngine.currentStepIndex = 0;
            
            // Clear highlights
            this.highlightManager.clearHighlights();
            
            // Re-render the current state
            if (this.state.sourceImage && this.state.columnOrder.length > 0) {
                renderAllColumns(this.canvas, this.state.sourceImage, this.state.columnOrder);
            }
        }

        if (!algorithmKey) {
            this.state.selectedAlgorithm = null;
            this.algorithmInfo.style.display = 'none';
            this._updateUIState();
            return;
        }

        const algorithm = this.algorithms[algorithmKey];
        if (!algorithm) {
            console.error('Unknown algorithm:', algorithmKey);
            return;
        }

        this.state.selectedAlgorithm = algorithm;

        // Display algorithm information
        this.algorithmName.textContent = algorithm.name;
        this.algorithmDescription.textContent = algorithm.description;
        
        // Display algorithm characteristics if available
        if (algorithm.characteristics && this.algorithmCharacteristics) {
            const chars = algorithm.characteristics;
            const charText = [
                `⏱️ Time: ${chars.timeComplexity}`,
                `💾 Space: ${chars.spaceComplexity}`,
                `${chars.stable ? '✓' : '✗'} Stable`,
                `📊 Best for: ${chars.bestFor}`
            ].join(' • ');
            this.algorithmCharacteristics.textContent = charText;
        }
        
        this.algorithmInfo.style.display = 'block';

        // Update UI state
        this._updateUIState();

        console.log('Algorithm selected:', algorithm.name);
    }

    /**
     * Start the sorting visualization.
     */
    startSorting() {
        if (!this.state.selectedAlgorithm || this.state.scrambledOrder.length === 0) {
            return;
        }

        try {
            // Generate sorting steps by running the algorithm on a copy of the column order
            const orderCopy = copyColumnOrder(this.state.columnOrder);
            this.state.sortingSteps = this.state.selectedAlgorithm.sort(orderCopy);

            // Start visualization
            this.visualizationEngine.start(
                this.state.sortingSteps,
                this.state.columnOrder,
                this.state.scrambledOrder
            );

            // Update UI state
            this._updateUIState();

            console.log('Sorting started:', this.state.selectedAlgorithm.name);
        } catch (error) {
            this._showError(`Failed to start sorting: ${error.message}`);
            console.error('Sorting start error:', error);
        }
    }

    /**
     * Pause the sorting visualization.
     */
    pauseSorting() {
        if (!this.visualizationEngine) {
            return;
        }

        this.visualizationEngine.pause();

        // Update UI state
        this._updateUIState();

        console.log('Sorting paused');
    }

    /**
     * Resume the sorting visualization.
     */
    resumeSorting() {
        if (!this.visualizationEngine) {
            return;
        }

        this.visualizationEngine.resume();

        // Update UI state
        this._updateUIState();

        console.log('Sorting resumed');
    }

    /**
     * Reset the visualization to the scrambled state.
     */
    resetSorting() {
        if (!this.visualizationEngine || this.state.scrambledOrder.length === 0) {
            return;
        }

        try {
            // Reset visualization engine
            const restoredOrder = this.visualizationEngine.reset();

            // Update state
            this.state.columnOrder = restoredOrder;

            // Re-render the scrambled image
            renderAllColumns(this.canvas, this.state.sourceImage, this.state.columnOrder);

            // Update UI state
            this._updateUIState();

            console.log('Visualization reset to scrambled state');
        } catch (error) {
            this._showError(`Failed to reset visualization: ${error.message}`);
            console.error('Reset error:', error);
        }
    }

    /**
     * Set the visualization speed in steps per second.
     * @param {number} stepsPerSec - Target steps per second
     */
    setStepsPerSecond(stepsPerSec) {
        if (isNaN(stepsPerSec) || stepsPerSec < 1) {
            stepsPerSec = 1;
        }
        if (stepsPerSec > 10000) {
            stepsPerSec = 10000;
        }

        if (this.visualizationEngine) {
            this.visualizationEngine.setStepsPerSecond(stepsPerSec);
            
            // Reset timing measurements when speed changes
            this.visualizationEngine.measurementWindowStart = performance.now();
            this.visualizationEngine.stepsExecutedInWindow = 0;
            this.visualizationEngine.actualStepsPerSecond = 0;
        }

        console.log(`Speed set to ${stepsPerSec} steps/sec`);
    }

    /**
     * Toggle audio feedback on/off.
     */
    toggleAudio() {
        this.state.audioEnabled = !this.state.audioEnabled;
        const success = this.audioGenerator.setEnabled(this.state.audioEnabled);

        // If audio failed to enable, revert state
        if (this.state.audioEnabled && !success) {
            this.state.audioEnabled = false;
            this._showError('Audio could not be enabled. Your browser may not support Web Audio API.');
        }

        // Update audio icon
        if (this.state.audioEnabled) {
            this.audioOnIcon.style.display = 'block';
            this.audioOffIcon.style.display = 'none';
        } else {
            this.audioOnIcon.style.display = 'none';
            this.audioOffIcon.style.display = 'block';
        }

        console.log('Audio:', this.state.audioEnabled ? 'enabled' : 'disabled');
    }

    /**
     * Called when sorting visualization completes.
     * @private
     */
    _onSortingComplete() {
        console.log('Sorting complete!');

        // If in endless mode, trigger the next cycle
        if (this.state.endlessMode && this.endlessModeController.isEndlessModeActive()) {
            this.endlessModeController.onSortComplete();
        }

        // Update UI state
        this._updateUIState();
    }
    
    /**
     * Initializes endless background mode.
     * @private
     */
    async _initializeEndlessMode() {
        try {
            console.log('Initializing endless mode...');
            
            // Set endless mode flag
            this.state.endlessMode = true;
            
            // Hide UI controls
            this._hideUIControls();
            
            // Enter fullscreen mode
            await this._enterFullscreen();
            
            // Generate square image with configured column count (same as regular generate)
            const image = await generateSyntheticImage(ENDLESS_MODE_CONFIG.columnCount);
            await this.loadImageFromElement(image);
            
            // Scale canvas to fill entire screen
            this._scaleCanvasToFullscreen();
            
            // Scramble the image
            this.scrambleColumns();
            
            // Initialize endless mode controller with algorithm configs
            this.endlessModeController.initialize(ENDLESS_MODE_CONFIG.algorithms);
            
            // Keep audio disabled in endless mode
            // (audio is disabled by default, so no action needed)
            
            // Start endless mode
            this.endlessModeController.start();
            
            console.log('Endless mode initialized successfully');
        } catch (error) {
            console.error('Failed to initialize endless mode:', error);
            this._showError(`Failed to start endless mode: ${error.message}`);
        }
    }
    
    /**
     * Hides UI controls for endless mode.
     * @private
     */
    _hideUIControls() {
        const controls = document.querySelector('.controls');
        const subtitle = document.querySelector('.subtitle');
        const title = document.querySelector('h1');
        
        if (controls) {
            controls.style.display = 'none';
        }
        if (subtitle) {
            subtitle.style.display = 'none';
        }
        if (title) {
            title.style.display = 'none';
        }
        
        // Hide algorithm info and stats
        if (this.algorithmInfo) {
            this.algorithmInfo.style.display = 'none';
        }
        const statsInfo = document.getElementById('statsInfo');
        if (statsInfo) {
            statsInfo.style.display = 'none';
            // Mark as hidden by endless mode so visualization engine won't show it
            statsInfo.dataset.endlessHidden = 'true';
        }
        
        // Make container take full screen
        const container = document.querySelector('.container');
        if (container) {
            container.style.padding = '0';
            container.style.margin = '0';
            container.style.maxWidth = '100%';
            container.style.height = '100vh';
            container.style.borderRadius = '0';
            container.style.background = '#0f172a';
        }
        
        // Make canvas container take full space
        const canvasContainer = document.querySelector('.canvas-container');
        if (canvasContainer) {
            canvasContainer.style.minHeight = '100vh';
            canvasContainer.style.maxHeight = '100vh';
            canvasContainer.style.border = 'none';
            canvasContainer.style.borderRadius = '0';
            canvasContainer.style.padding = '0';
            canvasContainer.style.margin = '0';
        }
        
        // Make body take full screen
        document.body.style.padding = '0';
        document.body.style.margin = '0';
        document.body.style.overflow = 'hidden';
    }
    
    /**
     * Enters fullscreen mode.
     * @private
     * @returns {Promise<void>}
     */
    async _enterFullscreen() {
        try {
            const elem = document.documentElement;
            
            if (elem.requestFullscreen) {
                await elem.requestFullscreen();
            } else if (elem.webkitRequestFullscreen) {
                await elem.webkitRequestFullscreen();
            } else if (elem.mozRequestFullScreen) {
                await elem.mozRequestFullScreen();
            } else if (elem.msRequestFullscreen) {
                await elem.msRequestFullscreen();
            } else {
                console.warn('Fullscreen API not supported');
            }
        } catch (error) {
            console.warn('Could not enter fullscreen:', error);
            // Continue anyway - fullscreen is nice to have but not required
        }
    }
    
    /**
     * Scales canvas to fullscreen dimensions.
     * Stretches to fill entire screen in endless mode.
     * @private
     */
    _scaleCanvasToFullscreen() {
        if (!this.state.sourceImage) {
            return;
        }
        
        const viewportWidth = window.innerWidth;
        const viewportHeight = window.innerHeight;
        
        // Fill entire screen (allow stretching)
        this.canvas.style.width = `${viewportWidth}px`;
        this.canvas.style.height = `${viewportHeight}px`;
    }

    /**
     * Display an error message to the user.
     * @private
     * @param {string} message - Error message to display
     */
    _showError(message) {
        if (this.errorDisplay) {
            this.errorDisplay.textContent = message;
            this.errorDisplay.style.display = 'block';
            
            // Auto-hide after 5 seconds
            setTimeout(() => {
                this._hideError();
            }, 5000);
        } else {
            // Fallback to alert if error display element not found
            alert(message);
        }
    }

    /**
     * Hide the error message display.
     * @private
     */
    _hideError() {
        if (this.errorDisplay) {
            this.errorDisplay.style.display = 'none';
        }
    }

    /**
     * Update the enabled/disabled state of UI controls based on application state.
     * @private
     */
    _updateUIState() {
        const hasImage = this.state.sourceImage !== null;
        const isScrambled = this.state.scrambledOrder.length > 0;
        const hasAlgorithm = this.state.selectedAlgorithm !== null;
        const isPlaying = this.visualizationEngine?.isPlayingState() || false;
        const isPaused = this.visualizationEngine?.isPausedState() || false;

        // Scramble button: enabled when image is loaded and not playing
        this.scrambleBtn.disabled = !hasImage || (isPlaying && !isPaused);

        // Algorithm select: enabled when image is loaded and not playing
        this.algorithmSelect.disabled = !hasImage || (isPlaying && !isPaused);

        // Play/Pause button: enabled when scrambled and algorithm selected
        this.playPauseBtn.disabled = !isScrambled || !hasAlgorithm;
        
        // Update play/pause icon visibility
        if (isPlaying && !isPaused) {
            // Show pause icon
            this.playIcon.style.display = 'none';
            this.pauseIcon.style.display = 'block';
        } else {
            // Show play icon
            this.playIcon.style.display = 'block';
            this.pauseIcon.style.display = 'none';
        }

        // Steps per second input: always enabled (can adjust during playback)
        this.stepsPerSecond.disabled = false;

        // Audio toggle: always enabled
        this.audioToggle.disabled = false;
        
        // Update audio icon based on state
        if (this.audioOnIcon && this.audioOffIcon) {
            if (this.state.audioEnabled) {
                this.audioOnIcon.style.display = 'block';
                this.audioOffIcon.style.display = 'none';
            } else {
                this.audioOnIcon.style.display = 'none';
                this.audioOffIcon.style.display = 'block';
            }
        }
    }
}

// Global error handler for unhandled errors
window.addEventListener('error', (event) => {
    console.error('Unhandled error:', event.error);
    // Don't show error display for every unhandled error to avoid spam
    // Just log it for debugging
});

// Global handler for unhandled promise rejections
window.addEventListener('unhandledrejection', (event) => {
    console.error('Unhandled promise rejection:', event.reason);
    // Don't show error display for every rejection to avoid spam
    // Just log it for debugging
});

// Initialize the application when DOM is ready
if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', () => {
        try {
            new ApplicationController();
        } catch (error) {
            console.error('Failed to initialize application:', error);
            alert('Failed to initialize the application. Please refresh the page and try again.');
        }
    });
} else {
    try {
        new ApplicationController();
    } catch (error) {
        console.error('Failed to initialize application:', error);
        alert('Failed to initialize the application. Please refresh the page and try again.');
    }
}
