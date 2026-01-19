/**
 * Application Controller - Manages state and coordinates UI with WebGL renderer
 */
import { WebGLRenderer } from './webgl-renderer.js';

class App {
  constructor() {
    this.state = {
      originalImage: null,
      paletteSize: 16,
      isRunning: false,
      animationFrameId: null
    };
    
    this.renderer = null;
    this.canvas = null;
    
    // UI elements
    this.imageUpload = null;
    this.paletteSlider = null;
    this.paletteValue = null;
    this.ditherOrderedBtn = null;
    this.ditherNearestBtn = null;
    this.playPauseBtn = null;
    this.playIcon = null;
    this.pauseIcon = null;
    this.stepBtn = null;
    this.resetBtn = null;
    this.errorMessage = null;
    this.warningMessage = null;
    this.canvasPlaceholder = null;
  }
  
  /**
   * Initialize the application
   */
  async init() {
    try {
      // Get UI elements
      this.canvas = document.getElementById('mainCanvas');
      this.imageUpload = document.getElementById('imageUpload');
      this.paletteSlider = document.getElementById('paletteSlider');
      this.paletteValue = document.getElementById('paletteValue');
      this.ditherOrderedBtn = document.getElementById('ditherOrderedBtn');
      this.ditherNearestBtn = document.getElementById('ditherNearestBtn');
      this.playPauseBtn = document.getElementById('playPauseBtn');
      this.playIcon = this.playPauseBtn.querySelector('.play-icon');
      this.pauseIcon = this.playPauseBtn.querySelector('.pause-icon');
      this.stepBtn = document.getElementById('stepBtn');
      this.resetBtn = document.getElementById('resetBtn');
      this.errorMessage = document.getElementById('errorMessage');
      this.warningMessage = document.getElementById('warningMessage');
      this.canvasPlaceholder = document.querySelector('.canvas-placeholder');
      
      // Initialize WebGL renderer
      this.renderer = new WebGLRenderer(this.canvas);
      
      // Load shaders
      await this.renderer.initDitherShader();
      await this.renderer.initAutomatonShader();
      await this.renderer.initCopyShader();
      
      // Set up event listeners
      this.setupEventListeners();
      
      console.log('Application initialized successfully');
    } catch (error) {
      this.showError(`Initialization failed: ${error.message}`);
      console.error('Initialization error:', error);
    }
  }
  
  /**
   * Set up event listeners for UI controls
   */
  setupEventListeners() {
    this.imageUpload.addEventListener('change', (e) => {
      if (e.target.files.length > 0) {
        this.handleImageUpload(e.target.files[0]);
      }
    });
    
    this.paletteSlider.addEventListener('input', (e) => {
      this.updatePaletteSize(parseInt(e.target.value));
    });
    
    this.ditherOrderedBtn.addEventListener('click', () => {
      this.applyDither(0); // 0 = ordered dithering
    });
    
    this.ditherNearestBtn.addEventListener('click', () => {
      this.applyDither(1); // 1 = nearest-neighbor
    });
    
    this.playPauseBtn.addEventListener('click', () => {
      this.togglePlayPause();
    });
    
    this.stepBtn.addEventListener('click', () => {
      this.stepAutomaton();
    });
    
    this.resetBtn.addEventListener('click', () => {
      this.resetCanvas();
    });
  }
  
  /**
   * Handle image upload
   */
  async handleImageUpload(file) {
    try {
      this.hideError();
      this.hideWarning();
      
      // Stop automaton if running
      if (this.state.isRunning) {
        this.pauseAutomaton();
      }
      
      // Validate file type
      const validTypes = ['image/png', 'image/jpeg', 'image/jpg', 'image/gif', 'image/webp'];
      if (!validTypes.includes(file.type)) {
        this.showError('Invalid file type. Please upload PNG, JPG, GIF, or WebP images.');
        return;
      }
      
      // Load the image
      const image = await this.loadImage(file);
      
      // Check image dimensions
      if (image.width > 4096 || image.height > 4096) {
        this.showError('Image is too large (max 4096x4096 pixels). Please use a smaller image.');
        return;
      }
      
      if (image.width > 2048 || image.height > 2048) {
        this.showWarning('Large image detected. Performance may be affected.');
      }
      
      // Initialize canvas with the image
      await this.initializeCanvas(image);
      
      // Hide placeholder
      if (this.canvasPlaceholder) {
        this.canvasPlaceholder.style.display = 'none';
      }
      
      // Reset all controls to initial state
      this.resetControlsAfterUpload();
      
      console.log(`Image loaded: ${image.width}x${image.height}`);
    } catch (error) {
      this.showError(`Failed to load image: ${error.message}`);
      console.error('Image upload error:', error);
    }
  }
  
  /**
   * Load an image from a file
   */
  loadImage(file) {
    return new Promise((resolve, reject) => {
      const reader = new FileReader();
      
      reader.onload = (e) => {
        const img = new Image();
        
        img.onload = () => resolve(img);
        img.onerror = () => reject(new Error('Failed to decode image'));
        
        img.src = e.target.result;
      };
      
      reader.onerror = () => reject(new Error('Failed to read file'));
      reader.readAsDataURL(file);
    });
  }
  
  /**
   * Initialize canvas with an image
   */
  async initializeCanvas(image) {
    // Set canvas dimensions to match image exactly (pixel-perfect)
    this.canvas.width = image.width;
    this.canvas.height = image.height;
    
    // Create texture from image
    this.renderer.currentTexture = this.renderer.createTexture(image);
    
    // Set up ping-pong buffers
    this.renderer.setupPingPongBuffers(image.width, image.height);
    
    // Copy image to ping buffer using copy shader
    const gl = this.renderer.gl;
    gl.useProgram(this.renderer.copyProgram);
    
    const imageLocation = gl.getUniformLocation(this.renderer.copyProgram, 'u_image');
    const resolutionLocation = gl.getUniformLocation(this.renderer.copyProgram, 'u_resolution');
    
    gl.uniform1i(imageLocation, 0);
    gl.uniform2f(resolutionLocation, this.canvas.width, this.canvas.height);
    
    gl.activeTexture(gl.TEXTURE0);
    gl.bindTexture(gl.TEXTURE_2D, this.renderer.currentTexture);
    
    // Render to ping buffer
    gl.bindFramebuffer(gl.FRAMEBUFFER, this.renderer.pingFramebuffer);
    gl.viewport(0, 0, this.canvas.width, this.canvas.height);
    this.renderer.renderQuad(this.renderer.copyProgram);
    
    // Display on canvas
    gl.bindFramebuffer(gl.FRAMEBUFFER, null);
    gl.viewport(0, 0, this.canvas.width, this.canvas.height);
    gl.bindTexture(gl.TEXTURE_2D, this.renderer.pingTexture);
    this.renderer.renderQuad(this.renderer.copyProgram);
    
    // Store original image for reset
    this.state.originalImage = this.renderer.getCanvasImageData();
  }
  
  /**
   * Update palette size
   */
  updatePaletteSize(size) {
    // Clamp to valid range
    size = Math.max(2, Math.min(256, size));
    
    this.state.paletteSize = size;
    this.paletteValue.textContent = size;
    this.paletteSlider.value = size;
  }
  
  /**
   * Apply dithering to the current image
   * @param {number} mode - 0 for ordered dithering, 1 for nearest-neighbor
   */
  applyDither(mode = 0) {
    try {
      this.hideError();
      
      // Stop automaton if running
      if (this.state.isRunning) {
        this.pauseAutomaton();
      }
      
      // Execute dithering with specified mode
      this.renderer.executeDither(this.state.paletteSize, mode);
      
      // Enable automaton controls
      this.playPauseBtn.disabled = false;
      this.stepBtn.disabled = false;
      this.resetBtn.disabled = false;
      
      const modeStr = mode === 0 ? 'ordered' : 'nearest-neighbor';
      console.log(`${modeStr} dithering applied with palette size ${this.state.paletteSize}`);
    } catch (error) {
      this.showError(`Dithering failed: ${error.message}`);
      console.error('Dithering error:', error);
    }
  }
  
  /**
   * Toggle play/pause state
   */
  togglePlayPause() {
    if (this.state.isRunning) {
      this.pauseAutomaton();
    } else {
      this.startAutomaton();
    }
  }
  
  /**
   * Start the automaton animation
   */
  startAutomaton() {
    if (this.state.isRunning) return;
    
    this.state.isRunning = true;
    this.updatePlayPauseIcon();
    
    this.animationLoop();
    
    console.log('Automaton started');
  }
  
  /**
   * Pause the automaton animation
   */
  pauseAutomaton() {
    if (!this.state.isRunning) return;
    
    this.state.isRunning = false;
    this.updatePlayPauseIcon();
    
    if (this.state.animationFrameId !== null) {
      cancelAnimationFrame(this.state.animationFrameId);
      this.state.animationFrameId = null;
    }
    
    console.log('Automaton paused');
  }
  
  /**
   * Update play/pause button icon
   */
  updatePlayPauseIcon() {
    if (this.state.isRunning) {
      this.playIcon.style.display = 'none';
      this.pauseIcon.style.display = 'block';
    } else {
      this.playIcon.style.display = 'block';
      this.pauseIcon.style.display = 'none';
    }
  }
  
  /**
   * Execute a single step of the automaton
   */
  stepAutomaton() {
    try {
      this.hideError();
      
      // Pause if currently running
      if (this.state.isRunning) {
        this.pauseAutomaton();
      }
      
      // Execute one step
      this.renderer.executeAutomatonStep(this.state.paletteSize);
      
      console.log('Automaton step executed');
    } catch (error) {
      this.showError(`Step failed: ${error.message}`);
      console.error('Step error:', error);
    }
  }
  
  /**
   * Animation loop for the automaton
   */
  animationLoop() {
    if (!this.state.isRunning) return;
    
    try {
      // Execute one automaton step
      this.renderer.executeAutomatonStep(this.state.paletteSize);
      
      // Schedule next frame
      this.state.animationFrameId = requestAnimationFrame(() => this.animationLoop());
    } catch (error) {
      this.pauseAutomaton();
      this.showError(`Automaton error: ${error.message}`);
      console.error('Automaton error:', error);
    }
  }
  
  /**
   * Reset canvas to original image
   */
  resetCanvas() {
    try {
      this.hideError();
      
      // Stop automaton if running
      if (this.state.isRunning) {
        this.pauseAutomaton();
      }
      
      if (!this.state.originalImage) {
        console.warn('No original image to restore');
        return;
      }
      
      // Create texture from original image data
      const texture = this.renderer.createTexture(this.state.originalImage);
      
      // Copy to ping buffer
      const gl = this.renderer.gl;
      gl.useProgram(this.renderer.copyProgram);
      
      const imageLocation = gl.getUniformLocation(this.renderer.copyProgram, 'u_image');
      const resolutionLocation = gl.getUniformLocation(this.renderer.copyProgram, 'u_resolution');
      
      gl.uniform1i(imageLocation, 0);
      gl.uniform2f(resolutionLocation, this.canvas.width, this.canvas.height);
      
      gl.activeTexture(gl.TEXTURE0);
      gl.bindTexture(gl.TEXTURE_2D, texture);
      
      // Render to ping buffer
      gl.bindFramebuffer(gl.FRAMEBUFFER, this.renderer.pingFramebuffer);
      gl.viewport(0, 0, this.canvas.width, this.canvas.height);
      this.renderer.renderQuad(this.renderer.copyProgram);
      
      // Display on canvas
      gl.bindFramebuffer(gl.FRAMEBUFFER, null);
      gl.viewport(0, 0, this.canvas.width, this.canvas.height);
      gl.bindTexture(gl.TEXTURE_2D, this.renderer.pingTexture);
      this.renderer.renderQuad(this.renderer.copyProgram);
      
      // Clean up temporary texture
      gl.deleteTexture(texture);
      
      // Reset buffer state
      this.renderer.currentBuffer = 'ping';
      
      // Disable automaton controls
      this.playPauseBtn.disabled = true;
      this.stepBtn.disabled = true;
      
      // Reset play/pause icon to play state
      this.playIcon.style.display = 'block';
      this.pauseIcon.style.display = 'none';
      
      console.log('Canvas reset to original image');
    } catch (error) {
      this.showError(`Reset failed: ${error.message}`);
      console.error('Reset error:', error);
    }
  }
  
  /**
   * Enable controls after image upload
   */
  enableControls() {
    this.paletteSlider.disabled = false;
    this.ditherOrderedBtn.disabled = false;
    this.ditherNearestBtn.disabled = false;
  }
  
  /**
   * Reset controls to initial state after image upload
   */
  resetControlsAfterUpload() {
    // Enable dither controls
    this.paletteSlider.disabled = false;
    this.ditherOrderedBtn.disabled = false;
    this.ditherNearestBtn.disabled = false;
    
    // Disable automaton controls (require dithering first)
    this.playPauseBtn.disabled = true;
    this.stepBtn.disabled = true;
    this.resetBtn.disabled = true;
    
    // Reset play/pause icon to play state
    this.playIcon.style.display = 'block';
    this.pauseIcon.style.display = 'none';
    
    // Cancel any running animation
    if (this.state.animationFrameId !== null) {
      cancelAnimationFrame(this.state.animationFrameId);
      this.state.animationFrameId = null;
    }
    
    // Reset running state
    this.state.isRunning = false;
  }
  
  /**
   * Show error message
   */
  showError(message) {
    this.errorMessage.textContent = message;
    this.errorMessage.style.display = 'block';
  }
  
  /**
   * Hide error message
   */
  hideError() {
    this.errorMessage.style.display = 'none';
    this.errorMessage.textContent = '';
  }
  
  /**
   * Show warning message
   */
  showWarning(message) {
    this.warningMessage.textContent = message;
    this.warningMessage.style.display = 'block';
  }
  
  /**
   * Hide warning message
   */
  hideWarning() {
    this.warningMessage.style.display = 'none';
    this.warningMessage.textContent = '';
  }
}

// Initialize app when DOM is ready
document.addEventListener('DOMContentLoaded', () => {
  const app = new App();
  app.init();
});
