/**
 * WebGLRenderer - Handles all WebGL operations for dithering and automaton
 * Manages shaders, textures, framebuffers, and rendering operations
 */
export class WebGLRenderer {
  constructor(canvas) {
    this.canvas = canvas;
    this.gl = null;
    
    // Shader programs
    this.ditherProgram = null;
    this.automatonProgram = null;
    this.copyProgram = null;
    
    // Ping-pong buffers for automaton
    this.pingTexture = null;
    this.pongTexture = null;
    this.pingFramebuffer = null;
    this.pongFramebuffer = null;
    this.currentBuffer = 'ping';
    
    // Current working texture
    this.currentTexture = null;
    
    // Quad buffer for rendering
    this.quadBuffer = null;
    
    this.initWebGL();
  }
  
  /**
   * Initialize WebGL context with error checking
   */
  initWebGL() {
    const gl = this.canvas.getContext('webgl', {
      premultipliedAlpha: false,
      preserveDrawingBuffer: true
    });
    
    if (!gl) {
      throw new Error('WebGL not supported in this browser');
    }
    
    this.gl = gl;
    
    // Disable image smoothing for pixel-perfect rendering
    this.canvas.style.imageRendering = 'pixelated';
    this.canvas.style.imageRendering = 'crisp-edges';
  }
  
  /**
   * Load shader source code from external file
   * @param {string} url - Path to shader file
   * @returns {Promise<string>} Shader source code
   */
  async loadShader(url) {
    const response = await fetch(url);
    if (!response.ok) {
      throw new Error(`Failed to load shader: ${url}`);
    }
    return await response.text();
  }
  
  /**
   * Compile a shader from source code
   * @param {string} source - Shader source code
   * @param {GLenum} type - gl.VERTEX_SHADER or gl.FRAGMENT_SHADER
   * @returns {WebGLShader} Compiled shader
   */
  compileShader(source, type) {
    const gl = this.gl;
    const shader = gl.createShader(type);
    
    gl.shaderSource(shader, source);
    gl.compileShader(shader);
    
    if (!gl.getShaderParameter(shader, gl.COMPILE_STATUS)) {
      const info = gl.getShaderInfoLog(shader);
      gl.deleteShader(shader);
      throw new Error(`Shader compilation failed: ${info}`);
    }
    
    return shader;
  }
  
  /**
   * Create a shader program from vertex and fragment shader sources
   * @param {string} vertexSource - Vertex shader source code
   * @param {string} fragmentSource - Fragment shader source code
   * @returns {WebGLProgram} Linked shader program
   */
  createProgram(vertexSource, fragmentSource) {
    const gl = this.gl;
    
    const vertexShader = this.compileShader(vertexSource, gl.VERTEX_SHADER);
    const fragmentShader = this.compileShader(fragmentSource, gl.FRAGMENT_SHADER);
    
    const program = gl.createProgram();
    gl.attachShader(program, vertexShader);
    gl.attachShader(program, fragmentShader);
    gl.linkProgram(program);
    
    if (!gl.getProgramParameter(program, gl.LINK_STATUS)) {
      const info = gl.getProgramInfoLog(program);
      gl.deleteProgram(program);
      throw new Error(`Program linking failed: ${info}`);
    }
    
    // Clean up shaders after linking
    gl.deleteShader(vertexShader);
    gl.deleteShader(fragmentShader);
    
    return program;
  }
  
  /**
   * Initialize the dithering shader program
   */
  async initDitherShader() {
    const vertexSource = await this.loadShader('shaders/vertex.vert');
    const fragmentSource = await this.loadShader('shaders/dither.frag');
    this.ditherProgram = this.createProgram(vertexSource, fragmentSource);
    this.setupQuadBuffer(this.ditherProgram);
  }
  
  /**
   * Initialize the automaton shader program
   */
  async initAutomatonShader() {
    const vertexSource = await this.loadShader('shaders/vertex.vert');
    const fragmentSource = await this.loadShader('shaders/automaton.frag');
    this.automatonProgram = this.createProgram(vertexSource, fragmentSource);
    this.setupQuadBuffer(this.automatonProgram);
  }
  
  /**
   * Initialize the copy shader program
   */
  async initCopyShader() {
    const vertexSource = await this.loadShader('shaders/vertex.vert');
    const fragmentSource = await this.loadShader('shaders/copy.frag');
    this.copyProgram = this.createProgram(vertexSource, fragmentSource);
    this.setupQuadBuffer(this.copyProgram);
  }
  
  /**
   * Set up vertex buffer for rendering a full-screen quad
   * @param {WebGLProgram} program - Shader program to set up buffer for
   */
  setupQuadBuffer(program) {
    const gl = this.gl;
    
    // Create buffer if it doesn't exist
    if (!this.quadBuffer) {
      this.quadBuffer = gl.createBuffer();
      
      // Full-screen quad vertices (two triangles)
      const vertices = new Float32Array([
        -1, -1,  // Bottom-left
         1, -1,  // Bottom-right
        -1,  1,  // Top-left
         1,  1   // Top-right
      ]);
      
      gl.bindBuffer(gl.ARRAY_BUFFER, this.quadBuffer);
      gl.bufferData(gl.ARRAY_BUFFER, vertices, gl.STATIC_DRAW);
    }
    
    // Set up position attribute
    const positionLocation = gl.getAttribLocation(program, 'a_position');
    if (positionLocation !== -1) {
      gl.bindBuffer(gl.ARRAY_BUFFER, this.quadBuffer);
      gl.enableVertexAttribArray(positionLocation);
      gl.vertexAttribPointer(positionLocation, 2, gl.FLOAT, false, 0, 0);
    }
  }
  
  /**
   * Create a WebGL texture from an image or ImageData
   * @param {HTMLImageElement|ImageData} source - Image source
   * @returns {WebGLTexture} Created texture
   */
  createTexture(source) {
    const gl = this.gl;
    const texture = gl.createTexture();
    
    gl.bindTexture(gl.TEXTURE_2D, texture);
    
    // Set texture parameters for pixel-perfect rendering
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.NEAREST);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.NEAREST);
    
    // Flip Y axis for images to match canvas coordinate system
    gl.pixelStorei(gl.UNPACK_FLIP_Y_WEBGL, true);
    
    // Upload image data
    if (source instanceof HTMLImageElement) {
      gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA, gl.RGBA, gl.UNSIGNED_BYTE, source);
    } else if (source instanceof ImageData) {
      gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA, gl.RGBA, gl.UNSIGNED_BYTE, source);
    } else {
      throw new Error('Invalid texture source type');
    }
    
    // Reset flip setting
    gl.pixelStorei(gl.UNPACK_FLIP_Y_WEBGL, false);
    
    return texture;
  }
  
  /**
   * Set up ping-pong buffers for automaton rendering
   * @param {number} width - Buffer width
   * @param {number} height - Buffer height
   */
  setupPingPongBuffers(width, height) {
    const gl = this.gl;
    
    // Clean up existing buffers
    if (this.pingTexture) gl.deleteTexture(this.pingTexture);
    if (this.pongTexture) gl.deleteTexture(this.pongTexture);
    if (this.pingFramebuffer) gl.deleteFramebuffer(this.pingFramebuffer);
    if (this.pongFramebuffer) gl.deleteFramebuffer(this.pongFramebuffer);
    
    // Create ping texture and framebuffer
    this.pingTexture = gl.createTexture();
    gl.bindTexture(gl.TEXTURE_2D, this.pingTexture);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.NEAREST);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.NEAREST);
    gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA, width, height, 0, gl.RGBA, gl.UNSIGNED_BYTE, null);
    
    this.pingFramebuffer = gl.createFramebuffer();
    gl.bindFramebuffer(gl.FRAMEBUFFER, this.pingFramebuffer);
    gl.framebufferTexture2D(gl.FRAMEBUFFER, gl.COLOR_ATTACHMENT0, gl.TEXTURE_2D, this.pingTexture, 0);
    
    // Create pong texture and framebuffer
    this.pongTexture = gl.createTexture();
    gl.bindTexture(gl.TEXTURE_2D, this.pongTexture);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.NEAREST);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.NEAREST);
    gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA, width, height, 0, gl.RGBA, gl.UNSIGNED_BYTE, null);
    
    this.pongFramebuffer = gl.createFramebuffer();
    gl.bindFramebuffer(gl.FRAMEBUFFER, this.pongFramebuffer);
    gl.framebufferTexture2D(gl.FRAMEBUFFER, gl.COLOR_ATTACHMENT0, gl.TEXTURE_2D, this.pongTexture, 0);
    
    // Reset to default framebuffer
    gl.bindFramebuffer(gl.FRAMEBUFFER, null);
    
    // Start with ping buffer
    this.currentBuffer = 'ping';
  }
  
  /**
   * Execute dithering operation on the current texture
   * @param {number} paletteSize - Number of colors in palette (2-256)
   * @param {number} mode - Dithering mode: 0 = ordered dithering, 1 = nearest-neighbor
   */
  executeDither(paletteSize, mode = 0) {
    const gl = this.gl;
    const program = this.ditherProgram;
    
    if (!program) {
      throw new Error('Dither shader not initialized');
    }
    
    if (!this.copyProgram) {
      throw new Error('Copy shader not initialized');
    }
    
    // Use the dither program
    gl.useProgram(program);
    
    // Set up uniforms
    const imageLocation = gl.getUniformLocation(program, 'u_image');
    const resolutionLocation = gl.getUniformLocation(program, 'u_resolution');
    const paletteSizeLocation = gl.getUniformLocation(program, 'u_paletteSize');
    const ditherModeLocation = gl.getUniformLocation(program, 'u_ditherMode');
    
    gl.uniform1i(imageLocation, 0);
    gl.uniform2f(resolutionLocation, this.canvas.width, this.canvas.height);
    gl.uniform1f(paletteSizeLocation, paletteSize);
    gl.uniform1i(ditherModeLocation, mode);
    
    // Bind current texture
    gl.activeTexture(gl.TEXTURE0);
    gl.bindTexture(gl.TEXTURE_2D, this.currentTexture);
    
    // Render to ping buffer
    gl.bindFramebuffer(gl.FRAMEBUFFER, this.pingFramebuffer);
    gl.viewport(0, 0, this.canvas.width, this.canvas.height);
    
    this.renderQuad(program);
    
    // Copy result to canvas using copy shader
    gl.useProgram(this.copyProgram);
    
    const copyImageLocation = gl.getUniformLocation(this.copyProgram, 'u_image');
    const copyResolutionLocation = gl.getUniformLocation(this.copyProgram, 'u_resolution');
    
    gl.uniform1i(copyImageLocation, 0);
    gl.uniform2f(copyResolutionLocation, this.canvas.width, this.canvas.height);
    
    gl.bindFramebuffer(gl.FRAMEBUFFER, null);
    gl.viewport(0, 0, this.canvas.width, this.canvas.height);
    
    gl.activeTexture(gl.TEXTURE0);
    gl.bindTexture(gl.TEXTURE_2D, this.pingTexture);
    
    this.renderQuad(this.copyProgram);
    
    // Update current buffer state
    this.currentBuffer = 'ping';
  }
  
  /**
   * Execute one step of the automaton
   * @param {number} paletteSize - Number of colors in palette
   */
  executeAutomatonStep(paletteSize) {
    const gl = this.gl;
    const program = this.automatonProgram;
    
    if (!program) {
      throw new Error('Automaton shader not initialized');
    }
    
    // Use the automaton program
    gl.useProgram(program);
    
    // Set up uniforms
    const stateLocation = gl.getUniformLocation(program, 'u_state');
    const resolutionLocation = gl.getUniformLocation(program, 'u_resolution');
    const paletteSizeLocation = gl.getUniformLocation(program, 'u_paletteSize');
    const randomLocation = gl.getUniformLocation(program, 'u_random');
    
    gl.uniform1i(stateLocation, 0);
    gl.uniform2f(resolutionLocation, this.canvas.width, this.canvas.height);
    gl.uniform1f(paletteSizeLocation, paletteSize);
    gl.uniform1f(randomLocation, Math.random());
    
    // Determine source and destination buffers
    const sourceTexture = this.currentBuffer === 'ping' ? this.pingTexture : this.pongTexture;
    const destFramebuffer = this.currentBuffer === 'ping' ? this.pongFramebuffer : this.pingFramebuffer;
    
    // Bind source texture
    gl.activeTexture(gl.TEXTURE0);
    gl.bindTexture(gl.TEXTURE_2D, sourceTexture);
    
    // Render to destination framebuffer
    gl.bindFramebuffer(gl.FRAMEBUFFER, destFramebuffer);
    gl.viewport(0, 0, this.canvas.width, this.canvas.height);
    
    this.renderQuad(program);
    
    // Swap buffers
    this.currentBuffer = this.currentBuffer === 'ping' ? 'pong' : 'ping';
    
    // Copy result to canvas using copy shader
    const resultTexture = this.currentBuffer === 'ping' ? this.pingTexture : this.pongTexture;
    
    if (!this.copyProgram) {
      throw new Error('Copy shader not initialized');
    }
    
    gl.useProgram(this.copyProgram);
    
    const copyImageLocation = gl.getUniformLocation(this.copyProgram, 'u_image');
    const copyResolutionLocation = gl.getUniformLocation(this.copyProgram, 'u_resolution');
    
    gl.uniform1i(copyImageLocation, 0);
    gl.uniform2f(copyResolutionLocation, this.canvas.width, this.canvas.height);
    
    gl.bindFramebuffer(gl.FRAMEBUFFER, null);
    gl.viewport(0, 0, this.canvas.width, this.canvas.height);
    
    gl.activeTexture(gl.TEXTURE0);
    gl.bindTexture(gl.TEXTURE_2D, resultTexture);
    
    this.renderQuad(this.copyProgram);
  }
  
  /**
   * Render a full-screen quad
   * @param {WebGLProgram} program - Shader program to use
   */
  renderQuad(program) {
    const gl = this.gl;
    
    // Set up vertex attributes
    this.setupQuadBuffer(program);
    
    // Draw the quad (triangle strip)
    gl.drawArrays(gl.TRIANGLE_STRIP, 0, 4);
  }
  
  /**
   * Get the current canvas content as ImageData
   * @returns {ImageData} Canvas image data
   */
  getCanvasImageData() {
    const gl = this.gl;
    const width = this.canvas.width;
    const height = this.canvas.height;
    
    const pixels = new Uint8Array(width * height * 4);
    gl.readPixels(0, 0, width, height, gl.RGBA, gl.UNSIGNED_BYTE, pixels);
    
    // WebGL reads from bottom-left, need to flip vertically
    const flipped = new Uint8Array(width * height * 4);
    for (let y = 0; y < height; y++) {
      const srcRow = (height - 1 - y) * width * 4;
      const dstRow = y * width * 4;
      flipped.set(pixels.subarray(srcRow, srcRow + width * 4), dstRow);
    }
    
    return new ImageData(new Uint8ClampedArray(flipped), width, height);
  }
  
  /**
   * Clean up WebGL resources
   */
  dispose() {
    const gl = this.gl;
    
    if (this.ditherProgram) gl.deleteProgram(this.ditherProgram);
    if (this.automatonProgram) gl.deleteProgram(this.automatonProgram);
    if (this.copyProgram) gl.deleteProgram(this.copyProgram);
    
    if (this.currentTexture) gl.deleteTexture(this.currentTexture);
    if (this.pingTexture) gl.deleteTexture(this.pingTexture);
    if (this.pongTexture) gl.deleteTexture(this.pongTexture);
    
    if (this.pingFramebuffer) gl.deleteFramebuffer(this.pingFramebuffer);
    if (this.pongFramebuffer) gl.deleteFramebuffer(this.pongFramebuffer);
    
    if (this.quadBuffer) gl.deleteBuffer(this.quadBuffer);
  }
}
