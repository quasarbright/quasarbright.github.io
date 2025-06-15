let img; // Original uploaded image
let juliaImg; // Image with Julia set transformation
let isImageLoaded = false;
let c = { re: 0, im: 0 }; // Julia set parameter
let maxIterations = 50;
const ESCAPE_RADIUS = 2;
let quality = 1; // 1 = full quality, higher values = lower quality but faster
let tileSize = 2.0; // Size of each tile in the complex plane
let needsUpdate = true; // Flag to determine if we need to recompute
let isMouseDragging = false; // Track if mouse is being dragged on canvas

function setup() {
  // Create canvas that will resize based on the uploaded image
  createCanvas(600, 600).parent('canvasContainer');
  pixelDensity(1);
  
  // Set up file input handler
  const fileInput = document.getElementById('imageUpload');
  fileInput.addEventListener('change', handleFileUpload);
  
  // Set up slider handlers
  const iterationSlider = document.getElementById('iterationSlider');
  iterationSlider.addEventListener('input', function() {
    maxIterations = parseInt(this.value);
    document.getElementById('iterationValue').textContent = maxIterations;
    needsUpdate = true;
  });
  
  const qualitySlider = document.getElementById('qualitySlider');
  qualitySlider.addEventListener('input', function() {
    quality = parseInt(this.value);
    document.getElementById('qualityValue').textContent = quality;
    needsUpdate = true;
  });
  
  // Set up tile size slider
  const tileSlider = document.getElementById('tileSlider');
  tileSlider.addEventListener('input', function() {
    tileSize = parseFloat(this.value);
    document.getElementById('tileValue').textContent = tileSize.toFixed(1);
    needsUpdate = true;
  });
  
  // Set up save button handler
  const saveButton = document.getElementById('saveButton');
  saveButton.addEventListener('click', function() {
    if (isImageLoaded) {
      // Force high quality render before saving
      const prevQuality = quality;
      quality = 1;
      applyJuliaTransform();
      quality = prevQuality;
      
      // Save the canvas
      saveCanvas('julia_transformed_image', 'png');
    }
  });
  
  // Set up reset button handler
  const resetButton = document.getElementById('resetButton');
  resetButton.addEventListener('click', function() {
    if (isImageLoaded) {
      // Reset parameters to defaults
      maxIterations = 50;
      document.getElementById('iterationSlider').value = 50;
      document.getElementById('iterationValue').textContent = 50;
      
      quality = 1;
      document.getElementById('qualitySlider').value = 1;
      document.getElementById('qualityValue').textContent = 1;
      
      tileSize = 2.0;
      document.getElementById('tileSlider').value = 2.0;
      document.getElementById('tileValue').textContent = '2.0';
      
      c = { re: 0, im: 0 };
      document.getElementById('cValue').textContent = '0.000 + 0.000i';
      
      needsUpdate = true;
    }
  });
  
  // Initial display
  background(50);
  textSize(20);
  textAlign(CENTER, CENTER);
  fill(255);
  text('Upload an image to begin', width/2, height/2);
}

function draw() {
  if (isImageLoaded) {
    // Calculate potential c value based on current mouse position
    const mouseRe = map(mouseX, 0, width, -1, 1);
    const mouseIm = map(mouseY, 0, height, -1, 1);
    
    // If mouse is being dragged within the canvas, update c
    if (isMouseDragging && mouseX >= 0 && mouseX < width && mouseY >= 0 && mouseY < height) {
      c.re = mouseRe;
      c.im = mouseIm;
      needsUpdate = true;
    }
    
    // Update display of current and potential c value
    if (isMouseDragging) {
      document.getElementById('cValue').textContent = `${c.re.toFixed(3)} + ${c.im.toFixed(3)}i`;
    } else {
      document.getElementById('cValue').textContent = `${c.re.toFixed(3)} + ${c.im.toFixed(3)}i (Click/drag to change: ${mouseRe.toFixed(3)} + ${mouseIm.toFixed(3)}i)`;
    }
    
    // Only recompute when needed
    if (needsUpdate) {
      applyJuliaTransform();
      needsUpdate = false;
    }
    
    // Display the transformed image
    image(juliaImg, 0, 0, width, height);
  }
}

// Handle mouse events
function mousePressed() {
  if (!isImageLoaded) return;
  
  // Check if the click is within the canvas
  if (mouseX >= 0 && mouseX < width && mouseY >= 0 && mouseY < height) {
    // Set dragging flag
    isMouseDragging = true;
    
    // Update c based on mouse click position
    c.re = map(mouseX, 0, width, -1, 1);
    c.im = map(mouseY, 0, height, -1, 1);
    
    // Force update
    needsUpdate = true;
  }
}

function mouseReleased() {
  // Reset dragging flag
  isMouseDragging = false;
}

// Handle keyboard shortcuts
function keyPressed() {
  if (!isImageLoaded) return;
  
  if (key === 's' || key === 'S') {
    // Save image (same as clicking save button)
    document.getElementById('saveButton').click();
  } else if (key === '+' || key === '=') {
    // Increase iterations
    const slider = document.getElementById('iterationSlider');
    slider.value = constrain(parseInt(slider.value) + 10, 10, 100);
    slider.dispatchEvent(new Event('input'));
  } else if (key === '-' || key === '_') {
    // Decrease iterations
    const slider = document.getElementById('iterationSlider');
    slider.value = constrain(parseInt(slider.value) - 10, 10, 100);
    slider.dispatchEvent(new Event('input'));
  } else if (key === '1' || key === '2' || key === '3' || key === '4' || key === '5') {
    // Set quality directly
    const slider = document.getElementById('qualitySlider');
    slider.value = key;
    slider.dispatchEvent(new Event('input'));
  } else if (key === ',') {
    // Decrease tile size
    const slider = document.getElementById('tileSlider');
    slider.value = constrain(parseFloat(slider.value) - 0.1, 0.1, 5.0).toFixed(1);
    slider.dispatchEvent(new Event('input'));
  } else if (key === '.') {
    // Increase tile size
    const slider = document.getElementById('tileSlider');
    slider.value = constrain(parseFloat(slider.value) + 0.1, 0.1, 5.0).toFixed(1);
    slider.dispatchEvent(new Event('input'));
  }
}

function handleFileUpload(event) {
  const file = event.target.files[0];
  if (file && file.type.match(/^image/)) {
    const fileReader = new FileReader();
    
    fileReader.onload = function(e) {
      // Load the image
      loadImage(e.target.result, loadedImg => {
        img = loadedImg;
        
        // Resize canvas to match image dimensions (with max size limits)
        const maxSize = 600;
        let imgWidth = img.width;
        let imgHeight = img.height;
        
        if (imgWidth > maxSize || imgHeight > maxSize) {
          const ratio = imgWidth / imgHeight;
          if (ratio > 1) {
            imgWidth = maxSize;
            imgHeight = maxSize / ratio;
          } else {
            imgHeight = maxSize;
            imgWidth = maxSize * ratio;
          }
        }
        
        resizeCanvas(imgWidth, imgHeight);
        
        // Create a p5.js image for the Julia transformation with same dimensions
        juliaImg = createImage(imgWidth, imgHeight);
        
        isImageLoaded = true;
        needsUpdate = true; // Force update with new image
        
        // Reset quality to 1 for initial render
        quality = 1;
        document.getElementById('qualitySlider').value = 1;
        document.getElementById('qualityValue').textContent = 1;
        
        // Enable buttons
        document.getElementById('saveButton').disabled = false;
        document.getElementById('resetButton').disabled = false;
      });
    };
    
    fileReader.readAsDataURL(file);
  }
}

function applyJuliaTransform() {
  // Show loading indicator
  push();
  background(50, 150);
  fill(255);
  textAlign(CENTER, CENTER);
  textSize(20);
  text('Processing...', width/2, height/2);
  pop();
  
  // Load pixels of both images
  img.loadPixels();
  juliaImg.loadPixels();
  
  // Use quality setting to skip pixels for faster rendering
  // quality of 1 = process every pixel
  // quality of 2 = process every other pixel
  // quality of 3 = process every third pixel, etc.
  
  // For each pixel in the output image
  for (let x = 0; x < juliaImg.width; x += quality) {
    for (let y = 0; y < juliaImg.height; y += quality) {
      // Map pixel coordinates to complex plane (-2 to 2)
      const zx = map(x, 0, juliaImg.width, -2, 2);
      const zy = map(y, 0, juliaImg.height, -2, 2);
      
      // Apply Julia iteration
      const result = juliaIteration(zx, zy);
      
      // Get pixel color
      let r, g, b;
      
      if (result.escaped) {
        // If point escapes (diverges), use original image color at that position
        // This helps preserve more of the original image
        const origX = constrain(Math.floor(map(x, 0, juliaImg.width, 0, img.width)), 0, img.width - 1);
        const origY = constrain(Math.floor(map(y, 0, juliaImg.height, 0, img.height)), 0, img.height - 1);
        const origIndex = (origX + origY * img.width) * 4;
        
        // Darken the original color to distinguish it from non-escaped points
        r = img.pixels[origIndex] * 0.3;
        g = img.pixels[origIndex + 1] * 0.3;
        b = img.pixels[origIndex + 2] * 0.3;
      } else {
        // For non-escaped points, use a tiling approach
        // This creates multiple copies of the image in the complex plane
        const scaledX = result.zx * 100; // Fixed scaling factor
        const scaledY = result.zy * 100;
        
        // Then use a tiling approach by taking modulo of the scaled coordinates
        // This creates a grid of image tiles in the complex plane
        // tileSize controls the size of each tile in the complex plane
        const tiledX = ((scaledX % tileSize) + tileSize) % tileSize; // Ensure positive
        const tiledY = ((scaledY % tileSize) + tileSize) % tileSize; // Ensure positive
        
        // Map the tiled coordinates to image coordinates with some variation
        // We'll use a different mapping strategy for each quadrant of the tile
        // This ensures we get more variety from the image
        let imgX, imgY;
        
        // Determine which quadrant of the tile we're in
        const halfTile = tileSize / 2;
        const quadrant = (tiledX < halfTile ? 0 : 1) + (tiledY < halfTile ? 0 : 2);
        
        switch(quadrant) {
          case 0: // Top-left quadrant - normal mapping
            imgX = Math.floor(map(tiledX, 0, halfTile, 0, img.width));
            imgY = Math.floor(map(tiledY, 0, halfTile, 0, img.height));
            break;
          case 1: // Top-right quadrant - flipped horizontally
            imgX = Math.floor(map(tiledX, halfTile, tileSize, img.width, 0));
            imgY = Math.floor(map(tiledY, 0, halfTile, 0, img.height));
            break;
          case 2: // Bottom-left quadrant - flipped vertically
            imgX = Math.floor(map(tiledX, 0, halfTile, 0, img.width));
            imgY = Math.floor(map(tiledY, halfTile, tileSize, img.height, 0));
            break;
          case 3: // Bottom-right quadrant - rotated 180 degrees
            imgX = Math.floor(map(tiledX, halfTile, tileSize, img.width, 0));
            imgY = Math.floor(map(tiledY, halfTile, tileSize, img.height, 0));
            break;
        }
        
        // Ensure coordinates are within bounds
        imgX = constrain(imgX, 0, img.width - 1);
        imgY = constrain(imgY, 0, img.height - 1);
        
        // Get color from original image at mapped position
        const imgIndex = (imgX + imgY * img.width) * 4;
        
        // Get the direct color from the image
        r = img.pixels[imgIndex];
        g = img.pixels[imgIndex + 1];
        b = img.pixels[imgIndex + 2];
      }
      
      // Fill a block of pixels based on quality setting
      for (let dx = 0; dx < quality && x + dx < juliaImg.width; dx++) {
        for (let dy = 0; dy < quality && y + dy < juliaImg.height; dy++) {
          const index = ((x + dx) + (y + dy) * juliaImg.width) * 4;
          juliaImg.pixels[index] = r;
          juliaImg.pixels[index + 1] = g;
          juliaImg.pixels[index + 2] = b;
          juliaImg.pixels[index + 3] = 255; // Alpha
        }
      }
    }
  }
  
  // Update the pixels
  juliaImg.updatePixels();
}

function juliaIteration(zx, zy) {
  let escaped = false;
  
  // Iterate the function f(z) = z^2 + c
  for (let i = 0; i < maxIterations; i++) {
    // z^2 = (a+bi)^2 = a^2 + 2abi - b^2 = (a^2 - b^2) + (2ab)i
    const zx2 = zx * zx - zy * zy;
    const zy2 = 2 * zx * zy;
    
    // z = z^2 + c
    zx = zx2 + c.re;
    zy = zy2 + c.im;
    
    // Check if point escapes
    if (zx * zx + zy * zy > ESCAPE_RADIUS * ESCAPE_RADIUS) {
      escaped = true;
      break;
    }
  }
  
  return { zx, zy, escaped };
}

 