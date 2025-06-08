document.addEventListener('DOMContentLoaded', async () => {
    // Get DOM elements
    const canvas = document.getElementById('game-canvas');
    const imageUpload = document.getElementById('image-upload');
    const startBtn = document.getElementById('start-btn');
    const pauseBtn = document.getElementById('pause-btn');
    const resetBtn = document.getElementById('reset-btn');
    const speedSlider = document.getElementById('speed-slider');
    const speedValue = document.getElementById('speed-value');
    
    // Initialize Game of Life
    let gameOfLife;
    try {
        gameOfLife = new GameOfLife(canvas);
    } catch (error) {
        console.error('Failed to initialize WebGL:', error);
        showError('Your browser does not support WebGL, which is required for this application.');
        return;
    }
    
    // Generate a default 800x800 random pixel image
    try {
        console.log('Generating default 800x800 random pixel image...');
        const defaultImage = await generateRandomImage(800, 800);
        console.log(`Default image dimensions: ${defaultImage.width}x${defaultImage.height}`);
        await gameOfLife.loadImage(defaultImage);
        
        // Enable controls
        startBtn.disabled = false;
        resetBtn.disabled = false;
        
        // Render the initial state
        gameOfLife.render();
        
        console.log('Default random image generated successfully');
    } catch (error) {
        console.error('Failed to generate default image:', error);
    }
    
    // Function to generate a random pixel image
    function generateRandomImage(width, height) {
        return new Promise((resolve) => {
            // Create a canvas element
            const canvas = document.createElement('canvas');
            canvas.width = width;
            canvas.height = height;
            const ctx = canvas.getContext('2d');
            
            // Create an ImageData object
            const imageData = ctx.createImageData(width, height);
            const data = imageData.data;
            
            // Fill with random black and white pixels
            for (let i = 0; i < data.length; i += 4) {
                // Random value (0 or 255)
                const value = Math.random() > 0.5 ? 255 : 0;
                
                // Set RGB values to the same random value (black or white)
                data[i] = value;     // R
                data[i + 1] = value; // G
                data[i + 2] = value; // B
                data[i + 3] = 255;   // A (fully opaque)
            }
            
            // Put the image data on the canvas
            ctx.putImageData(imageData, 0, 0);
            
            // Convert canvas to image
            const img = new Image();
            img.width = width;
            img.height = height;
            
            // Set up onload handler before setting src
            img.onload = () => {
                console.log(`Random image created with dimensions: ${img.width}x${img.height}`);
                resolve(img);
            };
            
            // Set src to trigger loading
            img.src = canvas.toDataURL();
        });
    }
    
    // Handle image upload
    imageUpload.addEventListener('change', async (event) => {
        const file = event.target.files[0];
        if (!file) return;
        
        // Check if the file is an image
        if (!file.type.match('image.*')) {
            showError('Please select an image file.');
            return;
        }
        
        try {
            const imageData = await loadImageFile(file);
            
            // Load the image into the Game of Life
            await gameOfLife.loadImage(imageData);
            
            // Enable controls
            startBtn.disabled = false;
            resetBtn.disabled = false;
            
            // Render the initial state
            gameOfLife.render();
            
            console.log('Image loaded successfully');
        } catch (error) {
            console.error('Failed to load image:', error);
            showError('Failed to process the image. Please try a different one.');
        }
    });
    
    // Helper function to load an image file
    function loadImageFile(file) {
        return new Promise((resolve, reject) => {
            const reader = new FileReader();
            
            reader.onload = (e) => {
                const img = new Image();
                img.onload = () => resolve(img);
                img.onerror = () => reject(new Error('Failed to load the image'));
                img.src = e.target.result;
            };
            
            reader.onerror = () => reject(new Error('Failed to read the image file'));
            reader.readAsDataURL(file);
        });
    }
    
    // Helper function to load an image from URL
    function loadImageFromUrl(url) {
        return new Promise((resolve, reject) => {
            const img = new Image();
            img.onload = () => resolve(img);
            img.onerror = () => reject(new Error(`Failed to load image from URL: ${url}`));
            img.src = url;
        });
    }
    
    // Handle start button
    startBtn.addEventListener('click', () => {
        gameOfLife.start();
        startBtn.disabled = true;
        pauseBtn.disabled = false;
    });
    
    // Handle pause button
    pauseBtn.addEventListener('click', () => {
        gameOfLife.pause();
        startBtn.disabled = false;
        pauseBtn.disabled = true;
    });
    
    // Handle reset button
    resetBtn.addEventListener('click', () => {
        gameOfLife.reset();
        if (gameOfLife.isRunning) {
            startBtn.disabled = true;
            pauseBtn.disabled = false;
        } else {
            startBtn.disabled = false;
            pauseBtn.disabled = true;
        }
    });
    
    // Handle speed slider
    speedSlider.addEventListener('input', () => {
        const fps = parseInt(speedSlider.value);
        gameOfLife.setFrameRate(fps);
        speedValue.textContent = `${fps} generations per second`;
    });
    
    // Show error message
    function showError(message) {
        const errorDiv = document.createElement('div');
        errorDiv.className = 'error-message';
        errorDiv.textContent = message;
        
        // Style the error message
        errorDiv.style.backgroundColor = '#f8d7da';
        errorDiv.style.color = '#721c24';
        errorDiv.style.padding = '10px';
        errorDiv.style.margin = '10px 0';
        errorDiv.style.borderRadius = '4px';
        errorDiv.style.textAlign = 'center';
        
        // Add a close button
        const closeBtn = document.createElement('button');
        closeBtn.textContent = '×';
        closeBtn.style.float = 'right';
        closeBtn.style.background = 'none';
        closeBtn.style.border = 'none';
        closeBtn.style.fontSize = '20px';
        closeBtn.style.cursor = 'pointer';
        closeBtn.style.color = '#721c24';
        closeBtn.onclick = () => errorDiv.remove();
        
        errorDiv.prepend(closeBtn);
        
        // Insert the error message at the top of the container
        const container = document.querySelector('.container');
        container.insertBefore(errorDiv, container.firstChild);
        
        // Auto-remove after 5 seconds
        setTimeout(() => {
            if (errorDiv.parentNode) {
                errorDiv.remove();
            }
        }, 5000);
    }
    
    // Handle window resize
    window.addEventListener('resize', () => {
        // Only resize if an image is loaded
        if (gameOfLife.originalImageData) {
            console.log(`Resizing display for image: ${gameOfLife.originalImageData.width}x${gameOfLife.originalImageData.height}`);
            
            // Calculate the maximum size that fits in the container
            const containerWidth = canvas.parentElement.clientWidth;
            const containerHeight = window.innerHeight * 0.6; // 60% of viewport height
            
            const imageAspectRatio = gameOfLife.originalImageData.width / gameOfLife.originalImageData.height;
            
            let newWidth, newHeight;
            
            if (containerWidth / containerHeight > imageAspectRatio) {
                // Container is wider than the image aspect ratio
                newHeight = containerHeight;
                newWidth = containerHeight * imageAspectRatio;
            } else {
                // Container is taller than the image aspect ratio
                newWidth = containerWidth;
                newHeight = containerWidth / imageAspectRatio;
            }
            
            // Set the display size (CSS pixels) for responsive scaling
            canvas.style.width = `${newWidth}px`;
            canvas.style.height = `${newHeight}px`;
            
            // The actual canvas dimensions are set in gameOfLife.resizeCanvas() to match the image dimensions
            // This ensures pixel-perfect rendering without anti-aliasing
            canvas.width = gameOfLife.originalImageData.width;
            canvas.height = gameOfLife.originalImageData.height;
            
            // Make sure the WebGL viewport matches the canvas dimensions
            const gl = gameOfLife.gl;
            gl.viewport(0, 0, gameOfLife.originalImageData.width, gameOfLife.originalImageData.height);
        }
    });
    
    // Initial resize
    window.dispatchEvent(new Event('resize'));
});
