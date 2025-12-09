/**
 * @fileoverview Image processing module for loading, scaling, and rendering image columns.
 * 
 * This module handles all image-related operations including:
 * - Loading images from File objects
 * - Validating image formats (PNG, JPEG, GIF)
 * - Scaling images while maintaining aspect ratio
 * - Efficient column rendering using canvas operations
 */

/**
 * Supported image MIME types
 */
const SUPPORTED_FORMATS = ['image/png', 'image/jpeg', 'image/gif'];

/**
 * Loads an image from a File object.
 * 
 * @param {File} file - The image file to load
 * @returns {Promise<HTMLImageElement>} Promise that resolves with the loaded image
 * @throws {Error} If file format is invalid or image fails to load
 */
export async function loadImage(file) {
    // Validate file exists
    if (!file) {
        throw new Error('No file provided');
    }

    // Validate file size (limit to 50MB)
    const maxSize = 50 * 1024 * 1024; // 50MB
    if (file.size > maxSize) {
        throw new Error(`File too large (${Math.round(file.size / 1024 / 1024)}MB). Maximum size is 50MB.`);
    }

    // Validate file format
    if (!SUPPORTED_FORMATS.includes(file.type)) {
        const formatName = file.type || 'unknown';
        throw new Error(`Unsupported format: ${formatName}. Please use PNG, JPEG, or GIF.`);
    }

    return new Promise((resolve, reject) => {
        const img = new Image();
        const url = URL.createObjectURL(file);

        img.onload = () => {
            URL.revokeObjectURL(url);
            
            // Validate image dimensions
            if (img.width === 0 || img.height === 0) {
                reject(new Error('Invalid image: width or height is zero.'));
                return;
            }

            // Warn about very large images
            if (img.width > 5000 || img.height > 5000) {
                console.warn(`Large image detected: ${img.width}x${img.height}. Performance may be affected.`);
            }

            resolve(img);
        };

        img.onerror = () => {
            URL.revokeObjectURL(url);
            reject(new Error('Failed to load image. The file may be corrupted or in an unsupported format.'));
        };

        img.src = url;
    });
}

/**
 * Scales an image to fit within maximum dimensions while maintaining aspect ratio.
 * If the image is smaller than the constraints, it is returned unchanged.
 * 
 * @param {HTMLImageElement} image - The source image to scale
 * @param {number} maxWidth - Maximum width constraint
 * @param {number} maxHeight - Maximum height constraint
 * @returns {HTMLImageElement} Scaled image (or original if no scaling needed)
 */
export function scaleImage(image, maxWidth, maxHeight) {
    const { width, height } = image;

    // If image fits within constraints, return as-is
    if (width <= maxWidth && height <= maxHeight) {
        return image;
    }

    // Calculate scaling factor to fit within constraints while maintaining aspect ratio
    const widthRatio = maxWidth / width;
    const heightRatio = maxHeight / height;
    const scale = Math.min(widthRatio, heightRatio);

    const newWidth = Math.floor(width * scale);
    const newHeight = Math.floor(height * scale);

    // Create a canvas to render the scaled image
    const canvas = document.createElement('canvas');
    canvas.width = newWidth;
    canvas.height = newHeight;

    const ctx = canvas.getContext('2d');
    ctx.drawImage(image, 0, 0, newWidth, newHeight);

    // Create a new image from the canvas
    const scaledImage = new Image();
    scaledImage.src = canvas.toDataURL();

    // Store dimensions for synchronous access
    scaledImage.width = newWidth;
    scaledImage.height = newHeight;

    return scaledImage;
}

/**
 * Renders all columns from the source image to the canvas based on column order.
 * This is used for initial rendering and after scrambling.
 * 
 * Performance: Renders all columns in one pass. For large images (1000+ columns),
 * this is still very fast due to efficient canvas operations.
 * 
 * @param {HTMLCanvasElement} canvas - Target canvas element
 * @param {HTMLImageElement} sourceImage - Source image to render from
 * @param {number[]} columnOrder - Array mapping display position to source column index
 * @throws {Error} If canvas rendering fails
 */
export function renderAllColumns(canvas, sourceImage, columnOrder) {
    try {
        const ctx = canvas.getContext('2d');
        if (!ctx) {
            throw new Error('Failed to get canvas 2D context. Canvas may not be supported.');
        }

        const imageWidth = sourceImage.width;
        const imageHeight = sourceImage.height;
        const numColumns = columnOrder.length;

        // Validate inputs
        if (!imageWidth || !imageHeight) {
            throw new Error('Invalid image dimensions');
        }

        if (!numColumns || numColumns === 0) {
            throw new Error('Invalid column order: empty array');
        }

        // Set canvas size to match image
        canvas.width = imageWidth;
        canvas.height = imageHeight;

        // Calculate column width (typically 1 pixel, but could be wider for narrow images)
        const colWidth = imageWidth / numColumns;

        // Clear canvas
        ctx.clearRect(0, 0, imageWidth, imageHeight);

        // Render each column
        for (let displayPos = 0; displayPos < numColumns; displayPos++) {
            const sourceCol = columnOrder[displayPos];
            
            // Validate column index
            if (sourceCol < 0 || sourceCol >= numColumns) {
                console.warn(`Invalid column index ${sourceCol} at position ${displayPos}`);
                continue;
            }
            
            // Copy column from source image to display position
            ctx.drawImage(
                sourceImage,
                sourceCol * colWidth, 0, colWidth, imageHeight,  // source rectangle
                displayPos * colWidth, 0, colWidth, imageHeight  // destination rectangle
            );
        }
    } catch (error) {
        console.error('Canvas rendering error:', error);
        throw new Error(`Failed to render image: ${error.message}`);
    }
}

/**
 * Renders specific columns at given positions (lazy rendering).
 * This is used during sorting to only redraw the columns that changed.
 * 
 * Performance: Only redraws affected columns (typically 2 during a swap).
 * This makes sorting animations smooth even for very large images.
 * 
 * @param {HTMLCanvasElement} canvas - Target canvas element
 * @param {HTMLImageElement} sourceImage - Source image to render from
 * @param {number[]} columnOrder - Array mapping display position to source column index
 * @param {number[]} positions - Array of display positions to redraw
 * @throws {Error} If canvas rendering fails
 */
export function renderColumnsAt(canvas, sourceImage, columnOrder, positions) {
    try {
        const ctx = canvas.getContext('2d');
        if (!ctx) {
            throw new Error('Failed to get canvas 2D context');
        }

        const imageWidth = sourceImage.width;
        const imageHeight = sourceImage.height;
        const numColumns = columnOrder.length;
        const colWidth = imageWidth / numColumns;

        // Render only the specified columns
        for (const displayPos of positions) {
            // Validate position
            if (displayPos < 0 || displayPos >= numColumns) {
                console.warn(`Invalid display position ${displayPos}`);
                continue;
            }

            const sourceCol = columnOrder[displayPos];
            
            // Validate source column
            if (sourceCol < 0 || sourceCol >= numColumns) {
                console.warn(`Invalid source column ${sourceCol} at position ${displayPos}`);
                continue;
            }
            
            // Clear the column area first to remove any previous content (including highlights)
            // This is essential for images with transparency
            ctx.clearRect(displayPos * colWidth, 0, colWidth, imageHeight);
            
            // Copy column from source image to display position
            ctx.drawImage(
                sourceImage,
                sourceCol * colWidth, 0, colWidth, imageHeight,  // source rectangle
                displayPos * colWidth, 0, colWidth, imageHeight  // destination rectangle
            );
        }
    } catch (error) {
        console.error('Canvas rendering error:', error);
        throw new Error(`Failed to render columns: ${error.message}`);
    }
}

/**
 * Gets the aspect ratio of an image.
 * 
 * @param {HTMLImageElement} image - The image to measure
 * @returns {number} Aspect ratio (width / height)
 */
export function getAspectRatio(image) {
    return image.width / image.height;
}

/**
 * Generates a synthetic image where each column has a height corresponding to its index.
 * Creates a diagonal pattern from black (top-left) to white (bottom-right).
 * 
 * @param {number} width - Width of the image (number of columns)
 * @returns {Promise<HTMLImageElement>} Promise that resolves with the generated image
 */
export function generateSyntheticImage(width) {
    const height = width; // Square image
    
    // Create a canvas to draw the synthetic image
    const canvas = document.createElement('canvas');
    canvas.width = width;
    canvas.height = height;
    
    const ctx = canvas.getContext('2d');
    
    // Draw each column with height corresponding to its index
    for (let col = 0; col < width; col++) {
        // Column height is proportional to its index
        // Column 0 has height 0, column (width-1) has height (height-1)
        const columnHeight = Math.floor((col / (width - 1)) * (height - 1));
        
        // Draw white column from bottom up to the calculated height
        ctx.fillStyle = '#FFFFFF';
        ctx.fillRect(col, height - columnHeight, 1, columnHeight);
        
        // Draw black for the rest (top part)
        ctx.fillStyle = '#000000';
        ctx.fillRect(col, 0, 1, height - columnHeight);
    }
    
    // Create an image from the canvas and wait for it to load
    return new Promise((resolve) => {
        const image = new Image();
        image.width = width;
        image.height = height;
        
        image.onload = () => {
            resolve(image);
        };
        
        // Set src after onload handler is attached
        image.src = canvas.toDataURL();
    });
}


