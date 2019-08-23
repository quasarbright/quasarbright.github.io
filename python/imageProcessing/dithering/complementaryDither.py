'''
Floyd-Steinberg error-diffusion dithering
converts from 8-bit grayscale to black and white
'''
import random
import colorsys
from PIL import Image
import numpy as np

def dither(img: np.ndarray) -> np.ndarray:
    '''Converts from 8-bit grayscale to black and white
    Uses Floyd-Steinberg dithering algorithm

    Args:
        img (numpy.ndarray): The 8-bit grayscale image in range [0-256)
            Shape (height, width)
    
    Returns:
        numpy.ndarray: The black and white image in set {0, 255}
            Shape (height, width)
    '''
    # normalize image to [0, 1)
    img = np.copy(img)
    img = img / 256

    def findClosestColor(color: int) -> int:
        if color > .5:
            return 1
        else:
            return 0
    
    height, width = img.shape
    for r in range(height):
        for c in range(width):
            oldPixel = img[r, c]
            newPixel = findClosestColor(oldPixel)
            img[r, c] = newPixel
            error = oldPixel - newPixel

            right = c < width -1
            down = r < height -1
            left = c > 0

            if right:
                img[r  , c+1] += error * 7/16
            if right and down:
                img[r+1, c+1] += error * 1/16
            if down:
                img[r+1, c  ] += error * 5/16
            if left and down:
                img[r+1, c-1] += error * 1/16
    img = img * 255
    return img

def imgToGrayscaleArr(imgPath: str) -> np.ndarray:
    '''Convert the image at the given path to a grayscale numpy array

    Args:
        imgPath (string)
    
    Returns:
        numpy.ndarray: Grayscale numpy array in range [0, 256)
            Shape (height, width)
    '''
    img = Image.open(imgPath)
    grayscaleImg = img.convert('L')
    grayscaleArr = np.asarray(grayscaleImg)
    height = grayscaleArr.shape[0]
    width = grayscaleArr.shape[1]
    grayscaleArr = grayscaleArr.reshape(height, width)
    return grayscaleArr

def blackWhiteToColor(img: np.ndarray) -> np.ndarray:
    '''takes a black and white image and converts white to a random color,
        and black to the color's complement
    
    Args:
        img (numpy.ndarray): black and white (dithered) image of shape (height, width)
            containing values of either 0 or 255
    
    Returns:
        numpy.ndarray: image with only two random complementary colors in it.
    '''
    height, width = img.shape
    newImg = np.zeros((height, width, 3))
    
    hu = random.random()
    complement = (0.5 + hu) % 1

    color1 = (hu, 1, .5)
    color1 = colorsys.hsv_to_rgb(*color1)
    color1 = np.array(color1) * 256

    color2 = (complement, 1, .5)
    color2 = colorsys.hsv_to_rgb(*color2)
    color2 = np.array(color2) * 256

    for r in range(height):
        for c in range(width):
            oldPixel = img[r, c]
            if oldPixel == 255:
                newPixel = color1
            else:
                newPixel = color2
            newImg[r, c] = newPixel
    newImg = np.array(newImg)
    return newImg

def main(imgPath: str) -> None:
    arr = imgToGrayscaleArr(imgPath)
    ditheredArr = dither(arr)
    ditheredArr = blackWhiteToColor(ditheredArr)
    ditheredArr = ditheredArr.astype('uint8')
    ditheredImg = Image.fromarray(ditheredArr)
    ditheredImg.show()

if __name__ == '__main__':
    import sys
    if len(sys.argv) < 2:
        print('must supply image path')
        sys.exit(1)
    imgPath = sys.argv[1]
    main(imgPath)


