'''Render an image as text with unicode shade characters
Uses Floyd-Steinberg dithering on a grayscale version of the image
'''
import numpy as np
from PIL import Image
from resize import resize

palette = [' ', '░', '▒', '▓', '█']
paletteSize = len(palette)

def findClosestColor(color: int, paletteSize: int = 5) -> int:
        color *= paletteSize
        color = color // 1
        color = color / paletteSize
        return color

def dither(arr: np.ndarray) -> np.ndarray:
    '''dithers image into a more discrete array with "lower bit depth"

    Args:
        arr (numpy.ndarray): image in range [0, 256)
            shape (height, width)
    
    Returns:
        numpy.ndarray: dithered image in range [0, 1)
            shape (height, width)
    '''
    # normalize image to [0, 1)
    img = np.copy(img)
    img = img / 256
    
    height, width = img.shape
    for r in range(height):
        for c in range(width):
            oldPixel = img[r, c]
            newPixel = findClosestColor(oldPixel)
            img[r, c] = newPixel
            error = oldPixel - newPixel

            right = c < width - 1
            down = r < height - 1
            left = c > 0

            if right:
                img[r, c+1] += error * 7/16
            if right and down:
                img[r+1, c+1] += error * 1/16
            if down:
                img[r+1, c] += error * 5/16
            if left and down:
                img[r+1, c-1] += error * 1/16
    return img

def ditherToText(arr: np.ndarray) -> list:
    '''takes dithered image array in range 0-1
    and converts it to text

    Args:
        arr (numpy.ndarray): dithered array in range [0, 1)
            shape (height, width)
            should be "low bit depth", only having values corresponding to elements in the dither palette

    Returns:
        list: list of strings
            Each string is a row of the image 
    '''
    height, width = arr.shape

    # map from grayscale value to palette item
    pixelToPalette = {i / paletteSize: palette[i] for i in range(paletteSize)}

    strings = []
    for r in range(height):
        row = []
        for c in range(width):
            pixel = arr[r, c]
            newValue = pixelToPalette[pixel]
            row.append(newValue)
        strings.append(row)
    return strings
