import colorsys
import numpy as np
from PIL import Image

def imageToGrayscale(arr: np.ndarray) -> np.ndarray:
    '''converts image from rgb to grayscale
    '''
    img = Image.fromarray(arr)
    grayscale = img.convert('L')
    return np.asarray(grayscale)

def shadeToHue(arr: np.ndarray) -> np.ndarray:
    '''converts image to grayscale, then converts shading to hue 
    '''
    grayscale = imageToGrayscale(arr)
    height, width = grayscale.shape
    newArr = np.zeros((height, width, 3))
    for r in range(height):
        for c in range(width):
            oldPixel = grayscale[r, c]
            shade = oldPixel / 256
            newPixel = (shade, 1, .5)
            newPixel = colorsys.hsv_to_rgb(*newPixel)
            newPixel = np.array(newPixel)*256
            newArr[r, c] = newPixel
    newArr = np.array(newArr)
    newArr = newArr.astype('uint8')
    return newArr

def main(imgPath: str) -> None:
    img = Image.open(imgPath)
    arr = np.asarray(img)
    arr = shadeToHue(arr)
    img = Image.fromarray(arr)
    img.show()

if __name__ == '__main__':
    import sys
    if len(sys.argv) < 2:
        print('must supply image path')
        sys.exit(1)
    imgPath = sys.argv[1]
    main(imgPath)

