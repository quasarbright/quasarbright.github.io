import numpy as np
from PIL import Image


def findClosestColor(color: int) -> int:
    '''color in range [0, 1)
    Rounds nearest 0 or 1
    '''
    if color < .5:
        return 0
    else:
        return 1


def dither(arr: np.ndarray) -> np.ndarray:
    '''dithers image

    Args:
        arr (numpy.ndarray): image in range [0, 256)
            shape (height, width)
    
    Returns:
        numpy.ndarray: dithered image in range [0, 1)
            shape (height, width)
    '''
    # normalize image to [0, 1)
    arr = np.copy(arr)
    arr = arr / 256

    height, width = arr.shape[:2]
    for r in range(height):
        for c in range(width):
            oldPixel = arr[r, c]
            newPixel = findClosestColor(oldPixel)
            arr[r, c] = newPixel
            error = oldPixel - newPixel

            right = c < width - 1
            down = r < height - 1
            left = c > 0

            if right:
                arr[r, c+1] += error * 7/16
            if right and down:
                arr[r+1, c+1] += error * 1/16
            if down:
                arr[r+1, c] += error * 5/16
            if left and down:
                arr[r+1, c-1] += error * 1/16
    return arr


def binaryToMoon(subArr):
    '''
    Convert a 1x4 array of ones and zeros to a moon emoji
    '''
    
    for i in range(4):



def main(imgPath: str) -> None:
    img = Image.open(imgPath)
    # grayscale
    img = img.convert('L')
    img = img.resize((80, 80))
    arr = np.asarray(img)
    dithered = dither(arr)
    braille = ditheredToBraille(dithered)
    print(braille)
    with open('braille.txt', 'w', encoding='utf-8') as f:
        f.write(braille)


if __name__ == '__main__':
    import sys
    if len(sys.argv) < 2:
        print('must supply image path')
        sys.exit(1)
    imgPath = sys.argv[1]
    main(imgPath)
