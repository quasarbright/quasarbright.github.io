'''Render an image in braille characters
Each braille character is a 4x2 grid.

In each grid space, there can be a dot or nothing. This means each braille character is like a 4x2
black and white image with bit depth of 1

This means we can render an image as a grid of braille characters with dithering

The image will be broken down into little 4x2 sections, and based on what pixels are going to be on and off,
we decide which braille character to put there

The tricky part is converting from a grid of pixels to the right braille character.
Here is how unicode does it:
Each Position in the braille character's grid is numbered like this:
1 4
2 5
3 6
7 8
The reason for the bottom two being out of order is because braille used to just be 3x2, but they
added the bottom two dots and didn't want to change the encoding for 3x2 characters.
Anyway, unicode basically counts the dots like binary with the reading order described by the numbering. This means each
character can be encoded with 8 bits. For example:
hex: 41
binary:
0100 0001
8765 4321
 7      1
This means the top left and bottom left are dots
here's the actual braille character:    ⡁
here's a full braille character (FF):   ⣿  

And in unicode, each braille character is prefixed with 28, so this character would be U+2841

That's how you convert from binary to braille, but how do we go from knowing which dots we want to binary?
We go in reverse, recording each bit, building a binary string. For example:
⢦
The 8th position has a dot. 1
1
The 7th position has no dot. 0
10
the 6th position has a dot. 1
101
the 5th position has no dot. 0
1010
the 4th position has no dot. 0
1010 0
the 3rd position has a dot. 1
1010 01
the 2nd position has a dot. 1
1010 011
the 1st position has no dot. 0
1010 0110
Hex: A6
'''
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

def binaryToBraille(arr: np.ndarray) -> str:
    '''4x2 binary arr to braille
    '''
    # read the binary string in reverse braille order
    binaryString = ''
    binaryString += str(arr[3, 1])
    binaryString += str(arr[3, 0])
    binaryString += str(arr[2, 1])
    binaryString += str(arr[1, 1])
    binaryString += str(arr[0, 1])
    binaryString += str(arr[2, 0])
    binaryString += str(arr[1, 0])
    binaryString += str(arr[0, 0])

    hexString = hex(int(binaryString, 2))
    # now remove the leading '0x'
    hexString = hexString[2:]
    if len(hexString) == 1:
        # pad beginning with 0 if necessary
        hexString = '0' + hexString
    # unicode prefixes braille characters with 28
    hexString = '28' + hexString
    unicodeEncoding = int(hexString, 16)
    brailleCharacter = chr(unicodeEncoding)
    return brailleCharacter


def ditheredToBraille(arr: np.ndarray) -> str:
    '''Convert dithered array into braille characters

    Args:
        arr (numpy.ndarray): dithered array of binary values
            shape: (height, width) where
            height % 4 == 0
            width % 2 == 0
    
    Returns:
        str: braille characters
            newline-separated grid of braille characters for the image
            "shape" will be (height / 4, width / 2)
    
    Raises:
        ValueError: when dimensions are invalid
    '''
    arr = arr.astype('int')
    height, width = arr.shape
    if height % 4 != 0 or width % 2 != 0:
        raise ValueError(f"shape must be a multiple of 4x2: {(height, width)}")
    
    lines = []
    for i in range(height // 4):
        r = i*4
        line = ''
        for j in range(width // 2):
            c = j*2
            subArr = arr[r:r+4, c:c+2]
            brailleCharacter = binaryToBraille(subArr)
            line += brailleCharacter
        lines.append(line)
    braille = '\n'.join(lines)
    return braille

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
