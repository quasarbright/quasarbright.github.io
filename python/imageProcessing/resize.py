import math
import colorsys
import numpy as np
from PIL import Image

def lerp(a, b, r: float):
    return a + (b-a)*r

def lerpColor(a, b, r: float):
    a = a / 256
    b = b / 256
    ahsv = np.array(colorsys.rgb_to_hsv(*a))
    bhsv = np.array(colorsys.rgb_to_hsv(*b))
    lerphsv = lerp(ahsv, bhsv, r)
    lerprgb = colorsys.hsv_to_rgb(*lerphsv)
    lerprgb = np.array(lerprgb)
    lerprgb = lerprgb * 256
    return lerprgb

def bilinear(arr: np.ndarray, shape: tuple) -> np.ndarray:
    oldH = arr.shape[0]
    oldW = arr.shape[1]

    newH = shape[0]
    newW = shape[1]
    newShape = list(arr.shape[:])
    newShape[0] = newH
    newShape[1] = newW

    resized = np.zeros(newShape)

    for newR in range(newH):
        oldR = newR * oldH / newH
        upInd = math.floor(oldR)
        downInd = min(math.ceil(oldR), oldH-1)
        # how far along the pixel is vertically between the old pixels
        if upInd == downInd:
            verticalLerpRatio = 0
        else:
            verticalLerpRatio = (oldR - upInd) / (downInd - upInd)
        for newC in range(newW):
            oldC = newC * oldW / newW
            rightInd = min(math.ceil(oldC), oldW-1)
            leftInd = math.floor(oldC)
            # how far along the pixel is horizontally between the old pixels
            if leftInd == rightInd:
                horizontalLerpRatio = 0
            else:
                horizontalLerpRatio = (oldC - leftInd) / (rightInd - leftInd)
            
            # four (old) pixel values
            ur = arr[upInd, rightInd]
            ul = arr[upInd, leftInd]
            dl = arr[downInd, leftInd]
            dr = arr[downInd, rightInd]

            # interpolate horizontally on top and bottom
            topHorizontalLerp = lerpColor(ul, ur, horizontalLerpRatio)
            bottomHorizontalLerp = lerpColor(dl, dr, horizontalLerpRatio)

            # now vertically interpolate between the two horizontal interpolations
            newPixel = lerpColor(topHorizontalLerp, bottomHorizontalLerp, verticalLerpRatio)
            resized[newR, newC, :] = newPixel

    resized = np.array(resized)
    resized = resized.astype('uint8')       
    return resized

def resize(imgPath: str, shape: tuple) -> Image:
    img = Image.open(imgPath)
    img.show()
    arr = np.asarray(img)
    resizedArr = bilinear(arr, shape)
    resizedImg = Image.fromarray(resizedArr)
    return resizedImg

imgPath = r'd:\OneDrive\Pictures\virgil.jpg'
resized = resize(imgPath, (800, 800))
resized.show()
