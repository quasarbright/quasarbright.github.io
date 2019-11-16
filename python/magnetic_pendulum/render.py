from PIL import Image
import numpy as np
import physics

def render(size=3, resolution=(800,800)):
    '''
    size is -xmin, xmax, ymin, ymax
    resolution is (width, height)
    '''
    imgWidth, imgHeight = resolution
    xmin = -size
    xmax = size
    ymin = -size
    ymax = size
    if imgWidth > imgHeight:
        k = imgWidth / imgHeight
        xmin *= k
        xmax *= k
    else:
        k = imgHeight / imgWidth
        ymin *= k
        ymax *= k
    X = np.linspace(xmin, xmax, imgWidth)
    Y = np.linspace(ymin, ymax, imgHeight)

    world = physics.World(3)
    endings = [] # 2d array of (index, trace length)
    for y in Y:
        row = []
        for x in X:
            row.append(world.simulate(x, y))
        endings.append(row)
    colors = [] # 2d array of (r, g, b)
    for endingRow in endings:
        row = []
        for ending in endingRow:
            index, length = ending
            color = (0, 0, 0)
            if index == 0:
                color = (255, 0, 0)
            elif index == 1:
                color = (0, 255, 0)
            elif index == 2:
                color = (0, 0, 255)
            row.append(color)
        colors.append(row)
    colors = np.array(colors)
    colors = colors.astype('uint8')
    print(colors.dtype)
    return Image.fromarray(colors)

if __name__ == '__main__':
    img = render(resolution=(1920,1080))
    img.save('1080p magnetic pendulum.png')
    img.show()