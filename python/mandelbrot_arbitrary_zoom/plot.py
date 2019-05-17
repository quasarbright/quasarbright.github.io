import p5
import mpmath as mp
import numpy as np
from matplotlib import pyplot as plt
import matplotlib.animation
from mandelbrot import steps_to_escape
# plt.imshow(np.random.rand(100,100), cmap='hot')
# plt.show()

size = 2
rmin = -size
rmax = size
imin = -size
imax = size
n = 256
nr = n
ni = n

max_steps = 100

zoom_target = np.complex(
    -1.74995768370609350360221450607069970727110579726252077930242837820286008082972804887218672784431700831100544507655659531379747541999999995,
    0.00000000000000000278793706563379402178294753790944364927085054500163081379043930650189386849765202169477470552201325772332454726999999995
)

def zs():
    r_list = np.linspace(rmin, rmax, nr)
    i_list = np.linspace(imin, imax, ni)
    ans = []
    for i in i_list:
        row = []
        for r in r_list:
            row.append(np.complex(r, i))
        ans.append(row)
    return ans

def steps():
    ans = []
    for row in zs():
        newRow = []
        for z in row:
            newRow.append(steps_to_escape(z, max_steps))
        ans.append(newRow)
    return ans

def steps_to_img():
    # just normalize
    ans = []
    for row in steps():
        newRow = []
        for z in row:
            newRow.append(z / max_steps)
        ans.append(newRow)
    return ans

def plot():
    steps = steps_to_img()
    plt.imshow(steps, cmap='hsv')
    plt.show()

def animate_zoom():
    fig, ax = plt.subplots()
    imgs = []
    n_images = 50
    for i in range(n_images):
        imgs.append(steps_to_img())
        zoomToPoint(zoom_target, .3)
    im_figs = []
    for img in imgs:
        im_figs.append([plt.imshow(img, animated=True, cmap="hsv")])
    # im = plt.imshow([imgs[0]])
    def init():
        return im,

    def animate(i):
        im.set_data(imgs[i])
        return im,

    ani = matplotlib.animation.ArtistAnimation(fig, im_figs, interval=300, blit=True,
                                    repeat_delay=0)

    plt.show()

def zoomToPoint(z, lam):
    global rmin, rmax, imin, imax
    rmin += lam * (z.real - rmin)
    rmax += lam * (z.real - rmax)
    imin += lam * (z.imag - imin)
    imax += lam * (z.imag - imax)


if __name__ == "__main__":
    animate_zoom()
    
