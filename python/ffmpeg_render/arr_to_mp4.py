import subprocess
import PIL
from PIL import Image
import numpy as np
import os
import shutil
script_path = os.path.dirname(os.path.realpath(__file__))
temp_img_dir_path = os.path.join(script_path, 'temp_imgs')

def arr_to_mp4(arr, output_path, framerate=30, temp_dir=temp_img_dir_path):
    '''
    arr shape should be (frames, height, width, 3)
    '''
    try:
        os.mkdir(temp_dir)
    except Exception as e:
        print(e)
    arr = arr.astype('uint8')
    for i in range(arr.shape[0]):
        imgarr = arr[i]
        img = Image.fromarray(imgarr)
        img.save(os.path.join(temp_dir, str(i)+'.png'))
    subprocess.call('ffmpeg -framerate {0} -i {1}/%d.png -pix_fmt yuv420p out.mp4'.format(framerate, temp_dir).split(' '))
    shutil.rmtree(temp_dir)

if __name__ == "__main__":
    arr = np.random.randint(0, 255, (120, 256, 256, 3), dtype="uint8")
    arr_to_mp4(arr, 'out.mp4')
    # produces out.mp4 which is 4 seconds long of image noise