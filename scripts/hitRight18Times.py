import time
from pynput import keyboard
from pynput.keyboard import Controller

if __name__ == '__main__':
    controller = Controller()
    time.sleep(1 - 1.0/60)
    for i in range(18):
        time.sleep(1.0/60)
        controller.press(keyboard.Key.right)
        # print('a')
