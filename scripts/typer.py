import sys
import pynput

controller = pynput.keyboard.Controller()
word = ""
controller.type("test")
def on_press(key):
    print(key)
    if key == pynput.keyboard.Key.esc:
        sys.exit()



with pynput.keyboard.Listener(on_press=on_press) as listener:
    listener.join()
#
