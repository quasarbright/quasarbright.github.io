import datetime
import pynput


script_start_time = datetime.datetime.now()
script_max_duration = 5 # in seconds
def get_how_long_running():
    dt = datetime.datetime.now() - script_start_time
    return dt.total_seconds()

with open('log.txt', 'a') as f:
    def on_move(x, y):
        out = '{0}, {1}, {2}\n'.format(x, y, datetime.datetime.now())
        f.write(out)
        if get_how_long_running() > script_max_duration:
            listener.stop()


    with pynput.mouse.Listener(on_move=on_move) as listener:
        listener.join()
