import os
script_dir = os.path.dirname(os.path.realpath(__file__))
try:
    os.mkdir(os.path.join(script_dir, 'songs'))
except FileExistsError:
    pass
