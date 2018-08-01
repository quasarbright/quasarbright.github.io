import os
import subprocess
subprocess.call(['python', 'mappings.py'])
script_dir = os.path.dirname(os.path.realpath(__file__))
try:
    os.mkdir(os.path.join(script_dir, 'songs'))
except FileExistsError:
    pass
