import os
import sys
import shutil
import json
import re
import subprocess


scriptDir = sys.path[0]
cwd = os.getcwd()
if sys.platform == 'win32':
    cwdSplit = cwd.split('\\')
else:
    cwdSplit = cwd.split('/')
cwdName = cwdSplit[-1]


def scriptPath(path):
    '''returns a path relative to the script directory'''
    return os.path.join(scriptDir, path)


def cwdPath(path):
    '''returns a path relative to the current working directory'''
    return os.path.join(cwd, path)


# write package.json
with open(scriptPath('package.json'), 'r') as oldPackageJson:
    # get package.json
    fileString = oldPackageJson.read()
    jsonString = re.sub(r'[\n\r\t]+', '', fileString)
    jsonDict = json.loads(jsonString)

    # ask for package name
    newName = input('package name: ({0}) '.format(cwdName))
    # validate
    match = re.search('[^a-zA-Z0-9-_]', newName)
    if match:
        raise Exception(
            'Invalid package name. Can only contain letters, numbers, dashes, and underscores')
    if newName:
        jsonDict['name'] = newName
    else:
        # default
        jsonDict['name'] = cwdName

    # ask for description
    newDescription = input('description: ')
    jsonDict['description'] = newDescription
    with open(cwdPath('package.json'), 'w') as newPackageJson:
        newPackageJson.write(json.dumps(jsonDict, sort_keys=True, indent=4))


# copy index.js and index.html
shutil.copyfile(os.path.join(scriptDir, 'index.js'), os.path.join(cwd, 'index.js'))
shutil.copyfile(os.path.join(scriptDir, 'index.html'), os.path.join(cwd, 'index.html'))


# run npm install
if sys.platform == 'win32':
    subprocess.call(['npm', 'install'], shell=True)
else:
    subprocess.call(['npm', 'install'])
