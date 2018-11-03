from appJar import gui
import subprocess
import re, sys, os, shutil
THIS_FOLDER = os.path.dirname(os.path.abspath(__file__))
defaultSize = '378x265'

#============================== general purpose functions ======================
def relPath(f):
    f = f.split('/')
    return os.path.join(THIS_FOLDER,*f)
def runpy(f,*options):
    '''like runpy('tensorflow/retrain.py','-h')'''
    return subprocess.check_output([sys.executable, relPath(f),*options]).decode('utf-8')
def profiles():
    return os.listdir(relPath('profiles'))
def train(profile,image_dir,shouldPrint=False):
    profile = relPath('profiles/'+profile)
    shutil.rmtree(relPath('profiles/'+profile+'/summaries'))
    myargs = """'--image_dir', '{image_dir}', '--output_graph', '{profile}\\output_graph.pb', '--intermediate_output_graphs_dir', '{profile}\\intermediate_out', '--output_labels', '{profile}\\output_labels.txt', '--summaries_dir', '{profile}\\summaries', '--bottleneck_dir', '{profile}\\bottleneck_dir', '--how_many_training_steps', '1000', '--model_dir', '{profile}\\model_dir'""".format(image_dir=image_dir,profile=profile)
    myargs.split(', ')
    myargs = re.sub("'",'',myargs)
    myargs = myargs.split(', ')
    # print(myargs)
    # sys.exit()
    if shouldPrint:
        print(runpy('tensorflow/retrain.py',*myargs))
    else:
        runpy('tensorflow/retrain.py',*myargs)
    return None
# print(train('flowers','D:\\code\\flower_neural_network\\flower_photos'))
# sys.exit()
# =============================== app-specific stuff ===========================

def createProfile(name):
    name = re.sub(' ','_',name)
    match = re.sub(r'\w','',name)
    if match != '':
        try:
            app.errorBox('profile name error','profile names can only contain letters, numbers, and underscores',parent='add profile window')
        except AttributeError:
            pass
        return None
    name = relPath('profiles/'+name)
    print(name)
    try:
        os.mkdir(name)
        def mk(x):
            os.mkdir(os.path.join(name,x))
        mk('bottleneck_dir')
        mk('intermediate_out')
        mk('summaries')
        app.hideSubWindow('add profile window')
    except FileExistsError:
        try:
            app.errorBox('profile exists error','It appears a profile with that name already exists',parent='add profile window')
        except AttributeError:
            pass
        app.openSubWindow('add profile window')
        return False

#=================================== main gui ==================================

def press(button):
    image_dir = ''
    image_dir_selected = False
    if button == 'create':
        createProfile(app.getEntry('profile name'))
    elif button == 'add a profile':
        app.showSubWindow('add profile window')
        app.setFocus('profile name')
    elif button == 'train a profile':
        if len(profiles()) > 0:
            app.showSubWindow('train profile window')
        else:
            app.errorBox('no profiles error','To train, you need a profile. Try adding one')
    elif button == 'choose image directory':
        image_dir = app.directoryBox(title='select a directory', dirName=None, parent=None)
        image_dir_selected = True
        app.openSubWindow('train profile window')
        app.setLabel('image_dir',image_dir)
        app.addButton('train',press)
    elif button == 'train':
        profile = app.getOptionBox('train profiles option box')
        image_dir = app.getLabel('image_dir')
        if image_dir:
            app.openSubWindow('train profile window')
            app.addMessage('training msg','The neural network is training. please do not close any of the applcation windows or shut down your computer. If it is the first time training this profile, it could take over 30 minutes. Otherwise, it should take about 1-5 minutes')
            app.disableButton('train')
            def whenDone(*args):
                app.setMessage('training msg','')
                try:
                    app.infoBox('training done','training complete',parent="train profile window")
                except AttributeError:
                    pass
                app.hideSubWindow('train profile window')
            app.threadCallback(train,whenDone,profile,image_dir,shouldPrint=True)

#============================== main window ====================================

app = gui('app',defaultSize)
app.setIcon(relPath('MISTER-BRAINWASH.ico'))
app.addButton('add a profile',press)
app.addButton('train a profile',press)
# def checkStop():
#     sys.exit()
#     return True
# app.setStopFunction(checkStop)

#=========================== create a new profile ==============================

app.startSubWindow('add profile window',title='add',modal=True)
app.setSize(defaultSize)
app.addLabelEntry("profile name")
app.addButton('create',press)
app.stopSubWindow()

#============================= train a profile =================================

app.startSubWindow('train profile window',title="train",modal=True)
app.startLabelFrame('select profile')
app.addOptionBox('train profiles option box',profiles())
app.stopLabelFrame()
app.setSize(defaultSize)
app.addButton('choose image directory',press)
app.addLabel('image_dir','')
app.stopSubWindow()

app.go()

#test
