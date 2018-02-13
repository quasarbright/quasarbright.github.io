from appJar import gui

app = gui('title of the app','300x200')
def press(button):
    if button == 'click me':
        response = app.textBox('question box title','type something')
        app.infoBox('information box title','you typed: '+response)
app.addButton('click me',press)
app.go()
