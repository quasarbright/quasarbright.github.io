from appJar import gui

app = gui('app title','400x300')
def press(button):
    if button == 'click me':
        response = app.textBox('a question box','type something')
        app.infoBox('an info box','you typed: '+response)
app.addButton('click me',press)

app.go()
