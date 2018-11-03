from appJar import gui
# import webbrowser
# webbrowser.open('index.html')

def launch(win):
    app.showSubWindow(win)

app=gui('title','400x200')

# this is a pop-up
app.startSubWindow("one", modal=True)
app.addLabel("l1", "SubWindow One")
app.stopSubWindow()

# this is another pop-up
app.startSubWindow("two")
app.addLabel("l2", "SubWindow Two")
app.stopSubWindow()

# these go in the main window
app.addButtons(["one", "two"], launch)
app.addGrip(0,1)

app.go()
# from appJar import gui
#
# def showDate(btn):
#     print(app.getDatePicker("dp"))
#
# app=gui()
# app.addDatePicker("dp")
# app.addButton("GET", showDate)
# app.setDatePickerRange("dp", 1900, 2100)
# app.setDatePicker("dp")
# app.go()
