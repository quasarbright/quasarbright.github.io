from appJar import gui
with gui() as app:

    # app.startLabelFrame("Login Details")
    # # these only affect the labelFrame
    # app.setSticky("ew")
    # app.setFont(20)
    #
    # app.addLabel("l1", "Name", 0, 0)
    # app.addEntry("Name", 0, 1)
    # app.addLabel("l2", "Password", 1, 0)
    # app.addEntry("Password", 1, 1)
    # app.addButtons(["Submit", "Cancel"], None, 2, 0, 2)
    # app.stopLabelFrame()
    def press(button):
        if button == 'click me':
            app.infoBox('title','here is some info, ok?',parent='sub')
            app.hideSubWindow('sub')
    with app.subWindow('sub',modal=True):
        app.showSubWindow('sub')
        app.addButton('click me',press)
