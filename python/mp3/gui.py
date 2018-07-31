from appJar import gui
app = gui('main menu', '400x400')
# for x in range(4):
#     app.addLabel(x, 'd')
app.startScrollPane('songPane', row=0, rowspan=3, colspan=3)
for x in range(4, 100):
    app.addLabel(str(x))
app.stopScrollPane()
with app.frame('left', row=3, column=0):
    app.addLabel('1')
with app.frame('mid', row=3, column=1):
    app.addLabel('2')
with app.frame('right', row=3, column=2):
    app.addLabel('3')
def stopFunction():
    print('k bye')
    return True
app.go()
