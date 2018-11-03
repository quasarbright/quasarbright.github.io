const {
    app,
    BrowserWindow
} = require('electron');

let mainWindow;

function createMainWindow() {
    mainWindow = new BrowserWindow({
        title: "python gui prototype",
        width: 800,
        height: 600
    });
    mainWindow.loadURL(__dirname + '/index.html');
}

app.on('ready', createMainWindow);

mainWindow.on('closed', () => {
    // Dereference the window object, usually you would store windows
    // in an array if your app supports multi windows, this is the time
    // when you should delete the corresponding element.
    mainWindow = null
  })
}

app.on('activate', () => {
  // On macOS it's common to re-create a window in the app when the
  // dock icon is clicked and there are no other windows open.
  if (mainWindow === null) {
    createMainWindow();
  }
});
