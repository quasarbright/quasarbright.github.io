const {
    app,
    BrowserWindow
} = require('electron');
let mainWindow;
app.on('ready', () => {
    mainWindow = new BrowserWindow({
        Title: "window title",
        width: 800,
        height: 600
    });
    mainWindow.loadURL(__dirname + '/index.html');
    mainWindow.on('closed', function() {
        app.quit();
    });
});
