const {app, BrowserWindow} = require('electron');
let mainWindow;
app.on('ready', () => {
  mainWindow = new BrowserWindow({
    webPreferences: {
      nodeIntegration: true,
      preload: './preload.js'
    },
    Title: "window title",
    width: 800,
    height: 600
  });
  mainWindow.loadURL('file://' + __dirname + '/index/index.html');
  mainWindow.on('closed', function() {
    app.quit();
  });
});
