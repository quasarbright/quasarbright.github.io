const {app, BrowserWindow, ipcMain} = require('electron');
const storage = require('electron-storage')
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
  mainWindow.loadURL('file://' + __dirname + '/index.html');
  mainWindow.on('closed', function() {
    app.quit();
  });
});
