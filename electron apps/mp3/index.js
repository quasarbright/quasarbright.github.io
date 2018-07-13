const {
  app,
  BrowserWindow
} = require('electron');
let mainWindow;
app.on('ready', () => {
  mainWindow = new BrowserWindow({
    webPreferences: {
      nodeIntegration: false,
      preload: './preload.js'
    },
    Title: "window title",
    width: 800,
    height: 600
  });
  mainWindow.loadURL(__dirname + '/index.html');
  mainWindow.on('closed', function() {
    app.quit();
  });
});
