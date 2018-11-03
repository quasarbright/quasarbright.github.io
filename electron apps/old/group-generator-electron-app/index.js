const electron = require('electron');
const app = electron.app;
const BrowserWindow = electron.BrowserWindow;
const ipcMain = electron.ipcMain;
app.on('ready', () => {
  const mainWindow = new BrowserWindow({
    title:"group generator",
    width: 800,
    height: 600
  });
  mainWindow.loadURL(__dirname + '/index.html');
  mainWindow.on('closed', function() {
    app.quit();
  });
});

function createEditWindow(){
  editWindow = new BrowserWindow({
    title:"edit classes",
    width: 800,
    height: 600//left off here 11-20-17
  });
  editWindow.on('closed',()=>{
    ;//update class select
  });
}
