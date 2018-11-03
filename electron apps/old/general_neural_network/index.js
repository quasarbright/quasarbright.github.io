const {app,BrowserWindow,ipcMain} = require('electron');
let mainWindow;
app.on('ready', () => {
  mainWindow = new BrowserWindow({
    width: 800,
    height: 600
  });
  mainWindow.loadURL(__dirname + '/index.html');
  mainWindow.on('closed', function() {
    app.quit();
  });
});

function openWindow(url){
  //expects url relative to main directory
  let newWindow = new BrowserWindow({
    width:800,
    height:600
  })
  newWindow.loadURL(`${__dirname}/${url}`)
}
ipcMain.on('open-window',function(event,url){
  openWindow(url)
})

ipcMain.on('update-profiles',function(event,args){
  mainWindow.webContents.send('update-profiles')
})
