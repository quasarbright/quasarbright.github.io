//https://www.sohamkamani.com/blog/2015/08/21/python-nodejs-comm/
const {app,BrowserWindow} = require('electron');
const {spawn} = require('child_process')
let mainWindow;
app.on('ready', () => {
  mainWindow = new BrowserWindow({
    Title:"window title",
    width: 800,
    height: 600
  });
  mainWindow.loadURL(__dirname + '/index.html');
  mainWindow.on('closed', function() {
    app.quit();
  });
});
let py = spawn('python', ['compute.py'])
py.stdout.on('data',function(data){
  console.log(data.toString())
})
py.stdout.on('end',function(){
  console.log('py done')
})
py.stdin.write('this is some input from index.js')
py.stdin.write('this is some more input from index.js')
py.stdin.end()
