# Making an Electron app
[video](https://www.google.com/search?q=building+an+electron+app&ie=utf-8&oe=utf-8&client=firefox-b-1-ab#kpvalbx=1)
1. Make a directory
2. Cd to it
3. `npm init`
4. `npm install --save electron`
5. Add "start": "electron .", in scripts before test in package.json:
```js
"scripts": {
    "start": "electron .",
    "test": "echo \"Error: no test specified\" && exit 1"
  },
```
6. create index.js:
```js
const {app,BrowserWindow} = require('electron');
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
```
7. Create an index.html file that displays your app
