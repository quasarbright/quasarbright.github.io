# making an electron app with materialize-css and jquery
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
# now the materialize and jquery stuff
8. run this stuff
```bash
npm install --save jquery
npm install --save hammerjs
npm install --save materialize-css
```
9. in your index.html add a link to materialize.css (or download and link):
```html
<link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/materialize/0.100.2/css/materialize.min.css">
```
10. in your renderer script (the one that operates from index.html) add this code to the beginning:
```js
require('jquery')
require('hammerjs')
require('materialize-css')
```
note: if you just want jquery you should only write `const $ = require('jquery')`
11. test is out by adding a `<button class="btn waves-effect">button</button>`and see if it works nicely
