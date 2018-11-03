const cmd = require('node-cmd')
const { spawn, execFile } = require('child_process');
require('jquery')
require('hammerjs')
require('materialize-css')
const fs = require('fs')
const {dialog} = require('electron').remote
const {ipcRenderer} = require('electron')
function openurl(url){
  ipcRenderer.send('open-window',url)
}
