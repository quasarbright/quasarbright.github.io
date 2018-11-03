ipcRenderer.on('update-profiles',function(event,args){
  console.log('received')
  update()
})
function update() {
  $('document').ready(() => {
    let profiles = fs.readdirSync('profiles')
    if (profiles.length) {
      $('#train-btn').removeClass('disabled')
    } else {
      $('#train-btn').addClass('disabled')
    }
  })
}
update()
