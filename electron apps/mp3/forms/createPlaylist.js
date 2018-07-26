const $ = require('jquery')
$('document').ready(function(){
  makeCheckBoxes()
  $('form').submit(function(){

  })
})

function makeCheckBoxes(){
  $in = $('#included-tags-td')
  $ex = $('#excluded-tags-td')
  getTags(function(tags){
    for(let i = 0; i < tags.length; i++){
      let s = `<input type="checkbox" id="check${i}"`
    }
  })///// 7-26-18 left off here writing checkboxes for tags onto the addPlaylist form
}
