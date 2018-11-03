const cmd = require('node-cmd')
const $ = require('jquery')

$('document').ready(function(){
  $('#submit').click(function(){
    let command = $('#in').val()
    cmd.get(
      command,
      function(err, data, stderr){
        $('#out').html(`<code>${data}</code>`)
      }
    )
  })
})
