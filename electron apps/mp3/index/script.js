const $ = require('jquery')
$('document').ready(function(){
  $('#name-head').click()
  getSongs(function(songs){
    for(let song of songs){
      let $tr = $(`<tr>`)
      $tr.append(`<td class="tooltip">${song.name}<span class="tooltiptext">${song.tags.join('<br>')}</span></td>`)
      $tr.append(`<td>${song.bangericity}</td>`)
      $('#songs-list').append($tr)
      // $('#songs-list').append($(`<li>${song.name}</li>`))
    }
  })
})
