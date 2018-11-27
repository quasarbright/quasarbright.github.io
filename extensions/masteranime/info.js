// add myanimelist link
$('document').ready(function(){
  const titles = $('h1')[0].innerText
  const searchUrl = encodeURI(`https://myanimelist.net/search/all?q=${titles}`)
  const $searchAnchor = $(`<a href=${searchUrl} class="item">MAL search</a>`)
  let $infoTable = $('div.ui.info.list')
  let $item = $(`<div class="item"></div>`)
  $item.append($searchAnchor)
  $infoTable.append($item)
})
