let tabs_ = new Set([])
$('document').ready(function(){
  chrome.tabs.query({active:false,currentWindow:true}, function(tabs) {
    let $newdiv = $(`<div class="tablist"></div>`)
    let $ul = $('<ul class="collection"></ul>')
    for(let tab of tabs){
      tabs_.add(tab)
    }
    for(let tab of tabs_){
      $ul.append(`<li class="collection-item"><a href="${tab.url}" title="${tab.url}" target="_blank">${tab.title}</a></li>`)
    }
    $newdiv.append($ul)
    $('.container').append($newdiv)
  })
  $('#openall').click(function(){
    for(let tab of tabs_){
      chrome.tabs.create({url:tab.url})
    }
  })
  $('li>ul').parent().addClass('selected')
})
