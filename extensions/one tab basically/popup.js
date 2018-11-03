// chrome.tabs.create({url:"popup.js"})
$('document').ready(() => {
  $('#add').click(() => {
    isOpen = false
    chrome.tabs.query({
      active: false,
      currentWindow: true,
      title: 'not onetab'
    }, function(tabs) {
      isOpen = true
    })
    if (isOpen) {
      //update main.html under the current stuff
      
    } else {
      chrome.tabs.create({
        url: 'main.html'
      })
    }
  })
})
