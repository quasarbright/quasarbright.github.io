// hotkeys
document.addEventListener('keypress', (event) => {
  const keyName = event.key
  // n goes to next episode
  if (keyName == "n") {
    // find next button in the html and click it
    const anchors = document.getElementsByClassName("ui basic button small svg uppercase")
    for (let anchor of anchors) {
      if (anchor.innerText == "NEXT") {
        anchor.click()
      }
    }
  }
  // b goes to previous episode
  else if (keyName == "b") {
    // find prev button in the html and click it
    const anchors = document.getElementsByClassName("ui basic button small svg uppercase")
    for (let anchor of anchors) {
      if (anchor.innerText == "PREV") {
        anchor.click()
      }
    }
  }
})
