document.addEventListener('keypress', (event) => {
  const keyName = event.key
  if (keyName == "n") {
    const anchors = document.getElementsByClassName("ui basic button small svg uppercase")
    for (let anchor of anchors) {
      if (anchor.innerText == "NEXT") {
        anchor.click()
      }
    }
  }
  else if (keyName == "b") {
    const anchors = document.getElementsByClassName("ui basic button small svg uppercase")
    for (let anchor of anchors) {
      if (anchor.innerText == "PREV") {
        anchor.click()
      }
    }
  }
})
