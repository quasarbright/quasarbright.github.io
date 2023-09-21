/**
 * A framework for user-customizable parameters.
 */

// A Slidable is a () -> number
// It represents a quantity that is customized by the user via UI.

/**
 * Create a slidable value.
 *
 * @param start initial value
 * @param min minimum value
 * @param max maximum value
 * @param step step size
 * @param label text to show before slider
 * @param id slider html id. must be unique.
 * @return {function(): number} (Slidable)
 */
function slidable(start, min, max, step, label, id) {
    makeSlider(start, min, max, step, label, id)
    return () => getSliderValue(id)
}

/**
 * Create a labelled UI slider with the specified parameters.
 * Assumes there is a div with id "slidable"
 */
function makeSlider(start, min, max, step, label, id) {
    function update() {
        console.log("update")
        document.getElementById(`${id}-p`).innerHTML = `${label}: ${getSliderValue(id)}`
    }
    $(document).ready(() => {
        console.log($("#slidable"))
        $("#slidable").append(
            `<div><p id="${id}-p">${label}: ${getSliderValue(id)}</p><input type="range" min="${min}" max="${max}" value="${start}" step="${step}" id="${id}" onchange="document.getElementById(\`${id}-p\`).innerHTML = \`${label}: ${getSliderValue(id)}\`"></div>`
        )
    })
}

/**
 * Get the value of the slider by html id.
 *
 * @param id html id of the slider
 */
function getSliderValue(id) {
    if (document.getElementById(id)) {
        return document.getElementById(id).value
    } else {
        return 0
    }
}