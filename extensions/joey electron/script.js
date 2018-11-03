// let script = document.createElement("script");
// script.src = "https://cdnjs.cloudflare.com/ajax/libs/findAndReplaceDOMText/0.4.5/findAndReplaceDOMText.js";
// document.getElementsByTagName('head')[0].appendChild(script);
let elems = document.body.getElementsByTagName("*");
let replacements = conversions.replacements;
for (let replacement of replacements) {
    let trigger = RegExp(replacement[0],"gi");
    for (let ele of elems) {
        //for trigger, new in json
        let should_revert = false;
        let replacer = findAndReplaceDOMText(ele, {
            // find: /^((?!joey ))electron/i,
            // replace: "joey electron"
            find: trigger,
            // replace: new_str
            replace: function(portion, match) {
                let new_str;
                if(typeof(replacement[1]) === "string"){
                    new_str = replacement[1];
                } else {
                    new_str = replacement[1][Math.floor(Math.random()*replacement[1].length)];
                }
                if (portion.text.match(trigger)){
                    el = document.createElement('span');
                    el.title = "original: " + portion.text;
                    el.innerHTML="";
                    bold = document.createElement("B");
                    bold.innerHTML = new_str;
                    el.appendChild(bold);
                    // el.innerHTML = new_str;
                    return el;
                } else {
                    should_revert = true;
                }
            }
            // find: RegExp(trigger,gi),
            // replace: "<span title = trigger>"+//either new or random(new) if array
            // + "</span>";
        });
        if(should_revert){
            replacer.revert();
        }
    }
}
//TODO result arrays
//TODO trigger tooltip
//TODO make sure replacements are completely independent and happen in order of the JSON
