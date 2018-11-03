// let xhr = new XMLHttpRequest();
// xhr.onreadystatechange = handleStateChange; // Implemented elsewhere.
// function handleStateChange(){
//     if (this.readyState == 4 && this.status == 200) {
//         var obj = JSON.parse(this.responseText);
//         console.log(obj);
//     }
// }
// xhr.open("GET", chrome.extension.getURL('data.json'), true);
// xhr.send();

//try listing data.json under web accessible resources and
$('document').ready(()=>{
    $.getJSON(chrome.runtime.getURL("history.json"),(data)=>{
        console.log(data);
    });
});

// var xhr = new XMLHttpRequest;
// xhr.open("GET", chrome.runtime.getURL("history.json"));
// console.log(chrome.runtime.getURL("history.json"))
// xhr.onreadystatechange = function() {
//   if (this.readyState == 4) {
//     console.log("request finished, now parsing");
//     window.json_text = xhr.responseText;
//     window.parsed_json = JSON.parse(xhr.responseText);
//     console.log("parse results:");
//     console.log(window.parsed_json);
//   }
// };
// xhr.send();
