let jsonready = false;
$(document).ready(function() {
  //update selects with classes
  if (!jsonready) {
    //start_loading();
  }
  update_class_select();

  //enable material select
  $('select').material_select();

});

// loading animation for classes json
// let is_loading = false;
//
// function start_loading() {
//   if (!is_loading)
//     $('#json_loading').html('<div class="progress"><div class="indeterminate"></div></div>');
//   is_loading = true;
// }
//
// function stop_loading() {
//   $('#json_loading').empty().html('');
//   is_loading = false;
// }

//loads all classes to the selector html element
function update_class_select() {
  $.getJSON('classes.json', (data) => {
    jsonready = true;
    let $class_select = $('#class_select').empty().html(' ');
    classes = data.classes;
    for (let i = 0; i < classes.length; i++) {
      c = classes[i];
      $class_select.append($('<option></option>')
        .attr('value', i)
        .text(c.name)
      );
    }
    $('select').material_select();
    // let classes = $.getJSON('classes.json', function(data) {
    //   group_arr = data.class_arrays[0];
    // });
  });
}

function shuffle(arr) {
  a = arr.slice();
  let j, x, i;
  for (i = a.length - 1; i > 0; i--) {
    j = Math.floor(Math.random() * (i + 1));
    x = a[i];
    a[i] = a[j];
    a[j] = x;
  }
  return a;
}

function split_into_groups(a, n, option) {
  //option is supposed to be either merge or new
  //new: make a new group with extras
  //merge: add extras to existing groups

  if (!option)
    option = "merge";

  let shuffled = shuffle(a);
  let groups = []; //2D array

  while (shuffled.length >= n) {
    let group = [];
    for (let i = 0; i < n; i++) {
      group.push(shuffled.pop());
    }
    groups.push(group);
  }
  if (option == "merge") {
    for (let i = 0; i < shuffled.length; i++) {

      groups[i % groups.length].push(shuffled[i]);
    }
  } else if (option == "new") {
    if (shuffled.length)
      groups.push(shuffled);
  }
  return groups;
}

function get_val(id) {
  if ($("#" + id).is('select')) {
    return $("#" + id + " :selected").text();
  } else {
    return $("#" + id).val();
  }
}

let class_arr;
// function load_class_arr() {
//   let ans="j";
//   let timer = setInterval(() => {
//     ans = "k";
//     console.log(ans);
//   },500);
//   // let class_index = $('#class_select :selected').val();
//   // let get_group_arr.ans;
//   // let timer = setInterval(function(){
//   //   $.getJSON('classes.json', function(data){
//   //     console.log('json success')
//   //     get_group_arr.ans = data.classes[class_index].students;
//   //   });
//   //   console.log(get_group_arr.ans);
//   //   if (!get_group_arr.ans){
//   //     //start_loading();
//   //   }
//   //   if (get_group_arr.ans){
//   //     stop_loading();
//   //     console.log(get_group_arr.ans);
//   //     clearInterval(timer);
//   //   }
//   // }, 100);
//   // console.log(get_group_arr.ans)
//   // return get_group_arr.ans;
// }

//do group generation
function generate() {
  //update group_arr from selected json class
  let class_index = $('#class_select :selected').val();
  $.getJSON('classes.json', (data) => {
    class_arr = data.classes[class_index].students;
  });
  if (class_arr) {
    console.log(class_arr)
    let size = parseInt(get_val('group_size_text'));
    let mode = get_val('mode_select');

    let output_arr = split_into_groups(class_arr, size, mode);
    show_output(output_arr);
  } else {
    //handle if group_arr doesn't exist
    $('#out').empty().html('<div class="red white-text card">not ready, click generate again soon</div>')
  }
}

//display group generation
function show_output(output_arr) {
  let $out = $('#out').html(' ');
  for (group of output_arr) {
    $out.append('<div class="teal white-text card">' + group.join(', ') + '</div>');
  }

  //table output
  // let $out = $('#out').empty().html(' ');
  // let $table = $('<table></table>');
  // let $thead = $('<thead></thead>');
  // let $thr = $('<tr></tr>');
  // group_size = output_arr[0].length;
  // for (let i = 0; i < output_arr.length; i++) {
  //   $thr.append($('<th>Group ' + i + '</th>'));
  // }
  // $thead.append($thr);
  // $out.append($thead);
  // let $tbody = $('<tbody></tbody>');
  // for(group of output_arr){
  //
  // }
}
