let image_dir
ipcRenderer.on('update-profiles',function(event,args){
  updateProfileSelect()
})
$('document').ready(function(){
  updateProfileSelect()
  $('select').change(function(){
    resetButtons()
    if($('select').val()){
      $('#choose').removeClass('disabled')
    }
  })
})
function resetButtons(){
  $('#dirname-out').html('')
  $('#choose').addClass('disabled')
  $('#begin').addClass('disabled')
}
function updateProfileSelect(){
  profiles = fs.readdirSync('profiles')
  $('select').html('<option value="">Choose profile</option>')
  for(let profile of profiles){
    $('select').append(`<option value="${profile}">${profile}</option>`)
  }
  $('select').material_select()
}
function loadOn(){
  $('#loading_bar_container').addClass('progress')
  $('#loading_bar').addClass('indeterminate')
  $('#loading-msg').html(`The neural network is training. Please do not close this or any of the application's windows until training is complete.`)
}
function loadOff(){
  $('#loading_bar_container').removeClass('progress')
  $('#loading_bar').removeClass('indeterminate')
  $('#loading-msg').html('')
}
function getDir(){
  let selectedPath = dialog.showOpenDialog({properties: ['openDirectory']})
  if(selectedPath){
    selectedPath = selectedPath[0]
  } else {
    return undefined
  }
  files = fs.readdirSync(selectedPath)
  fullFiles = files.map((e)=>selectedPath+'\\'+e)
  $('#dirname-out').html(selectedPath)
  image_dir = selectedPath
  $('#choose').addClass('disabled')
  $('#begin').removeClass('disabled')
}
function getSelected(){
  return $('select').val()
}
function begin(){
  profile = getSelected()
  if(profile == 0) {
    Materialize.toast('no profile selected',4000,'red')
    return undefined
  }
  let myargs = [`--image_dir`, `${image_dir}`, `--output_graph`, `profiles/${profile}/output_graph.pb`, `--intermediate_output_graphs_dir`, `profiles/${profile}/intermediate_out`, `--output_labels`, `profiles/${profile}/output_labels/output_labels.txt`, `--summaries_dir`, `profiles/${profile}/summaries`, `--bottleneck_dir`, `profiles/${profile}/bottleneck_dir`, `--how_many_training_steps`, `100`,'--model_dir',`profiles/${profile}/model_dir`]
  loadOn()
  const retrain = spawn('python/tensorflow/retrain.py',myargs)
  let failed = false
  retrain.stderr.on('data',(data)=>{
    $('#err').append(`<b>error while training</b><br><b>err: </b><code>${data}</code>`)
    loadOff()
    failed = true
  })
  retrain.on('close',(code)=>{
    loadOff()
    if(!failed)
      $('#out').html('training completed!')
  })
}
