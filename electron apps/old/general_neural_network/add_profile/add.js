function add_profile(){
  let name = $('#profile-input').val()
  if(name){
    name = name.replace(/ /g,'_')
    leftover = name.replace(/\w/g,'')
    if(leftover){
      Materialize.toast('profile names can only contain letters, numbers, and underscores',4000,'red')
    } else {
      status = prepare_dirs(name)
      if(status){
        Materialize.toast($(`<span>created profile </span>`).add(`<button class="btn-flat toast-action">${name}</button>`),4000)
      }
    }
  }
}
function prepare_dirs(name){
  let files = fs.readdirSync(`profiles`)
  for(let directory of files){
    if(directory == name){
      Materialize.toast('It appears you already have a profile with that name',4000)
      return undefined
    }
  }
  function mkdir(path){
    cmd.get(
      `mkdir ${path}`,
      function(err,data,stderr){
        if(err || stderr)
          console.log('mkdir in startup. err:',err,'data:',data,'stderr:',stderr)
        ipcRenderer.send('update-profiles')
      }
    )
  }
  let pre = `profiles\\${name}`
  let dirs = ['bottleneck_dir','intermediate_out','summaries']
  for(let suf of dirs){
    mkdir(`${pre}\\${suf}`)
  }
  return true
}
