function print(str){
  $('document').ready(()=>{
    $('#out').append('<p>'+str+'</p>')
  })
}


let type = 'website'
let title = "The Imbrah Empire"
let author = 'Noah J. Tentacles'
let year = 2000
let month = "October"
let day = 31
let url = 'https://www.youtube.com/watch?v=73EEafBHgPs&index=1&list=FL9T_0TbYqxehVEfW2hcUPrA'



if (type === 'website'){
  //date
  let datestr = `(${year}, ${month[0].toUpperCase()+month.substring(1)} ${day})`
  //title
  let titlestr = `<i>${title}</i>`
  // print(titlestr)
  //url
  let urlstr = `Retrieved from <a href=${url}>${url}</a>`
  // print(urlstr)

  // print(datestr)
  if(author){
    //author name
    let authArr = author.split(' ')
    let lastName = authArr[authArr.length-1]
    lastName = lastName[0].toUpperCase() + lastName.substring(1)
    // console.log(lastName)
    let rest = authArr.slice(0,authArr.length-1).map(x => x[0].toUpperCase()+'.')
    let fullAuthstr = lastName + ', '
    fullAuthstr += rest.join(' ')
    // print(fullAuthstr)
    //full
    print(`${fullAuthstr} ${datestr}. ${titlestr}. ${urlstr}`)
  }else{
    print(`${titlestr}. ${datestr}. ${urlstr}`)
  }
}
