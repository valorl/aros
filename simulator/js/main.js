window.addEventListener('load', eventWindowLoaded, false);
const preloaded = JSON.parse('{"walls":[{"sx":2,"sy":3,"x":1,"y":2},{"sx":1,"sy":1,"x":8,"y":8}],"targets":[{"x":9,"y":9}],"size":{"x":10,"y":10},"robot":{"x":0,"y":0},"instructions":["D","D","D","D","R","R","R","R","U","U","U","L","L","D","D","D","D","D"]}');

function eventWindowLoaded() {
  if(preloaded){
    document.getElementById("progfile").style.visibility = "hidden";
    startMakeFromModel(preloaded);
  } else {
    document.getElementById('progfile').addEventListener('change', processFile);
  }
}


function processFile(e){
  if(e){
    const file = e.target.files[0];
    readFileContent(file).then(content => {
      startMakeFromModel(processProgramTextToModel(content));
    }).catch(error => console.log(error))
  }
}

function readFileContent(file) {
	const reader = new FileReader()
  return new Promise((resolve, reject) => {
    reader.onload = event => resolve(event.target.result)
    reader.onerror = error => reject(error)
    reader.readAsText(file)
  })
}
