class obj {
  constructor(x, y, name) {
    this.x = x;
    this.y = y;
    this.name = name;
  }

  draw(ctx) {
    ctx.font = "30px Arial";
    ctx.fillText(this.name, x, y);
  }
}

function startMakeFromModel(model) {
  console.log(model);

  const myCanvas = document.getElementById('myCanvas');
  myCanvas.width = window.innerWidth/1.2;
  myCanvas.height = window.innerHeight/1.2;

  createGrid(model.size["x"], model.size["y"], myCanvas);
  //TODO drawRobot(0, 0, myCanvas);
}

function clear(myCanvas) {
  const ctx = myCanvas.getContext('2d');
  ctx.clearRect(0, 0, myCanvas.width, myCanvas.height);
}

function createGrid(x,y, myCanvas) {
  const ctx = myCanvas.getContext('2d');
  ctx.beginPath();
  const w = myCanvas.width;
  const h = myCanvas.height;
  
  for(let i = 0; i <= w; i += w/x) {
    ctx.moveTo(i, 0);
    ctx.lineTo(i, h);
    ctx.stroke();
  }
  for(let j = 0; j <= h; j += h/y) {
    ctx.moveTo(0, j);
    ctx.lineTo(w, j);
    ctx.stroke();
  }
}

function drawRobot(x, y, myCanvas){
  const ctx = myCanvas.getContext('2d');
  ctx.beginPath();
  ctx.font = "30px Arial";
  ctx.fillText("R", x, y);
}