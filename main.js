var node = document.getElementById('conways');
var app = Elm.Conways.embed(node);
var canvas = document.getElementById('board');
app.ports.render.subscribe(function(list) {
  var ctx = canvas.getContext('2d');
  list.forEach(function(cell) {
    ctx.fillStyle = cell.status === true ? 'red' : 'white';
    ctx.fillRect(cell.x * 10, cell.y * 10, 10, 10);
  });
});

document.addEventListener('DOMContentLoaded', function() {
  canvas.width = 200;
  canvas.height = 200;
  canvas.style.width = '200px';
  canvas.style.height = '200px';
});
