// let world = []
// let next = []
// // let rows = 400
// // let cols = 400
// var dA = 1;
// var dB = 0.5;
// var feed = 0.055;
// var k = 0.062;
//
// function setup() {
//     createCanvas(200, 200);
//     pixelDensity(1)
//     // frameRate(1)
//     for (let i = 0; i < height; i++) {
//         let row = []
//         for (let j = 0; j < width; j++) {
//             row.push({
//                 a: 1,
//                 b: 0
//             })
//         }
//         world.push(row)
//         next.push(row.slice())
//     }
//     for (var i = 95; i < 105; i++) {
//         for (var j = 95; j < 105; j++) {
//             world[i][j].b = 1;
//         }
//     }
// }
//
// function draw() {
//     background(51);
//     // let w = width / cols
//     // let h = height / rows
//     // for (let i = 0; i < rows; i++) {
//     //     for (let j = 0; j < cols; j++) {
//     //
//     //     }
//     // }
//     // for (var x = 1; x < width - 1; x++) {
//     //     for (var y = 1; y < height - 1; y++) {
//     //         let a = world[y][x].a
//     //         let b = world[y][x].b
//     //         next[y][x].a = a +
//     //             (dA * laplaceA(world,y,x)) -
//     //             (a * b * b) +
//     //             (feed * (1 - a))
//     //         next[y][x].b = b +
//     //             (dB * laplaceB(world,y,x)) +
//     //             (a * b * b) -
//     //             ((k + feed) * b)
//     //             next[y][x].a = constrain(next.a,0,1)
//     //             next[y][x].b = constrain(next.b,0,1)
//     //     }
//     // }
//     for (var x = 1; x < width - 1; x++) {
//         for (var y = 1; y < height - 1; y++) {
//             var a = world[x][y].a;
//             var b = world[x][y].b;
//             next[x][y].a = a +
//                 (dA * laplaceA(x, y)) -
//                 (a * b * b) +
//                 (feed * (1 - a));
//             next[x][y].b = b +
//                 (dB * laplaceB(x, y)) +
//                 (a * b * b) -
//                 ((k + feed) * b);
//
//             next[x][y].a = constrain(next[x][y].a, 0, 1);
//             next[x][y].b = constrain(next[x][y].b, 0, 1);
//         }
//     }
//
//     loadPixels();
//     for (var x = 0; x < width; x++) {
//         for (var y = 0; y < height; y++) {
//             var pix = (x + y * width) * 4;
//             var a = next[x][y].a;
//             var b = next[x][y].b;
//             var c = floor((a - b) * 255);
//             c = constrain(c, 0, 255);
//             pixels[pix + 0] = c;
//             pixels[pix + 1] = c;
//             pixels[pix + 2] = c;
//             pixels[pix + 3] = 255;
//         }
//     }
//     updatePixels();
//     let temp = world
//     world = next
//     next = temp
// }
//
// function laplaceA(arr, r, c) {
//     let ans = 0
//     for (let i = -1; i <= 1; i++) {
//         for (let j = -1; j <= 1; j++) {
//             let weight
//             if (!i && !j) { //0,0
//                 weight = -1
//             } else if (i && j) { //corner
//                 weight = .05
//             } else { //edge
//                 weight = .2
//             }
//             try {
//                 ans += arr[r + i][c + j].a * weight
//             } catch (e) {
//                 console.log(e)
//             }
//         }
//     }
//     return ans
// }
//
// function laplaceB(arr, r, c) {
//     let ans = 0
//     for (let i = -1; i <= 1; i++) {
//         for (let j = -1; j <= 1; j++) {
//             let weight
//             if (!i && !j) { //0,0
//                 weight = -1
//             } else if (i && j) { //corner
//                 weight = .05
//             } else { //edge
//                 weight = .2
//             }
//             try {
//                 ans += arr[r + i][c + j].b * weight
//             } catch (e) {
//                 console.log(e)
//             }
//         }
//     }
//     return ans
// }


// Daniel Shiffman
// http://codingtra.in
// http://patreon.com/codingtrain
// Code for this video: https://youtu.be/BV9ny785UNc

// Written entirely based on
// http://www.karlsims.com/rd.html

// Also, for reference
// http://hg.postspectacular.com/toxiclibs/src/44d9932dbc9f9c69a170643e2d459f449562b750/src.sim/toxi/sim/grayscott/GrayScott.java?at=default

var grid;
var next;

var dA = 1;
var dB = 0.5;
var feed = 0.055;
var k = 0.062;

function setup() {
  createCanvas(200, 200);
  pixelDensity(1);
  grid = [];
  next = [];
  for (var x = 0; x < width; x++) {
    grid[x] = [];
    next[x] = [];
    for (var y = 0; y < height; y++) {
      grid[x][y] = {
        a: 1,
        b: 0
      };
      next[x][y] = {
        a: 1,
        b: 0
      };
    }
  }

  for (var i = 100; i < 110; i++) {
    for (var j = 100; j < 110; j++) {
      grid[i][j].b = 1;
    }
  }

}

function draw() {
  background(51);

  for (var x = 1; x < width - 1; x++) {
    for (var y = 1; y < height - 1; y++) {
      var a = grid[x][y].a;
      var b = grid[x][y].b;
      next[x][y].a = a +
        (dA * laplaceA(x, y)) -
        (a * b * b) +
        (feed * (1 - a));
      next[x][y].b = b +
        (dB * laplaceB(x, y)) +
        (a * b * b) -
        ((k + feed) * b);

      next[x][y].a = constrain(next[x][y].a, 0, 1);
      next[x][y].b = constrain(next[x][y].b, 0, 1);
    }
  }


  loadPixels();
  for (var x = 0; x < width; x++) {
    for (var y = 0; y < height; y++) {
      var pix = (x + y * width) * 4;
      var a = next[x][y].a;
      var b = next[x][y].b;
      var c = floor((a - b) * 255);
      c = constrain(c, 0, 255);
      pixels[pix + 0] = c;
      pixels[pix + 1] = c;
      pixels[pix + 2] = c;
      pixels[pix + 3] = 255;
    }
  }
  updatePixels();


  swap();


}


function laplaceA(x, y) {
  var sumA = 0;
  sumA += grid[x][y].a * -1;
  sumA += grid[x - 1][y].a * 0.2;
  sumA += grid[x + 1][y].a * 0.2;
  sumA += grid[x][y + 1].a * 0.2;
  sumA += grid[x][y - 1].a * 0.2;
  sumA += grid[x - 1][y - 1].a * 0.05;
  sumA += grid[x + 1][y - 1].a * 0.05;
  sumA += grid[x + 1][y + 1].a * 0.05;
  sumA += grid[x - 1][y + 1].a * 0.05;
  return sumA;
}

function laplaceB(x, y) {
  var sumB = 0;
  sumB += grid[x][y].b * -1;
  sumB += grid[x - 1][y].b * 0.2;
  sumB += grid[x + 1][y].b * 0.2;
  sumB += grid[x][y + 1].b * 0.2;
  sumB += grid[x][y - 1].b * 0.2;
  sumB += grid[x - 1][y - 1].b * 0.05;
  sumB += grid[x + 1][y - 1].b * 0.05;
  sumB += grid[x + 1][y + 1].b * 0.05;
  sumB += grid[x - 1][y + 1].b * 0.05;
  return sumB;
}



function swap() {
  var temp = grid;
  grid = next;
  next = temp;
}
