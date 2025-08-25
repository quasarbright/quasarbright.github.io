var food = [];
var poison = [];
var vehicles = [];
var popsize = 40;
var poisonsize = 200;
var foodsize = 500;

var debug = false; // Debug mode off
var frate = 0.4; // Set fixed frame rate value

function setup() {
    // Remove UI controls and make canvas fullscreen
    createCanvas(windowWidth, windowHeight);

    for (var j = 0; j < popsize; j++) {
        vehicles.push(new Vehicle());
    }

    for (var i = 0; i < 100; i++) {
        food.push(createVector(random(width), random(height)));
    }

    for (var k = 0; k < 50; k++) {
        poison.push(createVector(random(width), random(height)));
    }
}

function draw() {
    background(0);

    for (var k = vehicles.length - 1; k >= 0; k--) {
        vehicles[k].update();
        vehicles[k].show();
        vehicles[k].behavior();
        if (vehicles[k].health == 0) {
            food.push(vehicles[k].pos);
            vehicles.splice(k, 1);
        }
    }

    if (random(1) < frate)
        food.push(createVector(random(width), random(height)));
    while (poison.length < poisonsize)
        poison.push(createVector(random(width), random(height)));

    for (var i = 0; i < food.length; i++) {
        stroke(0, 255, 0);
        strokeWeight(5);
        point(food[i].x, food[i].y);
    }
    for (var j = 0; j < poison.length; j++) {
        stroke(255, 0, 0);
        strokeWeight(5);
        point(poison[j].x, poison[j].y);
    }
}

// Handle window resize to maintain fullscreen
function windowResized() {
    resizeCanvas(windowWidth, windowHeight);
}