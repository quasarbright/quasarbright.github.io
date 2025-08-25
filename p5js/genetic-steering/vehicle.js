function Vehicle(pos, dna) {
    this.fmax = 2;

    this.dna = [];
    this.dna[0] = random(-this.fmax, this.fmax); //fmag
    this.dna[1] = random(-this.fmax, this.fmax); //pmag
    this.dna[2] = random(7); //vmax
    this.dna[3] = random(75); //frange
    this.dna[4] = random(75); //prange
    if (dna)
        this.dna = dna;



    this.mutate = function () {
        var vdna = [];
        vdna[0] = random(-this.fmax, this.fmax); //fmag
        vdna[1] = random(-this.fmax, this.fmax); //pmag
        vdna[2] = random(7); //vmax
        vdna[3] = random(75); //frange
        vdna[4] = random(75); //prange
        for (var i = 0; i < this.dna.length; i++) {
            if (random(1) < 0.05)
                this.dna[i] = vdna[i];
        }
    }
    this.mutate();

    this.pos = createVector(random(width), random(height));
    if (pos)
        this.pos = pos;

    this.vel = p5.Vector.random2D();
    this.acc = createVector(0, 0);

    this.r = 7;
    this.health = 1;

    this.show = function () {
        push();
        translate(this.pos.x, this.pos.y);
        rotate(this.vel.heading() + HALF_PI);
        var red = color(255, 0, 0);
        var green = color(0, 255, 0);
        var f = lerpColor(red, green, this.health);
        fill(f);
        // stroke(0);
        noStroke();
        beginShape();
        vertex(0, -this.r * 2);
        vertex(-this.r, this.r * 2);
        vertex(this.r, this.r * 2);
        vertex(0, -this.r * 2);
        endShape();
        if (debug) {
            strokeWeight(1);
            noFill();
            stroke(0, 255, 0, 200);
            line(0, 0, 0, this.dna[0] * -20);
            ellipse(0, 0, this.dna[3] * 2, this.dna[3] * 2);
            stroke(255, 0, 0, 200);
            line(0, 0, 0, this.dna[1] * -20);
            ellipse(0, 0, this.dna[4] * 2, this.dna[4] * 2);
        }
        pop();
    }

    this.pullTo = function (target, magnitude) {
        var diff = p5.Vector.sub(target, this.pos);
        diff.normalize();
        diff.mult(magnitude);
        this.acc.add(diff);
    }

    this.update = function () {
        this.health -= 0.005;
        this.health = constrain(this.health, 0, 1);

        if (this.health * random(1) > 1 - 0.05)
            vehicles.push(this.clone());

        this.bounds();
        this.acc.limit(this.fmax)
        this.vel.add(this.acc);
        this.vel.limit(this.dna[2]);
        this.pos.add(this.vel);
        this.acc.mult(0);
    }

    this.bounds = function () {
        var margin = 0;
        var bool = this.pos.x < margin;
        bool = bool || this.pos.x > width - margin;
        bool = bool || this.pos.y < margin;
        bool = bool || this.pos.y > height - margin;
        if (bool)
            this.pullTo(createVector(width / 2, height / 2), 1);
    }

    this.clone = function () {
        var newdna = [];
        for (var i = 0; i < this.dna.length; i++) {
            newdna.push(this.dna[i] + random(-0.05, 0.05));
        }
        return new Vehicle(this.pos.copy(), newdna);
    }

    this.behavior = function () {
        //food
        if (food.length > 0) {
            var record = Infinity;
            var closest = null;
            var closestInd = 0;
            //find closest element
            for (var i = 0; i < food.length; i++) {
                var difference = p5.Vector.sub(this.pos, food[i]);
                var distance = difference.mag();
                if (distance < record) {
                    record = distance;
                    closest = food[i];
                    closestInd = i;
                }
            }

            if (record < this.r) {
                food.splice(closestInd, 1);
                this.health += .5;
                this.health = constrain(this.health, 0, 1);
            } else {
                if (record < this.dna[3])
                    this.pullTo(closest, this.dna[0]);
            }
        }


        //poison
        if (poison.length > 0) {
            var record = Infinity;
            var closest = null;
            var closestInd = 0;
            //find closest element
            for (var i = 0; i < poison.length; i++) {
                var difference = p5.Vector.sub(this.pos, poison[i]);
                var distance = difference.mag();
                if (distance < record) {
                    record = distance;
                    closest = poison[i];
                    closestInd = i;
                }
            }

            if (record < this.r) {
                poison.splice(closestInd, 1);
                this.health -= 0.75;
                this.health = constrain(this.health, 0, 1);
            } else {
                if (record < this.dna[4])
                    this.pullTo(closest, this.dna[1]);
            }
        }

    }

}