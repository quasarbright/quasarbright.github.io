let's make a 2D light ray simulator using html ts css 

```typescript
type Vector = { x: number, y: number }

type Ray = {
    position: Vector
    velocity: Vector        // magnitude = speed
    leftSibling: Ray | null
    rightSibling: Ray | null
    optics: Optic[]         // optics interacted with so far, in order
}

interface Optic {
    isCollision(oldPosition: Vector, newPosition: Vector): boolean // true when oldPosition and newPosition are on opposite sides of object
    interact(ray: Ray, newPosition: Vector): void // ex: reflect the ray if we're a mirror. mirror: mutates velocity only. refractor: sets position to newPosition and refracts velocity.
}

type World = {
    rays: Ray[]
    optics: Optic[]
}
```

We track a doubly linked list of rays. We start out by sending a pulse of rays from a "light source".

This could be a point light source that emits a circle of rays, or maybe an arc of rays, or a spotlight that emits parallel rays beside each other.

We draw a line segment connecting two sibling ray "heads" if their list of optics are the same (reference equality on elements). We want to show the wave front, not just individual rays. If they are different, that means one was reflected and the other wasn't, which means they "split".

Per-tick update loop:
1. For each ray: compute `newPosition = position + velocity * dt`, check optics for collision (first found wins)
2. No collision → set `position = newPosition`
3. Collision → call `optic.interact(ray, newPosition)`, push optic to `ray.optics`
4. Check left sibling: if optics lists have diverged (any element differs at same index by reference), unlink both sibling pointers

For circular light sources, if we start out with dense wavefronts with ray heads close together, the wavefront actually looks like a circular arc. However, over time, they will become far apart and the circle will look like a polygon, which is bad. And eventually, if rays get really far apart, two siblings could pass around a mirror with neither of them colliding with it, making the simulation invalid.

To alleviate this: trace the lines formed by the rays (going forwards and backwards), treat the intersection of those lines as the center of a circle and the ray heads as points on the circle, and insert a new sibling in the middle of that arc. This should be fine if things never get too far apart, it's better than linear interpolating between ray heads since it accounts for curvature a little.

Let's do this in phases. Don't advance to the next phase until you get approval from me. Mark phases as complete (markdown checkbox) after I approve and we go to the next one.

have unit testing for this as we go. Test what you implement at each phase before prompting for approval.

- [x] Getting on the same page wrt design

- [x] Get set up

Create all data definitions.

Visualize propagating light from a circular light source without worrying about mirrors just to get the basics set up and so I have something to look at as we build on this.

Implement sibling connection (drawing connected siblings with a line segment)

No interpolation/insertion for now.

visually, let's do fullscreen black background with yellow light

draw dots for ray heads and line segments for connectors between connected siblings.

unit test basic circular pulse propagation, making sure position advances parallel to velocity, etc.

- [x] Line mirrors

Add support for (infinite) line mirrors. Create a box of line mirrors and let's see reflections inside the box

Draw mirrors in white

unit test reflection collision, reflection action, checking whether siblings are connected, etc.

- [ ] interactivity

click to spawn a circular light pulse at the mouse cursor

have a helper `addPulseAt(world, position)` and test that, don't test actual input handling.

- [ ] more types of mirrors

parabolic mirrors, circular mirrors, line segment mirrors

have some preset scenes and add a dropdown. one scene for the box, one scene for a circular mirror. one for a parabolic mirror.

test new mirror types, that their collision and reflection logic works properly.

- [ ] more light sources

add support for spotlight light sources (beam of parallel rays) instead of just a circular pulse.

parabolic mirror preset scene should have beam going right into the parabola so we can see the beams converge on the focus.

- [ ] advanced interpolation/insertion

when connected siblings get too far apart, trace the lines formed by the rays, treat the intersection as the center of a circle, and insert a new sibling in the middle of the arc formed by the heads and that center point.

- [ ] refraction

directed curves/line segments with an "inside" and "outside" and a refractive index of its inside.

glass block implemented as 4 line segments with consistent "inside" region and refractive index.

when going outside in vs inside out, change velocity/angle in opposite ways.

draw surface as light blue

create scene with glass block

add support for actual lens (math for collision and refraction might be tough?)

create a scene with actual lens and spotlight going into it focusing.

test line segment refractors from both directions (outside in and inside out)

test lens collision and refraction

---

possible future work. don't worry about this yet.

- [ ] walls

walls are red. they just delete rays. by nulling out sibling references appropriately. will have to be careful about the representative ray though. Might want to just go ECS at this point.

- [ ] dispersion

different colors of light. white light splits into different colors on a refraction boundary that get refracted differently according to wavelength. will break current data model for optics, which assumes that optics don't change the number of rays. Might want to just go ECS at this point.
