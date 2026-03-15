let's make a 2D light ray simulator using html js (maybe ts) css 

```typescript
type Ray = {
    origin: Vector // where it started
    displacement: Vector // displacement from origin (parallel to velocity)
    velocity: Vector // direction of travel (parallel to displacement)
    leftSibling: Ray
    rightSibling: Ray
    optics: Optic[] // optics we've interacted with in order
}

interface Optic {
    isCollision(oldPosition: Vector, newPosition: Vector): boolean // true when oldPosition and newPosition are on opposite sides of object
    interact(ray: Ray): void // ex: reflect the ray if we're a mirror
}

type World = {
    representativeRay: Ray
    optics: Optic[]
}
```

We track a doubly linked list of rays. We start out by sending a pulse of rays from a "light source".

This could be a point light source that emits a circle of rays, or maybe an arc of rays, or a spotlight that emits parallel rays beside each other.

We draw a line segment connecting two sibling ray "heads" if their list of optics are the same. We want to show the wave front, not just individual rays. If they are different, that means one was reflected and the other wasn't, which means they "split".

We increase displacement by `velocity * dt` at each timestep. For circular light sources, if we start out with dense wavefronts with ray heads close together, the wavefront actually looks like a circular arc. However, over time, they will become far apart and the circle will look like a polygon, which is bad. And eventually, if rays get really far apart, two siblings could pass around a mirror with neither of them colliding with it, making the simulation invalid.

One way to alleviate this is, if we have two connected siblings with the same origin and they're too far apart, we can compute the circular arc that would form between them and insert a sibling in the middle of that arc between the two original siblings.

But if we have non-circular light sources or curved mirrors, the wavefront may not be a circle. connected siblings may have different origins because they got reflected at different angles from a curved mirror. One possible idea to overcome this is, trace the lines formed by the rays (going forwards and backwards), treat the intersection of those lines as the center of a circle and the ray heads as points on the circle, and the insertion happens in the middle of that arc formed by the heads and that center point. This should be fine if things never get too far apart, it's better than linear interpolating between ray heads since it accounts for curvature a little. If you have a better idea, I'm open to suggestions.

Let's do this in phases. Don't advance to the next phase until you get approval from me. Mark phases as complete (markdown checkbox) after I approve and we go to the next one.

have unit testing for this as we go. Test what you implement at each phase before prompting for approval.

- [ ] Getting on the same page wrt design

ask me clarifying questions if you have any, propose improvements to my design, and let's get on the same page and agree on the design

- [ ] Get set up

Create all data definitions.

Visualize propagating light from a circular light source without worrying about mirrors just to get the basics set up and so I have something to look at as we build on this.

Implement sibling connection (drawing connected siblings with a line segment)

No interpolation/insertion for now.

visually, let's do fullscreen black background with yellow light

draw dots for ray heads and line segments for connectors between connected siblings.

unit test basic circular pulse propagation, making sure displacement grows but stays parallel to velocity, etc.

- [ ] Line mirrors

Add support for (infinite) line mirrors. Create a box of line mirrors and let's see reflections inside the box

Draw mirrors in white

unit test reflection collision, reflection action, checking whether siblings are connected, etc.

- [ ] simple circular interpolation/insertion

when connected siblings with the same origin get far enough apart (custom distance threshold), insert a sibling between them in the middle of the circular arc formed by them centered at the origin. There are two possible arcs when you pick two points on a circle. pick the smaller arc, not the one that goes all the way around.

if connected siblings have different origins, don't insert ever.

unit test insertion after meeting threshold, and not triggering when different origin or under threshold or disconnected (due to different interacted optics)

- [ ] interactivity

click to spawn a circular light pulse at the mouse cursor

world now has a list of representative rays, one per light pulse

have a helper `addPulseAt(world, position)` and test that, don't test actual input handling.

- [ ] more types of mirrors

parabolic mirrors, circular mirrors, line segment mirrors

have some preset scenes and add a dropdown. one scene for the box, one scene for a circular mirror. one for a parabolic mirror.

test new mirror types, that their collision and reflection logic works properly.

- [ ] more light sources

add support for spotlight light sources (beam of parallel rays) instead of just a circular pulse.

parabolic mirror preset scene should have beam going right into the parabola so we can see the beams converge on the focus.

- [ ] do the more advanced interpolation/insertion

implement the more advanced interpolation/insertion that creates a circular arc based on the intersection of the connected sibling rays and the ray heads

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