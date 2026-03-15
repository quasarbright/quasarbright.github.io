/**
 * Shared simulation and rendering constants.
 * Tweak these to change the behaviour of the simulation.
 */

// ---------------------------------------------------------------------------
// Simulation
// ---------------------------------------------------------------------------

/** Number of rays emitted by a circular pulse. */
export const RAYS_PER_PULSE = 120;

/** Travel speed of all rays, in pixels per second. */
export const LIGHT_SPEED = 150;

/** Number of rays in a spotlight beam. */
export const SPOTLIGHT_COUNT = 120;

/** Pixel gap between adjacent spotlight rays. */
export const SPOTLIGHT_SPACING = 6;

/** Pixel distance between sibling ray heads that triggers insertion of a new ray. */
export const MAX_SIBLING_DISTANCE = 10;

/** Hard cap on total rays in the world. Insertion stops when this is reached. */
export const MAX_RAYS = 20000;

// ---------------------------------------------------------------------------
// Rendering
// ---------------------------------------------------------------------------

/** Radius of the dot drawn at each ray head, in pixels. */
export const RAY_DOT_RADIUS = 2;

/** Colour used for ray heads and sibling connectors. */
export const RAY_COLOR = "#FFD700";

/** Colour used for drawing optics (mirrors). */
export const MIRROR_COLOR = "#FFFFFF";

/** Half-length used when drawing "infinite" line mirrors, in pixels. */
export const MIRROR_EXTENT = 5000;

// ---------------------------------------------------------------------------
// Feature flags
// ---------------------------------------------------------------------------

/** When false, adaptive sibling insertion is skipped entirely. */
export const INSERTION_ENABLED = true;

/** Opacity of the background fade overlay each frame (0 = full trails, 1 = no trails). */
export const TRAIL_OPACITY = 0.05;

/** When false, ray-head dots are not drawn — only sibling connectors are shown. */
export const DRAW_DOTS = false;
export const SMOOTHING_ENABLED = true;

/** Fraction of the way to move toward the arc midpoint each tick (0 = off, 1 = snap). */
export const SMOOTHING_FACTOR = 0.05;
