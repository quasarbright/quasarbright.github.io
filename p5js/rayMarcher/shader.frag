#ifdef GL_ES
precision mediump float;
#endif

# define NUM_MAGNETS 3

const float PI=3.1415926535897932384626433;
const float TAU = 2.0 * PI;
const int MAX_ITER = 1000;
const int MAX_BOUNCES = 10;
const float MAX_DISTANCE = 10000.0;
const float HIT_DISTANCE = 0.0001;

uniform float u_time;// the time in seconds
uniform vec2 u_resolution;// the display width and height
uniform vec2 u_mouse;// the pixel coordinate for the mouse pointer
uniform vec2 center;// the complex number for the center of the display
uniform float zoom;// how zoomed in the display should be

uniform float offset;
uniform int brightness;

struct Ray {
 vec3 position;
 vec3 direction;
};

struct Material {
  vec3 color;
  float specularProbability;
};

Material pureMirror() {
  return Material(vec3(1,1,1), 1.0);
}

Material emissive(vec3 color) {
  return Material(color, 0.0);
}

float map(float x, float min1, float max1, float min2, float max2) {
  float d1 = max1 - min1;
  float d2 = max2 - min2;
  float disp1 = (x - min1) / d1;
  return min2 + disp1 * d2;
}

vec3 hsv2rgb(vec3 c)
{
  vec4 K=vec4(1.,2./3.,1./3.,3.);
  vec3 p=abs(fract(c.xxx+K.xyz)*6.-K.www);
  return c.z*mix(K.xxx,clamp(p-K.xxx,0.,1.),c.y);
}

vec3 rgb2hsv(vec3 c)
{
  vec4 K=vec4(0.,-1./3.,2./3.,-1.);
  vec4 p=mix(vec4(c.bg,K.wz),vec4(c.gb,K.xy),step(c.b,c.g));
  vec4 q=mix(vec4(p.xyw,c.r),vec4(c.r,p.yzx),step(p.x,c.r));
  
  float d=q.x-min(q.w,q.y);
  float e=1.e-10;
  return vec3(abs(q.z+(q.w-q.y)/(6.*d+e)),d/(q.x+e),q.x);
}

vec2 toCoord(vec2 pos) {
  return(pos.xy-u_resolution*.5)/min(u_resolution.x,u_resolution.y)*3./zoom+center;
}

// TODO rename
struct DistanceEstimation {
  float dist;
  vec3 normal;
  Material material;
};

struct Sphere {
  vec3 center;
  float radius;
  Material material;
};

// signed distance to sphere
DistanceEstimation sphereDistance(vec3 position, Sphere sphere) {
  return DistanceEstimation(
    length(position - sphere.center) - sphere.radius,
    normalize(position - sphere.center),
    sphere.material
  );
}

struct HorizontalPlane {
  float y;
  Material material;
};

DistanceEstimation horizontalPlaneDistance(vec3 position, HorizontalPlane plane) {
  return DistanceEstimation(
    position.y - plane.y,
    vec3(0,1,0),
    plane.material
  );
}

// use this to take the union of two objects in a scene
DistanceEstimation minDistanceEstimation(DistanceEstimation distEst1, DistanceEstimation distEst2) {
  if (distEst1.dist < distEst2.dist) {
    return distEst1;
  } else {
    return distEst2;
  }
}

// use this to take the intersection of two objects in a scene
DistanceEstimation maxDistanceEstimation(DistanceEstimation distEst1, DistanceEstimation distEst2) {
  if (distEst1.dist > distEst2.dist) {
    return distEst1;
  } else {
    return distEst2;
  }
}

// lower bound on distance to nearest object in the scene
DistanceEstimation distanceEstimation(vec3 position) {
  return minDistanceEstimation(
    // mirrors
    minDistanceEstimation(
    sphereDistance(position, Sphere(vec3(5,1,0), 1.0, pureMirror())),
    sphereDistance(position, Sphere(vec3(5,1,-2.1), 1.0, pureMirror()))
    ),
    // emitters
    minDistanceEstimation(
      sphereDistance(position, Sphere(vec3(5,2,2), 1.0, emissive(vec3(0,1,0)))),
      minDistanceEstimation(
      sphereDistance(position, Sphere(vec3(4,-1,-1), 1.0, emissive(vec3(0,0,1)))),
      horizontalPlaneDistance(position, HorizontalPlane(-2.0, emissive(vec3(1,0,0))))
    ))
  );
}

struct HitInfo {
  bool didHit;
  DistanceEstimation distanceEstimation;
  vec3 hitPosition;
};

// March the ray until it hits something
HitInfo calculateRayCollision(Ray ray) {
  float totalDistance = 0.0;
  for(int i = 0; i < MAX_ITER; i++) {
    if (totalDistance > MAX_DISTANCE) {
      break;
    } else {
      DistanceEstimation distEst = distanceEstimation(ray.position);
      float dist = distEst.dist;
      // close and going into the surface
      if (dist < HIT_DISTANCE && dot(distEst.normal, (ray.direction)) < 0.0) {
        return HitInfo(true, distEst, ray.position);
      } else {
        ray.position += ray.direction * dist;
      }
    }
  }
  return HitInfo(false, DistanceEstimation(0.0, vec3(0), emissive(vec3(0))), ray.position);
}

// return color
vec3 trace(Ray ray) {
  vec3 color = vec3(1,1,1);
  // otherwise, with only reflections and no misses, you get white light
  bool hitEmitter = false;
  for(int reflections = 0; reflections < MAX_BOUNCES + 1; reflections++) {
    HitInfo hitInfo = calculateRayCollision(ray);
    if(hitInfo.didHit) {
      if(hitInfo.distanceEstimation.material.specularProbability > 0.0) {
        // TODO random
        // specular reflection
        vec3 reflectedDirection = reflect(ray.direction, hitInfo.distanceEstimation.normal);
        // step in the direction of the new ray to avoid getting trapped on a surface
        // if you're on the mirror, your distance is 0, and you will march 0 distance
        ray = Ray(hitInfo.hitPosition + reflectedDirection * HIT_DISTANCE, reflectedDirection);
      } else {
        // diffuse reflection
        // TODO
        // for now, just 
        hitEmitter = true;
        color *= hitInfo.distanceEstimation.material.color;
        break;
      }
    } else {
      // miss
      color *= 0.0;
      break;
    }
  }
  if (!hitEmitter) {
    color *= 0.0;
  }
  return color;
}

// get the ray for the current pixel
Ray getRay() {
  vec3 cameraPosition = vec3(0,0,0);
  vec3 cameraForward = vec3(1,0,0);
  vec3 cameraUp = vec3(0,1,0);
  vec3 cameraRight = cross(cameraForward, cameraUp);
  float horizontalFov = 70.0;
  float verticalFov = 2.0 * atan(tan(horizontalFov / 2.0) * u_resolution.y / u_resolution.x);
  // physical size of image plane 1 unit in front of the camera to satisfy fov
  float imagePlaneWidth = 2.0 * tan(horizontalFov / 2.0);
  vec2 imagePlaneDimensions = vec2(imagePlaneWidth, imagePlaneWidth * u_resolution.y / u_resolution.x);
  vec2 imagePlaneCoords = vec2(
    map(gl_FragCoord.x, 0.0, u_resolution.x, -imagePlaneDimensions.x / 2.0, imagePlaneDimensions.x / 2.0),
    map(gl_FragCoord.y, 0.0, u_resolution.y, -imagePlaneDimensions.y / 2.0, imagePlaneDimensions.y / 2.0)
  );
  // vector from here to pixel. takes advantage of the fact that the image plane is 1 unit away (length of cameraForward)
  vec3 rayDirection = normalize(cameraForward + imagePlaneCoords.x * cameraRight + imagePlaneCoords.y * cameraUp);
  return Ray(cameraPosition, rayDirection);
}

void main(void) {
  Ray ray = getRay();
  gl_FragColor = vec4(trace(ray),1);

  // gl_FragColor = vec4(1., 0., 1., 1.);
  // vec2 position=toCoord(gl_FragCoord.xy);

  // float hu = sin(position.x * 5.0) + sin(position.y * 5.0);
  // hu = mod(hu + offset * 2.0 + u_time / 10.0, 1.0);
  // float br = float(brightness) / 10.0;

  // vec3 color = hsv2rgb(vec3(hu,1.0,br));
  // gl_FragColor = vec4(color, 1.0);
}