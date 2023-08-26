// a lot of stuff copied from
// sebastian lague youtube
#ifdef GL_ES
precision mediump float;
#endif

#define MAX_OBJECTS 100

const float PI=3.1415926535897932384626433;
const float TAU = 2.0 * PI;
const int MAX_ITER = 200;
const int MAX_BOUNCES = 4;
const float MAX_DISTANCE = 100.0;
const float HIT_DISTANCE = 0.001;
const int RAYS_PER_PIXEL = 10;
const vec3 cameraPosition = vec3(0,1,0);
const vec3 cameraForward = normalize(vec3(1,-.3,0));
const float horizontalFov = 80.0;
const bool skyBoxEnabled = true;

uniform float u_time;// the time in seconds
uniform vec2 u_resolution;// the display width and height
uniform vec2 u_mouse;// the pixel coordinate for the mouse pointer
uniform vec2 center;// the complex number for the center of the display
uniform float zoom;// how zoomed in the display should be

struct Ray {
 vec3 position;
 vec3 direction;
};

struct Material {
  vec3 color;
  float specularProbability;
  vec3 emittedColor;
};

Material pureMirror() {
  return Material(vec3(1,1,1), 1.0, vec3(0));
}

Material diffuse(vec3 color) {
  return Material(color, 0.0, vec3(0));
}

Material diffuseMirror(vec3 color) {
  return Material(color, 1.0, vec3(0));
}

Material emissive(vec3 color) {
  return Material(vec3(0), 0.0, color);
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

// Gold Noise ©2015 dcerisano@standard3d.com
// - based on the Golden Ratio
// - uniform normalized distribution
// - fastest static noise generator function (also runs at low precision)
// - use with indicated fractional seeding method. 

float PHI = 1.61803398874989484820459;  // Φ = Golden Ratio   

float random(vec2 xy, float seed){
  return fract(tan(distance(xy*PHI, xy)*seed)*(xy.y+xy.x*u_resolution.x));
}

float randomNormal(vec2 xy, float seed) {
  // Thanks to https://stackoverflow.com/a/6178290
  float theta = 2.0 * 3.1415926 * random(xy, seed);
  float rho = sqrt(-2.0 * log(random(xy, seed + 1000.0)));
  return rho * cos(theta);
}

vec3 randomDirection(vec2 xy, float seed) {
  return normalize(vec3(randomNormal(xy, seed), randomNormal(xy, seed + 300.0), randomNormal(xy, seed + 400.0)));
}

struct Sphere {
  vec3 center;
  float radius;
  Material material;
};

struct Triangle {
  // order determines orientation, so it matters
  // normal is determined by right hand rule
  // (ABC counter clockwise)
  vec3 positionA;
  vec3 positionB;
  vec3 positionC;
  Material material;
};

struct HorizontalPlane {
  float y;
  Material material;
};

vec3 skyBox(vec3 direction) {
  vec3 groundColor = vec3(.5,.5,.5);
  vec3 skyHorizonColor = vec3(197, 218, 250) / 255.0;
  vec3 skyZenithColor = vec3(84, 150, 255) / 255.0;

  // no idea why, but we need to handle 0 separately
  if (direction.y < 0.0) {
    return groundColor;
  } else if (direction.y > 0.0) {
    float lerpRate = direction.y;
    return mix(skyHorizonColor, skyZenithColor, lerpRate);
  } else {
    return vec3(0);
  }
}

struct HitInfo {
  bool didHit;
  float dist;
  vec3 position;
  vec3 normal;
  Material material;
};

HitInfo miss() {
  return HitInfo(false, 0.0, vec3(0), vec3(0), diffuse(vec3(0)));
}

HitInfo sphereCollision(Ray ray, Sphere sphere) {
  vec3 fromCenter = ray.position - sphere.center;
  // from quadratic equation |ray.position + ray.direction * dist - center|^2 = radius^2
  float a = dot(ray.direction, ray.direction);
  float b = 2.0 * dot(fromCenter, ray.direction);
  float c = dot(fromCenter, fromCenter) - sphere.radius * sphere.radius;
  float discriminant = b * b - 4.0 * a * c;

  if(discriminant >= 0.0) {
    // use - sqrt to get nearest intersection
    float dist = (-b - sqrt(discriminant)) / (2.0 * a);
    
    // ignore intersections behind the ray
    // if + sqrt is positive, then we're in the sphere. ignore that too
    if (dist >= 0.0) {
      vec3 position = ray.position + ray.direction * dist;
      return HitInfo(true, dist, position, normalize(position - sphere.center), sphere.material);
    }
  }
  return miss();
}

HitInfo triangleCollision(Ray ray, Triangle triangle) {
  vec3 edgeAB = triangle.positionB - triangle.positionA;
  vec3 edgeAC = triangle.positionC - triangle.positionA;
  vec3 normal = cross(edgeAB, edgeAC);
  vec3 fromA = ray.position - triangle.positionA;
  vec3 fromACrossDirection = cross(fromA, ray.direction);

  float determinant = -dot(ray.direction, normal);
  float inverseDeterminant = 1.0 / determinant;

  float dist = dot(fromA, normal) * inverseDeterminant;
  float u = dot(edgeAC, fromACrossDirection) * inverseDeterminant;
  float v = -dot(edgeAB, fromACrossDirection) * inverseDeterminant;
  float w = 1.0 - u - v;

  bool didHit = determinant >= 1e-6 && dist >= 0.0 && u >= 0.0 && v >= 0.0 && w >= 0.0;
  if (!didHit) {
    return miss();
  }
  vec3 position = ray.position + ray.direction * dist;
  vec3 hitNormal = normalize(normal);
  // if you want vertices to have normals
  // vec3 hitNormal = normalize(triangle.normalA * w + triangle.normalB * u = triangle.normalC * v);
  return HitInfo(true, dist, position, hitNormal, triangle.material);
}

// can be used to combine objects. like a mesh is the closest hit info of a bunch of triangle hit infos
HitInfo closestHitInfo(HitInfo a, HitInfo b) {
  if(!a.didHit) {
    return b;
  } else if (!b.didHit) {
    return a;
  } else if (a.dist < b.dist) {
    return a;
  } else {
    return b;
  }
}

HitInfo sceneCollision(Ray ray) {
  return closestHitInfo(
    closestHitInfo(
      triangleCollision(ray, Triangle(vec3(9,5,0), vec3(10,-1,-5), vec3(10,-1,10), diffuseMirror(vec3(1,1,0.5)))),
      sphereCollision(ray, Sphere(vec3(6,0,-1), 1.0, diffuse(vec3(1,1,1))))
    ),
    sphereCollision(ray, Sphere(vec3(5,0,1), 1.0, emissive(vec3(1,1,1))))
  );
}

// return color
vec3 trace(Ray ray, int seed) {
  vec3 color = vec3(1,1,1);
  vec3 incomingLight = vec3(0);
  // otherwise, with only reflections and no misses, you get white light
  for(int reflections = 0; reflections < MAX_BOUNCES + 1; reflections++) {
    int seed = 3 * (seed + 1) + 5 * (reflections + 1);
    HitInfo hitInfo = sceneCollision(ray);
    if(hitInfo.didHit) {
      vec3 reflectedDirection;
      incomingLight += (hitInfo.material.emittedColor * color);
      color *= hitInfo.material.color;
      if(random(gl_FragCoord.xy, float(seed)) < hitInfo.material.specularProbability) {
        // specular reflection
        reflectedDirection = reflect(ray.direction, hitInfo.normal);
      } else {
        // diffuse reflection
        reflectedDirection = normalize(hitInfo.normal + randomDirection(gl_FragCoord.xy, float(seed)));
      }
      // step in the direction of the new ray to avoid getting trapped on a surface
      // if you're on the mirror, your distance is 0, and you will march 0 distance
      ray = Ray(hitInfo.position + reflectedDirection * HIT_DISTANCE, reflectedDirection);
    } else {
      // miss
      if (skyBoxEnabled) {
        incomingLight += skyBox(ray.direction) * color ;
      }
      break;
    }
  }
  return incomingLight;
}

// get the ray for the current pixel
Ray getRay() {
  vec3 up = vec3(0,1,0);
  // (a dot b) a
  // is the parallel component of b projected onto a
  // b - (a dot b) a
  // is the perpendicular component of b projected onto a
  // we want cameraUp to be cameraForward rotated 90 degrees towards the direction of up
  vec3 cameraUp = normalize(up - cameraForward * dot(cameraForward, up));
  vec3 cameraRight = cross(cameraForward, cameraUp);
  // physical size of image plane 1 unit in front of the camera to satisfy fov
  float imagePlaneWidth = 2.0 * tan(radians(horizontalFov) / 2.0);
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

  vec3 color = vec3(0);
  for(int i = 0; i < RAYS_PER_PIXEL; i++) {
    vec3 newColor = trace(ray, i);
    color += newColor * newColor;
  }
  color = pow(color / float(RAYS_PER_PIXEL), vec3(0.5,0.5,0.5));

  gl_FragColor = vec4(color,1);
}