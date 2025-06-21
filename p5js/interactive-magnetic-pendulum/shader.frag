#ifdef GL_ES
precision mediump float;
#endif

# define NUM_MAGNETS 3

struct magnet {
  vec2 pos;
  vec3 color;
};

struct result {
  magnet mag;
  int iterations;
  float traceLength;
};

const float PI=3.1415926535897932384626433;

uniform float u_time;// the time in seconds
uniform vec2 u_resolution;// the display width and height
uniform vec2 u_mouse;// the pixel coordinate for the mouse pointer
uniform vec2 center;// the complex number for the center of the display
uniform float zoom;// how zoomed in the display should be
uniform int maxIter;
uniform float kf;
uniform float km;
uniform float kp;
uniform float trap_radius; // actually radius squared
uniform float trap_velocity; // actually velocity squared
uniform float max_magnetism;
uniform float shading_strength;
uniform vec2 magnet_positions[NUM_MAGNETS]; // Positions of the magnets
uniform vec2 pendulum_center; // Position of the pendulum center

float sigmoid(float x){
  return 1./(1.+exp(-x));
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

magnet getClosestMagnet(magnet[NUM_MAGNETS] magnets, vec2 position) {
  magnet nearest = magnets[0];
  float nearestDistSq = 1e20;
  for(int index = 0; index < NUM_MAGNETS; index++) {
    magnet mag = magnets[index];
    vec2 disp = position - mag.pos;
    float dsq = dot(disp, disp);
    if(dsq < nearestDistSq) {
      nearest = mag;
      nearestDistSq = dsq;
    }
  }
  return nearest;
}

float getClosestMagnetDistSq(magnet[NUM_MAGNETS] magnets, vec2 position) {
  float nearestDistSq = 1e20;
  for(int index = 0; index < NUM_MAGNETS; index++) {
    magnet mag = magnets[index];
    vec2 disp = position - mag.pos;
    float dsq = dot(disp, disp);
    if(dsq < nearestDistSq) {
      nearestDistSq = dsq;
    }
  }
  return nearestDistSq;
}

result simulate_pde(magnet[NUM_MAGNETS] magnets, vec2 position) {
  vec2 velocity = vec2(0.);
  vec2 acceleration = vec2(0.);
  int iter = 0;
  float traceLength = 0.;
  for(int i = 0; i >= -1; i++) {
    if(i >= maxIter) {
      break;
    }
    iter = i;
    float nearestDsq = getClosestMagnetDistSq(magnets, position);
    if(nearestDsq < trap_radius && dot(velocity, velocity) < trap_velocity) {
      break;
    }
    vec2 force = vec2(0.);

    // friction
    force += velocity * -kf;

    // magnets (inverse square law)
    for(int index = 0; index < NUM_MAGNETS; index++) {

      magnet mag = magnets[index];
      vec2 displacement = mag.pos - position;
      float dsq = dot(displacement, displacement);
      vec2 temp = normalize(displacement) * km / dsq;
      if (dot(temp, temp) > max_magnetism*max_magnetism) {
        temp = normalize(temp) * max_magnetism;
      }
      force += temp;
    }

    // pendulum (hookes' law) - now using pendulum_center instead of origin
    force += (pendulum_center - position) * kp;

    acceleration = force;
    velocity += acceleration;
    position += velocity / 60.;
  }

  magnet nearest = getClosestMagnet(magnets, position);

  return result(nearest, iter, traceLength);
}

void main(void){
  gl_FragColor = vec4(1., 0., 1., 1.);
  magnet magnets[NUM_MAGNETS];
  for(int i = 0; i < NUM_MAGNETS; i++) {
    float hu = float(i) / float(NUM_MAGNETS);
    vec3 color = hsv2rgb(vec3(hu, 1., 1.));
    magnets[i] = magnet(magnet_positions[i], color);
  }

  vec2 position = toCoord(gl_FragCoord.xy);

  result res = simulate_pde(magnets, position);

  vec3 color = res.mag.color;
  color = rgb2hsv(color);
  color.z = 1.-pow(float(res.iterations) / float(maxIter), 2.) * shading_strength;
  color = hsv2rgb(color);
  gl_FragColor = vec4(color, 1.0);
} 