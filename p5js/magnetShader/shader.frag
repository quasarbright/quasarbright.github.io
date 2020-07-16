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

uniform float u_time;// the time in seconds
uniform vec2 u_resolution;// the display width and height
uniform vec2 u_mouse;// the pixel coordinate for the mouse pointer
uniform vec2 center;// the complex number for the center of the display
uniform float zoom;// how zoomed in the display should be
const float PI=3.1415926535897932384626433;

const int maxIter=256;
const float epsilon = .001;
const float epsilon_vel = .2;
const int min_iter = 10;

const float k_hookes = 5.0; // constant for hooke's law
const float k_mag = 2.0;
const float k_friction = 40.0; // friction constant
const float mass = 200.; // pendulum mass
const float dt = 30. / float(maxIter);

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

vec2 rotate(vec2 v,float a){
  float s=sin(a);
  float c=cos(a);
  mat2 m=mat2(c,-s,s,c);
  return m*v;
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

result simulate(magnet magnets[NUM_MAGNETS], vec2 position) {
  vec2 velocity = vec2(0.);
  vec2 acceleration = vec2(0.);

  int iter = 0;
  float traceLength = 0.;
  for(int i = 0; i <= maxIter; i++) {
    iter = i;

    float nearestDsq = getClosestMagnetDistSq(magnets, position);
    if (iter > min_iter && nearestDsq < epsilon && dot(velocity, velocity) < epsilon_vel) {
      break;
    }

    vec2 force = vec2(0.);

    // hooke's law on the pendulum
    vec2 force_hookes = -k_hookes * position;
    force += force_hookes;

    // sum over magnetic forces
    for(int index = 0; index < NUM_MAGNETS; index++) {
      magnet mag = magnets[index];
      vec2 disp = position - mag.pos;
      float denominator = pow(dot(disp, disp), 1.5);
      vec2 force_mag = -k_mag * disp / denominator;
      force += force_mag;
    }

    // friction on pendulum
    vec2 force_friction = -k_friction * velocity;
    force += force_friction;

    position += velocity * dt;
    traceLength += sqrt(dot(velocity, velocity));
    velocity += acceleration * dt;
    acceleration = force / mass;
  }

  magnet nearest = getClosestMagnet(magnets, position);

  return result(nearest, iter, traceLength);
}

result simulate_pde(magnet[NUM_MAGNETS] magnets, vec2 position) {
  float kf = .01;
  float km = .4;
  float kp = .1;


  vec2 velocity = vec2(0.);
  vec2 acceleration = vec2(0.);
  int iter = 0;
  float traceLength = 0.;
  for(int i = 0; i <= maxIter; i++) {
    iter = i;
    float nearestDsq = getClosestMagnetDistSq(magnets, position);
    if(nearestDsq < 0.0005 && dot(velocity, velocity) < 10.) {
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
      if (dot(temp, temp) > .25) {
        temp = normalize(temp) * .5;
      }
      force += temp;
    }

    // pendulum (hookes' law)
    force += position * -kp;

    acceleration = force;
    velocity += acceleration;
    traceLength += sqrt(dot(velocity, velocity)) / 60.;
    position += velocity / 60.;
  }

  magnet nearest = getClosestMagnet(magnets, position);

  return result(nearest, iter, traceLength);
}



void main(void){
  gl_FragColor = vec4(1., 0., 1., 1.);
  magnet magnets[NUM_MAGNETS];
  float jiggle_amp = .3;
  float jiggle_period = 10.;
  for(int i = 0; i < NUM_MAGNETS; i++) {
    float r = 1.+jiggle_amp*sin((u_time/jiggle_period-float(i)/float(NUM_MAGNETS))*(2.*PI)); 
    float hu = float(i) / float(NUM_MAGNETS);
    vec3 color = hsv2rgb(vec3(hu, 1., 1.));
    magnets[i]=magnet(rotate(vec2(r,0.),float(i)*2.*PI/float(NUM_MAGNETS)),color);
  }

  vec2 position=toCoord(gl_FragCoord.xy);

  result res = simulate_pde(magnets, position);

  vec3 color = res.mag.color;
  color = rgb2hsv(color);
  color.z = 1.-pow(float(res.iterations) / float(maxIter), 2.) * .8;
  // color.z = 1.-pow(float(res.iterations) / float(maxIter) / pow(dot(position, position), .08), 2.) * .8;
  // color.z = 1. / exp(log(float(maxIter)) / pow(float(maxIter), 2.0) * pow(float(res.iterations), 2.1));
  // color.z = exp(-0.02 * pow(float(res.traceLength), 1.6));
  // color.z = 1. - pow(res.traceLength * 20. / float(maxIter), .1);
  // color.z = 1. / exp(log(float(256)) / pow(100., 2.0) * pow(res.traceLength, 2.0));
  // color.z = 1. - (res.traceLength / float(maxIter));
  color = hsv2rgb(color);
  gl_FragColor = vec4(color, 1.0);

  // float nearestDist = getClosestMagnetDistSq(magnets, position);
  // if(nearestDist < epsilon) {
  //   gl_FragColor = vec4(1.);
  // }
}