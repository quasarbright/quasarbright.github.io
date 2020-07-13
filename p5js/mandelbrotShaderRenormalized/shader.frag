/*
coloring and renormalization copied from
https://www.shadertoy.com/view/4df3Rn
*/

#ifdef GL_ES
precision mediump float;
#endif

#extension GL_OES_standard_derivatives:enable

uniform float u_time;
uniform vec2 u_resolution;
const float PI = 3.1415926535897932384626433;
const int maxIter=512;
const float escapeRadius=2.5;
const float escapeRadiusSq=escapeRadius*escapeRadius;

float sigmoid(float x) {
  return 1.0 / (1.0 + exp(-x));
}

vec3 hsv2rgb(vec3 c)
{
  vec4 K=vec4(1.,2./3.,1./3.,3.);
  vec3 p=abs(fract(c.xxx+K.xyz)*6.-K.www);
  return c.z*mix(K.xxx,clamp(p-K.xxx,0.,1.),c.y);
}

vec2 rotate(vec2 v,float a){
  float s=sin(a);
  float c=cos(a);
  mat2 m=mat2(c,-s,s,c);
  return m*v;
}

float mandelbrot(vec2 position) {
  float cx=position.x;
  float cy=position.y;
  float x=0.;
  float y=0.;
  int escape=0;
  for(int i=0;i<=maxIter;i++){
    escape++;
    float x_=x*x-y*y+cx;
    float y_=2.*x*y+cy;
    x=x_;
    y=y_;
    if(x*x+y*y>escapeRadiusSq){
      break;
    }
  }

  // apparently this helps
  for(int i=0;i<2;i++){
    escape++;
    float x_=x*x-y*y+cx;
    float y_=2.*x*y+cy;
    x=x_;
    y=y_;
  }

  // float mu = float(escape)-(log(0.5*log(x*x+y*y)))/log(2.);
  float mu;
  if(escape>=maxIter){
    mu=0.;
  }else{
    mu=float(escape)-(log2(log2(x*x+y*y)))-4.;
  }
  return mu;
}

void main(void) {
  // float minZoom = 8.910478809532371e-05;
  // float zoom = minZoom;
  float zoomTime = 20.0; // time to reach max zoom
  float maxAmplitude = 30.0;
  float zoom=1.0/pow(1.5, maxAmplitude*(1.0-0.5*cos(2.0*PI * u_time / (zoomTime * 2.0)))-20.0);
  vec2 target = vec2(-.994822384150892528442,0.280591204141620403577);
  //target = (target - resolution * 0.5) / max(resolution.x, resolution.y) * zoom;
  
  vec2 position=(gl_FragCoord.xy-u_resolution*.5)/max(u_resolution.x,u_resolution.y)*zoom;
  
  float rotationSpeed = 2.0;
  float angle = rotationSpeed * (.5 - .5*cos(2.*PI*u_time / (zoomTime * 2.0)));
  position = rotate(position, angle);
  position=position+target;

  
  vec3 col=vec3(0.);
  float mu= mandelbrot(position);
  float hu = 0.5 - 0.5 * cos(mu / 20.0 - u_time / 2.0 - 2.0);
  gl_FragColor = vec4(hsv2rgb(vec3(hu, 1.0,.80)), 1.0);
  // col+=.5+.5*cos(3.+mu*.5*.15+vec3(0.,.5,1.));
  // gl_FragColor=vec4(col,1.);
  
  // mu /= 100.0;
  // // mu = mod(mu, 1.0);

  // // mu = sigmoid(mu);

  // gl_FragColor=vec4(hsv2rgb(vec3(mu,1.0,1.0)),1.0);
  // for debugging target centering
  // if(gl_FragCoord.x <= u_resolution.x / 2.0 && gl_FragCoord.y <= u_resolution.y / 2.0) {
  //   gl_FragColor = vec4(1.0,0.0,0.0,1.0);
  // }
  
}