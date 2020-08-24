#ifdef GL_ES
precision mediump float;
#endif

# define NUM_MAGNETS 3

const float PI=3.1415926535897932384626433;

uniform float u_time;// the time in seconds
uniform vec2 u_resolution;// the display width and height
uniform vec2 u_mouse;// the pixel coordinate for the mouse pointer
uniform vec2 center;// the complex number for the center of the display
uniform float zoom;// how zoomed in the display should be

uniform float offset;
uniform int brightness;

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

void main(void){
  gl_FragColor = vec4(1., 0., 1., 1.);
  vec2 position=toCoord(gl_FragCoord.xy);

  float hu = sin(position.x * 5.0) + sin(position.y * 5.0);
  hu = mod(hu + offset * 2.0 + u_time / 10.0, 1.0);
  float br = float(brightness) / 10.0;
  

  vec3 color = hsv2rgb(vec3(hu,1.0,br));
  gl_FragColor = vec4(color, 1.0);
}