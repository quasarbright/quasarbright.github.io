/*
coloring and renormalization copied from
https://www.shadertoy.com/view/4df3Rn
*/

#ifdef GL_ES
precision mediump float;
#endif


uniform float u_time;// the time in seconds
uniform vec2 u_resolution;// the display width and height
uniform vec2 u_mouse;// the pixel coordinate for the mouse pointer
uniform vec2 center;// the complex number for the center of the display
uniform float zoom;// how zoomed in the display should be
uniform vec2 c;// the current c-value used to generate the julia set
const float PI=3.1415926535897932384626433;
const int maxIter=256;
const float escapeRadius=2.5;
const float escapeRadiusSq=escapeRadius*escapeRadius;
const float BLACK_MU=-100000.;

float sigmoid(float x){
  return 1./(1.+exp(-x));
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

float mandelbrot(vec2 position){
  float cx=position.x;
  float cy=position.y;
  float x=cx;
  float y=cy;
  int escape=0;
  for(int i=0;i<=maxIter;i++){
    escape++;
    if(x*x+y*y>escapeRadiusSq){
      break;
    }
    float x_=x*x-y*y+cx;
    float y_=2.*x*y+cy;
    x=x_;
    y=y_;
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
  // if(interior) {
  //   return float(escape) - log2(abs(log2(x*x+y*y))) - 4.;
  // }
    mu=float(escape) - log2(abs(log2(x*x+y*y))) - 4.;//-(log2(log2(dx*dx+dy*dy)))-4.;
  // } else 
  // if(escape<maxIter) {
    // mu=float(escape)-(log2(log2(x*x+y*y)))-4.;
  // }else{
    // mu=BLACK_MU;
  // }
  return mu;
}

float julia(vec2 position,vec2 c){
  float x=position.x;
  float y=position.y;
  float cx=c.x;
  float cy=c.y;
  int escape;
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
  
  float mu;
  if(escape>=maxIter){
    mu=BLACK_MU;
  }else{
    mu=float(escape)-(log2(log2(x*x+y*y)))-4.;
  }
  return mu;
}

vec2 toComplex(vec2 pos){
  return(pos.xy-u_resolution*.5)/min(u_resolution.x,u_resolution.y)*3./zoom+center;
}

void main(void){  
  vec2 position=toComplex(gl_FragCoord.xy);
  
  // vec2 c = toComplex(u_mouse);
  
  float mu=mandelbrot(position);
  
  if(mu==BLACK_MU){
    gl_FragColor=vec4(0.,0.,0.,1.);
  }else{
    float hu=mu/10.;
    float offset=210./360.+u_time/50.;// blue
    hu=mod(-hu+offset,1.);
    float br_amp=.3;
    float br_period=2.;
    float br=((1.-br_amp)+br_amp*cos((mu-u_time)*2.*PI/br_period));
    gl_FragColor=vec4(hsv2rgb(vec3(float(hu),1.,br)),1.);
  }
}