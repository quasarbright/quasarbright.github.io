#ifdef GL_ES
precision mediump float;
#endif

#extension GL_OES_standard_derivatives:enable

uniform float u_time;
uniform vec2 u_resolution;
const float PI = 3.1415926535897932384626433;
const int maxIter=200;

vec4 getColor(int escape, vec4[16] palette) {
  if(escape == maxIter) {
    return vec4(0.0,0.0,0.0,1.0);
  }
  int index = int(mod(float(escape+2), 16.0));
  // array index must be syntactically constant :(
  if(index==0){return palette[0];}
  if(index==1){return palette[1];}
  if(index==2){return palette[2];}
  if(index==3){return palette[3];}
  if(index==4){return palette[4];}
  if(index==5){return palette[5];}
  if(index==6){return palette[6];}
  if(index==7){return palette[7];}
  if(index==8){return palette[8];}
  if(index==9){return palette[9];}
  if(index==10){return palette[10];}
  if(index==11){return palette[11];}
  if(index==12){return palette[12];}
  if(index==13){return palette[13];}
  if(index==14){return palette[14];}
  if(index==15){return palette[15];}
  else{
    return palette[3];
  }
}

void main(void){
  vec4 palette[16];
  palette[0] = vec4(66./256.,30./256.,15./256.,1.);
  palette[1] = vec4(25./256.,7./256.,26./256.,1.);
  palette[2] = vec4(9./256.,1./256.,47./256.,1.);
  palette[3] = vec4(4./256.,4./256.,73./256.,1.);
  palette[4] = vec4(0./256.,7./256.,100./256.,1.);
  palette[5] = vec4(12./256.,44./256.,138./256.,1.);
  palette[6] = vec4(24./256.,82./256.,177./256.,1.);
  palette[7] = vec4(57./256.,125./256.,209./256.,1.);
  palette[8] = vec4(134./256.,181./256.,229./256.,1.);
  palette[9] = vec4(211./256.,236./256.,248./256.,1.);
  palette[10] = vec4(241./256.,233./256.,191./256.,1.);
  palette[11] = vec4(248./256.,201./256.,95./256.,1.);
  palette[12] = vec4(255./256.,170./256.,0./256.,1.);
  palette[13] = vec4(204./256.,128./256.,0./256.,1.);
  palette[14] = vec4(153./256.,87./256.,0./256.,1.);
  palette[15] = vec4(106./256.,52./256.,3./256.,1.);
  // float minZoom = 8.910478809532371e-05;
  // float zoom = minZoom;
  float zoomTime = 10.0; // time to reach max zoom
  float maxAmplitude = 30.0;
  float zoom=1.0/pow(1.5, maxAmplitude*(1.0-0.5*cos(2.0*PI * u_time / (zoomTime * 2.0)))-20.0);
  vec2 target = vec2(-0.1010965,0.9562865);
  //target = (target - resolution * 0.5) / max(resolution.x, resolution.y) * zoom;
  
  vec2 position=(gl_FragCoord.xy-u_resolution*.5)/max(u_resolution.x,u_resolution.y)*zoom;
  
  position=position+target;
  
  float cx=position.x;
  float cy=position.y;
  float x=0.;
  float y=0.;
  int escape=0;
  for(int i=0;i<=maxIter;i++){
    escape=i;
    float x_=x*x-y*y+cx;
    float y_=2.*x*y+cy;
    x=x_;
    y=y_;
    if(x*x+y*y>4.){
      break;
    }
  }


  gl_FragColor=getColor(escape, palette);
  // for debugging target centering
  // if(gl_FragCoord.x <= u_resolution.x / 2.0 && gl_FragCoord.y <= u_resolution.y / 2.0) {
  //   gl_FragColor = vec4(1.0,0.0,0.0,1.0);
  // }
  
}