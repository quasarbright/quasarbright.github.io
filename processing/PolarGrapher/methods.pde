float scl = 3;
PVector polar(float r, float theta){
  return new PVector(r*cos(theta),-r*sin(theta));//because +y down
}
void polarPoint(float r,float theta){
  point(r*cos(theta),-r*sin(theta));//because +y down
}
void pLine(PVector i, PVector f){
  line(i.x,i.y,f.x,f.y);
}
float r(float theta){
  float r = superShape(90.0/1,.2,1,1,theta);
  return r*height/scl;
}
float superShape(float m,float n1,float n2,float n3,float theta)
{
   float r;
   float t1,t2;
   float a=1;
   float b=1;

   t1 = cos(m * theta / 4) / a;
   t1 = abs(t1);
   t1 = pow(t1,n2);

   t2 = sin(m * theta / 4) / b;
   t2 = abs(t2);
   t2 = pow(t2,n3);

   r = pow(t1+t2,1/n1);
   if (abs(r) == 0) {
      return 0;
   } else {
      return 1 / r;
   }
}