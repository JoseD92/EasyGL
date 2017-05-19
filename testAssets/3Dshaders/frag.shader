varying vec2 coord;
varying vec3 norm;
uniform sampler2D sampler01;
uniform int use;
void main (void)
{
   if (use==0) gl_FragColor = vec4 (coord.x,coord.y,0,1);
   if (use==1) gl_FragColor = texture2D(sampler01, coord);
   if (use==2) gl_FragColor = vec4 (norm.x,norm.y,norm.z,1);
}
