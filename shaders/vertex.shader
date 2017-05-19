float factor;

void main(void)
{
   vec4 a = gl_Vertex;
   factor = 1;
   a.x = a.x * factor;
   a.y = a.y * factor;


   gl_Position = gl_ModelViewProjectionMatrix * a;

}   