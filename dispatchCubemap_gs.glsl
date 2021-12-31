#version 330 core
layout (triangles) in;
layout (triangle_strip, max_vertices=18) out;

uniform mat4 projection;
uniform mat4 views[6];

out vec3 localPos;

void main()
{
    for(int face = 0; face < 6; ++face)
    {
        gl_Layer = face;
        for(int i = 0; i < 3; ++i)
        {
            localPos = gl_in[i].gl_Position.xyz;
            gl_Position = projection * views[face] * vec4(localPos, 1.0);
            EmitVertex();
        }    
        EndPrimitive();
    }
}

