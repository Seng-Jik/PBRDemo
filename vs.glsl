#version 330 core

layout (location = 0) in vec3 aPos;
layout (location = 1) in vec3 aNormal;
layout (location = 2) in vec2 aTexCoord;
layout (location = 3) in vec3 aTangent;

uniform mat4 projection, view, model;

out vec3 worldPos;
out vec3 worldNormal;
out vec2 texCoord;
out mat3 TBN;

void main() {
	vec4 worldPosV4 = model * vec4(aPos, 1.0);
	worldPos = worldPosV4.xyz;
	worldNormal = transpose(inverse(mat3(model))) * aNormal;
	texCoord = aTexCoord;

	vec3 T = normalize(vec3(model * vec4(aTangent,   0.0)));
    vec3 N = normalize(vec3(model * vec4(aNormal,    0.0)));
	T = normalize(T - dot(T, N) * N);
	vec3 B = cross(T, N);
    TBN = mat3(T, B, N);

	gl_Position = projection * view * worldPosV4;
}
