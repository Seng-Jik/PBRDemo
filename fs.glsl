#version 330 core

out vec4 FragColor;

in vec3 worldPos;
in vec3 worldNormal;
in vec2 texCoord;
in mat3 TBN;

uniform vec3 camPos;

uniform sampler2D albedoTex;
uniform sampler2D aoTex;
uniform sampler2D metallicTex;
uniform sampler2D normalTex;
uniform sampler2D roughnessTex;
uniform samplerCube irradianceMap;
uniform samplerCube specularMap;
uniform sampler2D brdfLut;

struct Light {
	vec3 WorldPosition;
	vec3 Color;
};

uniform Light lights[8];

const float PI = 3.14159265359;

vec3 fresnelSchlick(float cosTheta, vec3 F0, float roughness) {
	return F0 + (max(vec3(1.0 - roughness), F0) - F0) * pow(1.0 - cosTheta, 5.0);
}

float distributionGGX(vec3 N, vec3 H, float roughness) {
	float a = roughness * roughness;
	float a2 = a * a;
	float NdotH = max(dot(N, H), 0.0);
	float NdotH2 = NdotH * NdotH;
	
	float nom = a2;
	float denom = (NdotH2 * (a2 - 1.0) + 1.0);
	denom = PI * denom * denom;
	
	return nom / denom;
}

float geometrySchlickGGX(float NdotV, float roughness) {
	float r = roughness + 1.0;
	float k = (r * r) / 8.0;
	float nom = NdotV;
	float denom = NdotV * (1.0 - k) + k;
	return nom / denom;
}

float geometrySmith(vec3 N, vec3 V, vec3 L, float roughness) {
	float NdotV = max(dot(N, V), 0.0);
	float NdotL = max(dot(N, L), 0.0);

	float ggx1 = geometrySchlickGGX(NdotV, roughness);
	float ggx2 = geometrySchlickGGX(NdotL, roughness);
	return ggx1 * ggx2;
}

const float MAX_REFLECTION = 4.0;

void main() {
	vec3 V = normalize(camPos - worldPos);	

	vec3 albedo     = texture2D(albedoTex, texCoord).rgb;
	albedo.x = pow(albedo.x, 2.2);
	albedo.y = pow(albedo.y, 2.2);
	albedo.z = pow(albedo.z, 2.2);

	vec3 N = normalize(texture2D(normalTex, texCoord).xyz * 2.0 - vec3(1.0));
	N = vec3(0.0, 0.0, 1.0);
    N = TBN * N;

	vec3 R = reflect(-V, N);

	vec3 irradiance = texture(irradianceMap, N).rgb;

    float metallic  = texture2D(metallicTex, texCoord).r;
    float roughness = texture2D(roughnessTex, texCoord).r;
    float ao        = texture2D(aoTex, texCoord).r;

	vec3 prefilteredColor = textureLod(specularMap, R, roughness * MAX_REFLECTION).rgb;

	vec3 F0 = vec3(0.04);

	vec3 Lo = vec3(0.0);
	for(int i = 0; i < 8; ++i) {
		vec3 L = normalize(lights[i].WorldPosition - worldPos);
		vec3 H = normalize(V + L);
		
		float distance = length(lights[i].WorldPosition - worldPos);
		float attenuation = 1.0 / (distance * distance);
		vec3 radiance = lights[i].Color * attenuation;

		F0 = mix(F0, albedo, metallic);
		vec3 F = fresnelSchlick(max(dot(H, V), 0.0), F0, roughness);

		float NDF = distributionGGX(N, H, roughness);
		float G = geometrySmith(N, V, L, roughness);

		vec3 nominator = NDF * G * F;
		float denominator = 4.0 * max(dot(N, V), 0.0) * max(dot(N, L), 0.0) + 0.001;
		vec3 specular = nominator / denominator;

		vec3 kS = F;
		vec3 kD = vec3(1.0) - kS;

		kD *= 1.0 - metallic;

		float NdotL = max(dot(N, L), 0.0);
		Lo += (kD * albedo / PI + specular) * radiance * NdotL;
	}

	vec3 F = fresnelSchlick(max(dot(N, V), 0.0), F0, roughness);
	vec3 kS = F;
	vec3 kD = vec3(1.0) - kS;
	kD *= 1.0 - metallic;

	vec2 envBRDF = texture(brdfLut, vec2(max(dot(N, V), 0.0), roughness)).rg;
	vec3 specular = prefilteredColor * (F * envBRDF.x + envBRDF.y);

	vec3 diffuse = irradiance * albedo;
	vec3 ambient = (kD * diffuse + specular) * ao;

	vec3 color = ambient + Lo;

	color = color / (color + vec3(1.0));

	FragColor = vec4(color, 1.0);
}
