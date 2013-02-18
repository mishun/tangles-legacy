//uniform sampler2DShadow sampler;
//uniform mat4 shadow_matrix;
uniform vec4 light_to_camera_position;

//varying vec4 shadow_position;
varying vec3 fragment_to_camera_position;
varying vec3 fragment_normal;

const vec4 fog_color = vec4(0.5, 0.5, 0.5, 1.0);
const float fog_begin = 2.0;
const float fog_end = 30.0;

void main()
{
	vec3 normal = normalize(fragment_normal);
	vec3 to_light = light_to_camera_position.xyz / light_to_camera_position.w - fragment_to_camera_position;
	float dist_to_light = length(to_light);

	float attenuation = 1.0 / (gl_LightSource[0].constantAttenuation + gl_LightSource[0].linearAttenuation * dist_to_light + gl_LightSource[0].quadraticAttenuation * dist_to_light * dist_to_light);

	vec3 dir_to_light = normalize(to_light);

	float nxDir;
	if(dot(fragment_to_camera_position, normal) <= 0.0)
		nxDir = max(0.0, dot(normal, dir_to_light));
	else
		nxDir = max(0.0, dot(-normal, dir_to_light));

	vec4 diffuse_light = gl_LightSource[0].diffuse * nxDir * attenuation;

	float specular_power = 0.0;
	if(nxDir > 0.0)
	{
		vec3 dir_to_camera = normalize(-fragment_to_camera_position);
		vec3 half = normalize(dir_to_light + dir_to_camera);
		float nxHalf = max(0.0, dot(normal, half));
		specular_power = pow(nxHalf, gl_FrontMaterial.shininess);
	}
	vec4 specular_light = gl_LightSource[0].specular * specular_power * attenuation;

	vec3 ambient_color = gl_FrontMaterial.ambient.rgb * gl_LightSource[0].ambient.rgb;
	vec3 diffuse_color = gl_FrontMaterial.diffuse.rgb * diffuse_light.rgb;
	vec3 specular_color = gl_FrontMaterial.specular.rgb * specular_light.rgb;
	//float shadow = shadow2DProj(sampler, shadow_position).x;
	float shadow = 1.0;

	vec4 color = vec4(ambient_color + (diffuse_color + specular_color) * shadow, 1.0);

	float fog = min(1.0, max(0.0, (-fragment_to_camera_position.z - fog_begin) / (fog_end - fog_begin)));

	gl_FragColor = fog_color * fog + color * (1.0 - fog);
}
