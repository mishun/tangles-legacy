//uniform mat4 shadow_matrix;
uniform vec4 light_to_camera_position;

//varying vec4 shadow_position;
varying vec3 fragment_to_camera_position;
varying vec3 fragment_normal;

void main()
{
	vec4 vertex_position = gl_ModelViewMatrix * gl_Vertex;
	//shadow_position = shadow_matrix * vertex_position;
	fragment_normal = normalize((gl_ModelViewMatrix * vec4(gl_Normal, 0.0)).xyz);
	fragment_to_camera_position = vertex_position.xyz / vertex_position.w;
	gl_Position = gl_ProjectionMatrix * vertex_position;
}
