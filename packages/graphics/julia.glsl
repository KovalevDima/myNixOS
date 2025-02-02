#version 450
#extension GL_ARB_separate_shader_objects : enable

const int width = 512;
const int height = width;
const int workgroup_x = 32;
const int workgroup_y = 4;

// r^2 - r = |c|
const vec2 c = vec2(-0.8, 0.156);
const float r = 0.5 * (1 + sqrt (4 * dot(c,c) + 1));

layout (local_size_x = workgroup_x, local_size_y = workgroup_y, local_size_z = 1 ) in;
layout(std140, binding = 0) buffer buf
{
   vec4 imageData[];
};


// From https://iquilezles.org/www/articles/palettes/palettes.htm
//
// Traditional Julia blue and orange
vec3 color(const float t) {
  const vec3 a = vec3(0.5);
  const vec3 b = vec3(0.5);
  const vec3 c = vec3(8);
  const vec3 d = vec3(0.5, 0.6, 0.7);
  return a + b * cos(6.28318530718 * (c * t + d));
}

// complex multiplication
vec2 mulC(const vec2 a, const vec2 b) {
  return vec2(a.x * b.x - a.y * b.y, a.x * b.y + a.y * b.x);
}

vec2 f(const vec2 z) {
  return mulC(z,z) + c;
}

// Algorithm from https://en.wikipedia.org/wiki/Julia_set
void main() {
  vec2 z = vec2
    ( float(gl_GlobalInvocationID.y) / float(height) * 2 * r - r
    , float(gl_GlobalInvocationID.x) / float(width) * 2 * r - r
    );

  uint iteration = 0;
  const int max_iteration = 1000;

  while (dot(z,z) < dot(r,r) && iteration < max_iteration) {
    z = f(z);
    iteration++;
  }

  const uint i = width * gl_GlobalInvocationID.y + gl_GlobalInvocationID.x;
  if (iteration == max_iteration) {
    imageData[i] = vec4(0,0,0,1);
  } else {
    imageData[i] = vec4(color(float(iteration) / float(max_iteration)),1);
  }
}
