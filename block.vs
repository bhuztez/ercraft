attribute vec3 position;
attribute vec2 aTextureCoord;

uniform mat4 rotateMatrix;
uniform mat4 translateMatrix;
uniform mat4 perspectiveMatrix;

varying vec2 vTextureCoord;

void main() {
  gl_Position = perspectiveMatrix * translateMatrix * rotateMatrix * vec4(position, 1.0);
  vTextureCoord = aTextureCoord;
}
