varying vec2 vTextureCoord;
uniform sampler2D sampler;

void main() {
  gl_FragColor = texture2D(sampler, vec2(vTextureCoord.s, vTextureCoord.t));
}
