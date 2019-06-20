#version 330 core
in vec2 outTexCoord;
out vec4 color;

uniform sampler2D tex;

void main() {
    color = texture(tex, outTexCoord);
}
