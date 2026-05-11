### Fonts

fonts can be MSDF, SDF, or SoftMask fonts, generated with 
[msdf-atlas-gen](https://github.com/Chlumsky/msdf-atlas-gen/tree/master).

Fonts are generated like so:

`msdf-atlas-gen -font <font.ttf> -type <fonttype> -size 24 -aemrange -0.05 +0.05 -yorigin top -imageout font.png -json font.json`

`<fonttype>` may be softmask, sdf, mtsdf or msdf. The font loader will
be able to handle these 3 correctly.
MTSDF will be treated as MSDF*
** THE IMPLEMENTATION WILL THROW IF ITS NOT ONE OF THESE **

`size` may be changed (it will be handled correctly)

`aemrange` may _also_ be changed, but the current shaders don't really
require large ranges anyway


TODO: custom font shaders later?