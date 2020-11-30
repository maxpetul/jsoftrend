# Simple Software Renderer in J
## Purpose
This is a small project I created because I was curious to experiment with an APL-like language. It's effectively an OBJ file viewer.
## Usage
    viewobjfile '/path/to/mesh.obj'; '/path/to/texture.png'
It should work with most OBJ files but the file must only contain one mesh using one material. `viewobjfile` only accepts PNG files as textures but `viewobj` accepts any 2D array of color values.

To set the resolution:

    setres_ssr_ width height
By default, the model coords are assumed to be right-handed with +Z up and +Y forward. The up and forward vectors can be changed by setting `up_ssr_` and `fwd_ssr_` (must be orthogonal and normalized).
## Result
![Example Cube Render](result.png)
