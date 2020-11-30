load 'viewmat'
load 'graphics/png'

NB. Default up vector is +Z tilted forward a bit, fwd is +Y tilted down a bit. The slight tilt makes the render look more natural
NB. compared to a straight side-on view.
up_ssr_  =: 0 0.447  0.894
fwd_ssr_ =: 0 0.894 _0.447

NB. Distance from camera to object relative to its size. Setting this too low can cause division by zero errors.
dist_ssr_ =: 1

NB. fb_ssr_ is the framebuffer, zb_ssr_ is the depth (AKA "Z") buffer
clear_ssr_ =: monad : 'fb =: (dispw, disph) $ 0 [ zb =: (dispw, disph) $ __'

setres_ssr_ =: monad define
  'xx yy' =. y
  dispw =: xx  [ disph =: yy     NB. Display width & height
  pixw  =: %xx [ pixh  =: %yy    NB. Pixel width & height in units
  clear ''
  xx, yy
)

NB. Initialize variables. Default resolution is only 300x300 b/c rendering is pretty slow.
setres_ssr_ 300 300

NB. Round to nearest integer
rnd =: <.@+&0.5

NB. Basic vector functions
len =: monad : '%:+/*:y' "1
dot =: dyad : '+/x*y' "1 1
cross =: dyad : '((1|.x) * (_1|.y)) - ((_1|.x) * (1|.y))' "1 1
crsxy =: dyad : '-/x*|.y' "1 1   NB. Cross 2D vectors, like crossing vectors in the XY plane and returning the Z coord of the result.

NB. Compute barycentric coordinates for a point (y) relative to a triangle (x)
NB. The point is assumed to lie in the triangle's plane
bary =: dyad : '1|. ((%dot~) (2{egs) cross 0{egs) dot egs cross (y -"1 x) [ egs=. (-~1&|.) x' "2 1

NB. Compute barycentric coodinates for a point (y) relative to a triangle (x) in 2D coordinates
bary2d =: dyad : '1|. ((%*~) (2{egs) crsxy 0{egs) * egs crsxy (y -"1 x) [ egs=. (-~1&|.) x' "2 1

rasterize =: monad define
  y2d =. 2&{."1 y
  bb =. ((disph_ssr_-1), dispw_ssr_-1)<. (0 0)>. <. (disph_ssr_, dispw_ssr_)* (>."1/,:<."1/) y2d   NB. Compute and clip bounding box
  indices =. ,/ ((0{1{bb)+i.bbh) ,"0/ (1{1{bb)+i.bbw [ 'bbh bbw'=.>:-/bb   NB. Compute indices for each pixel in BB
  barys =. y2d bary2d (pixw_ssr_, pixh_ssr_) *"1 (0.5 + indices)   NB. Convert indices to screen coords and compute barycentric coords
  mask_intri =. I. -.0><./"1 barys   NB. Filter out pixels outside the triangle
  indices =. mask_intri { indices [ barys =. mask_intri { barys
  indices ; barys
)

NB. Sample active texture (global var at_ssr_) at point (y)
sample_ssr_ =: monad : 'at {::~ (<:$at)<. 0 0>. <. ($at)* 1|. y' "2 1

drawsingletri =: dyad define
  'ind barys' =. rasterize y

  NB. Depth test
  depths =. +/"1 barys *"1 1 (2&{"1 y)
  mask_vis =. I. depths > ind {:: zb_ssr_
  ind =. mask_vis { ind [ barys =. mask_vis { barys
  zb_ssr_ =: (mask_vis{depths) ind}zb_ssr_

  NB. Perspective correct interpolation of UV coords
  uvdz =. (,&1"1 x) %"1 0 (2&{"1 y)  NB. compute u/z v/z 1/z
  iuv =. +/"2 (uvdz&(*"1 0))"1 barys  NB. interpolate over triangle
  uvs =. (2&{. % 2&{)"1 iuv  NB. Compute u/z v/z

  fb_ssr_ =: (sample_ssr_ uvs) ind} fb_ssr_
  ''
)

readobjline_ssr_ =. monad define
  begins =. dyad : '*./x=($x){.y'"1 1
  triangulate =. monad : 'if. 4=0{$y do. (0 1 2{y),: 2 3 0{y else. y end.' "_
  dropnor =. monad : 'if. 9=$y do. 0 1 3 4 6 7 {y else. y end.' "1
  if.     'v '  begins y do. v  =: v , _&".;.1 [  }. y
  elseif. 'vt ' begins y do. vt =: vt, 2{. _&".;.1 [ 2}. y
  elseif. 'f '  begins y do. f  =: f , dropnor _". ,/"3 (];._1)"1 triangulate ('/'&,);.1 [ }. y
  end.
  ''
)

readobj =: monad define
  NB. Initialize lists to a row of the expected size so appending works properly, e.g. gives 1 2; 3 4; 5 6 instead of 1 2 3 4 5 6
  NB. This is especially important for faces where we might append one or two rows at a time (for quads)
  v_ssr_ =: 1 3 $0 [ vt_ssr_ =: 1 2 $0 [ f_ssr_ =: 1 6 $0
  readobjline_ssr_;._2 LF,~ freads y
  (}.v_ssr_) ; (}.vt_ssr_) ; }.f_ssr_
)

NB. Extract triangles from an obj, containing either vertex positions or UV coordinates. The UV coords are inverted vertically.
objtripos =: monad : '{&v"1 (0 2 4){"1 <:f [ ''v vt f'' =. y'
objtriuv  =: monad : '0 1 (+"1 1) 1 _1 (*"1 1) {&vt"1 (1 3 5){"1 <:f [ ''v vt f'' =. y'

transformobj =: monad define
  NB. Extract vertex positions from the OBJ and transform them into our coordinate space (-X up, +Y right, -Z fwd)
  remapaxes =. (_1*up_ssr_) , (fwd_ssr_ cross up_ssr_) ,: _1*fwd_ssr_
  pos =. remapaxes (+/ . *)"2 1 >0{y

  bb =. (>."1/,:<."1/) pos   NB. bounding box
  persp =. monad : '(p,p,1)*y [ p=.%(dist_ssr_+0.5)-2{y'"1   NB. Perspective transformation (divide by z)

  NB. Recenter positions around BB center, rescale pos so the longest BB edge is length 1, perspective transform,
  NB. then recenter pos around the display center
  pos =. (0.5, 0.5, -(dist_ssr_+0.5)) +"1 1 persp (% >./ -"1/bb) * pos (-"1 1) -:+"1/bb

  (<pos) 0}y
)

NB. Renders a textured model and opens a window to display it. Input (y) is obj;texture where 'texture' is a 2D array of color values e.g. the
NB. return value of readpng and 'obj' is the contents of a Wavefront OBJ file returned from readobj.
viewobj =: monad define
  'obj tex' =. y
  obj =. transformobj obj
  clear_ssr_ ''
  at_ssr_ =: tex

  uvs =. objtriuv obj [ pos =. objtripos obj

  NB. Backface culling
  mask_front =. I. 0< (2&{ @ (2&{ cross 0&{) @ (-~1&|.))"2 pos
  pos =. mask_front { pos [ uvs =. mask_front { uvs

  uvs drawsingletri"2 2 pos
  viewrgb fb_ssr_
)

NB. Like viewobj but takes filenames. The texture must be a PNG file.
viewobjfile =: monad : 'viewobj (readobj jpath >0{y); (readpng jpath >1{y)'
