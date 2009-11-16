## affine_basic.g
ag := AffineSpace(3,3);
points := Points(ag);;
x := Random(points);
Display(x);
planes := AsList( Planes(ag) );;
p := Random(planes);
Display(p);
g := CollineationGroup( ag );
quit;
