# example: affine_shadow1.g
as := AffineSpace(3, 3);
l := Random( Lines( as ) );
planesonl := Planes(l);
AsList(planesonl);
quit;
