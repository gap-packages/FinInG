#coordinates and related operations for subgeometries
pg := PG(3,3^5);
vectors := [ [ Z(3)^0, Z(3^5)^58, Z(3), Z(3^5)^239 ], 
  [ Z(3)^0, Z(3^5)^217, Z(3^5)^18, Z(3^5)^65 ], 
  [ Z(3)^0, Z(3^5)^202, Z(3^5)^52, Z(3^5)^209 ], 
  [ Z(3)^0, Z(3^5)^93, Z(3^5)^123, Z(3^5)^93 ], 
  [ Z(3)^0, Z(3^5)^199, Z(3^5)^68, Z(3^5)^13 ] ];
frame := List(vectors,x->VectorSpaceToElement(pg,x));
sub := SubgeometryOfProjectiveSpaceByFrame(pg,frame,GF(3));
p := Random(Points(sub));
Coordinates(p);
plane := Random(Planes(sub));
DualCoordinatesOfHyperplane(plane);
dual := [ Z(3)^0, Z(3^5)^78, Z(3^5)^58, Z(3^5)^8 ];
pi := HyperplaneByDualCoordinates(sub,dual);
EquationOfHyperplane(pi);
quit;
