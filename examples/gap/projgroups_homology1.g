#homologies of a projective space
ps := PG(3,81);
sub := VectorSpaceToElement(ps,[[1,0,1,0],[0,1,0,1],[1,2,3,0]]*Z(3)^0);
centre := VectorSpaceToElement(ps,[0*Z(3),Z(3)^0,Z(3^4)^36,0*Z(3)]);
p1 := VectorSpaceToElement(ps,[0*Z(3),Z(3)^0,Z(3^4)^51,0*Z(3)]);
p2 := VectorSpaceToElement(ps,[0*Z(3),Z(3)^0,Z(3^4)^44,0*Z(3)]);
phi := HomologyOfProjectiveSpace(sub,centre,p1,p2);
quit;
