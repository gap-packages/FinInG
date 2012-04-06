#elations of a projective space
ps := PG(3,9);
sub := VectorSpaceToElement(ps,[[1,0,1,0],[0,1,0,1],[1,2,3,0]]*Z(3)^0);
p1 := VectorSpaceToElement(ps,[1,0,1,2]*Z(3)^0);
p2 := VectorSpaceToElement(ps,[1,2,0,2]*Z(3)^0);
phi := ElationOfProjectiveSpace(sub,p1,p2);
quit;
