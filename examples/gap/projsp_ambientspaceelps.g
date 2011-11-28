#base field of a projective space
ps := PG(3,27);
p := VectorSpaceToElement(ps,[1,2,1,0]*Z(3)^3);
AmbientSpace(p);
quit;
