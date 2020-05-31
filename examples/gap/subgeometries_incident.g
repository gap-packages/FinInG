#incidence in a subgeometry of a projective space
ps := ProjectiveSpace(5,9);
sub := CanonicalSubgeometryOfProjectiveSpace(ps,GF(3));
p := VectorSpaceToElement(sub,[1,1,1,1,0,0]*Z(9)^0);
l := VectorSpaceToElement(sub,[[1,1,1,1,0,0],[0,0,0,0,1,0]]*Z(9)^0);
plane := VectorSpaceToElement(sub,[[1,0,0,0,0,0],[0,1,0,0,0,0],[0,0,1,0,0,0]]*Z(9)^0);
p * l;
l * p;
IsIncident(p,l);
p in l;
l in p;
p * plane;
l * plane;
l in plane;
e := EmptySubspace(sub);
e * l;
quit;
e in l;
l in sub;
plane := VectorSpaceToElement(ps,[[1,0,0,0,0,0],[0,1,0,0,0,0],[0,0,1,0,0,0]]*Z(9)^0);
p * plane;
quit;
quit;

