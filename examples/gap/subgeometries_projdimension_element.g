#projective dimension of an element of a subgeometry
sub := CanonicalSubgeometryOfProjectiveSpace(PG(2,5^3),GF(5));
v := [[1,1,0],[0,3,2]]*Z(5)^0;
line := VectorSpaceToElement(sub,v);
ProjectiveDimension(line);
Dimension(line);
p := VectorSpaceToElement(sub,[1,2,3]*Z(5)^0);
ProjectiveDimension(p);
Dimension(p);
ProjectiveDimension(EmptySubspace(sub));
quit;
