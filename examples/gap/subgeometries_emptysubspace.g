#the empty subspace
sub := CanonicalSubgeometryOfProjectiveSpace(PG(4,49),GF(7));
e := EmptySubspace(sub);
p := VectorSpaceToElement(sub,[1,0,0,1,0]*Z(7)^0);
e*p;
quit;
e in p;
quit;
