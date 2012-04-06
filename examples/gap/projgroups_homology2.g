#homology group with axis and centre
ps := PG(2,27);
sub := VectorSpaceToElement(ps,[[1,0,1,],[0,1,0]]*Z(3)^0);
p := VectorSpaceToElement(ps,[1,0,2]*Z(3)^0);
g := ProjectiveHomologyGroup(sub,p);
Order(g);
StructureDescription(g);
quit;
