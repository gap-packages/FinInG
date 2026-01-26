#ElationOfProjectiveSpace ProjectiveElationGroup
ps := PG(3,9);
sub := VectorSpaceToElement(ps,[[1,0,1,0],[0,1,0,1],[1,2,3,0]]*Z(3)^0);
p1 := VectorSpaceToElement(ps,[1,0,1,2]*Z(3)^0);
p2 := VectorSpaceToElement(ps,[1,2,0,2]*Z(3)^0);
phi := ElationOfProjectiveSpace(sub,p1,p2);
ps := PG(2,27);
sub := VectorSpaceToElement(ps,[[1,0,1,],[0,1,0]]*Z(3)^0);
p := VectorSpaceToElement(ps,[1,1,1]*Z(3)^0);
g := ProjectiveElationGroup(sub,p);
Order(g);
StructureDescription(g);
ps := PG(3,4);
sub := Random(Hyperplanes(ps));
g := ProjectiveElationGroup(sub);
Order(g);
Transitivity(g,Difference(Points(ps),Points(sub)),OnProjSubspaces);
StructureDescription(g);
quit;
