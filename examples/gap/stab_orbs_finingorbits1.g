#orbits of some groups on elements of the projective space
ps := HermitianPolarSpace(3,9);
g := CollineationGroup(ps);
FiningOrbits(g,Lines(PG(3,9)));
FiningOrbits(g,Planes(PG(3,9)));
ps := ParabolicQuadric(2,5);
g := CollineationGroup(ps);
pts := Filtered(Points(PG(2,5)),x->not x in ps);;
Length(pts);
FiningOrbits(g,Points(PG(2,5)));
FiningOrbits(g,pts,OnProjSubspaces);
quit;
