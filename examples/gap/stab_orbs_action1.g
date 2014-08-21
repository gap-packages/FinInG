#action of a group on a subset (1)
pg := PG(2,3);
conic := Set(Points(ParabolicQuadric(2,3)));;
coll := CollineationGroup(pg);
orb := Orbit(coll,conic,OnSetsProjSubspaces);;
Length(orb);
hom := ActionHomomorphism(coll,orb,OnSetsProjSubspaces);
perm := Image(hom);
Order(perm);
NrMovedPoints(perm);
ps := SymplecticSpace(5,2);
coll := CollineationGroup(ps);
perm := Action(coll,Lines(ps),OnProjSubspaces);
NrMovedPoints(perm);
quit;
