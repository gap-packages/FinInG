#regular 13 system
q := 3;
gh := SplitCayleyHexagon(q);
ps := AmbientPolarSpace(gh);
coll := CollineationGroup(gh);
orbits := FiningOrbits(coll,Planes(ps));
Length(orbits);
S := First(orbits,x->Length(x)=(q^6-1)/(q-1));
pts := AsList(Points(ps));;
Collected(List(pts,x->Number(S,y->x in y)));
ps := ParabolicQuadric(6,q);
gh := SplitCayleyHexagon(ps);
coll := CollineationGroup(gh);
orbits := FiningOrbits(coll,Planes(ps));
Length(orbits);
S := First(orbits,x->Length(x)=(q^6-1)/(q-1));
pts := AsList(Points(ps));;
Collected(List(pts,x->Number(S,y->x in y)));
quit;
