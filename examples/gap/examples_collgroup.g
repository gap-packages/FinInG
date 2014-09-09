#collineation group: elementary
pg := PG(3,4);
coll := CollineationGroup(pg);
gens := GeneratorsOfGroup(coll);
UnderlyingMatrix(gens[2]);
Unpack(last);
as := AffineSpace(3,4);
coll := CollineationGroup(as);
GeneratorsOfGroup(coll);
gp := SplitCayleyHexagon(3);
coll:= CollineationGroup(gp);
GeneratorsOfGroup(coll);
egq := EGQByqClan(LinearqClan(3));
coll := CollineationGroup(egq);
quit;

