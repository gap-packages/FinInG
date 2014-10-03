q := 5;
pg := PG(2,q);
coll := CollineationGroup(pg);
conic := ParabolicQuadric(2,q);
pts := Set(Points(conic));
orb := Orb(coll,pts,OnSetsProjSubspaces);