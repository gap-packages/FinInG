#collineation fixing the subgeometry
pg := PG(2,7^3);
sub := CanonicalSubgeometryOfProjectiveSpace(pg,GF(7));
coll := CollineationFixingSubgeometry(sub);
Order(coll);
quit;

