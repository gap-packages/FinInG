#Random for subspaces of a vector space
pg := PG(4,53);
vs := UnderlyingVectorSpace(pg);
subs := Subspaces(vs,2);
Random(subs);
quit;

