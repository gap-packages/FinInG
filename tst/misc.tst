#
gap> START_TEST("misc.tst");

# test NiceMonomorphism for subgeometries, which uses hash tables
gap> pg := PG(1,5^4);
ProjectiveSpace(1, 625)
gap> sub := CanonicalSubgeometryOfProjectiveSpace(pg, GF(5));
Subgeometry PG(1, 5) of ProjectiveSpace(1, 625)
gap> g := CollineationGroup(sub);
The FinInG collineation group PGL(2,5) of Subgeometry PG(1, 5) of ProjectiveSpace(1, 625)
gap> Size(g);
480
gap> Number([1..10], i -> PseudoRandom(g) in g);
10

#
gap> STOP_TEST("misc.tst", 10000 );
