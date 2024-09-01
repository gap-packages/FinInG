gap> START_TEST("Forms: prdim.tst");
gap> ps := PG(6,7);
ProjectiveSpace(6, 7)
gap> Dimension(ps);
6
gap> Rank(ps);
6
gap> ps := PG(1,4);
ProjectiveSpace(1, 4)
gap> Dimension(ps);
1
gap> Rank(ps);
1
gap> STOP_TEST("prdim.tst", 10000 );
