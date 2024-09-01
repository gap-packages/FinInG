gap> START_TEST("Forms: elshort.tst");
gap> ps := PG(6,13);
ProjectiveSpace(6, 13)
gap> Points(ps);
<points of ProjectiveSpace(6, 13)>
gap> Lines(ps);
<lines of ProjectiveSpace(6, 13)>
gap> Planes(ps);
<planes of ProjectiveSpace(6, 13)>
gap> Solids(ps);
<solids of ProjectiveSpace(6, 13)>
gap> Hyperplanes(ps);
<proj. 5-subspaces of ProjectiveSpace(6, 13)>
gap> ps := PG(2,2);
ProjectiveSpace(2, 2)
gap> Hyperplanes(ps);
<lines of ProjectiveSpace(2, 2)>
gap> STOP_TEST("elshort.tst", 10000 );
