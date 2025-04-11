gap> START_TEST("Forms: random.tst");
gap> ps := PG(9,27);
ProjectiveSpace(9, 27)
gap> Random(Lines(ps));
<a line in ProjectiveSpace(9, 27)>
gap> Random(Points(ps));
<a point in ProjectiveSpace(9, 27)>
gap> Random(Solids(ps));
<a solid in ProjectiveSpace(9, 27)>
gap> Random(Hyperplanes(ps));
<a proj. 8-space in ProjectiveSpace(9, 27)>
gap> elts := ElementsOfIncidenceStructure(ps,6);
<proj. 5-subspaces of ProjectiveSpace(9, 27)>
gap> Random(elts);
<a proj. 5-space in ProjectiveSpace(9, 27)>
gap> RandomSubspace(ps,3);
<a solid in ProjectiveSpace(9, 27)>
gap> RandomSubspace(ps,7);
<a proj. 7-space in ProjectiveSpace(9, 27)>
gap> STOP_TEST("random.tst", 10000 );
