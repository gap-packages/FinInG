gap> START_TEST("Forms: randomsubspace.tst");
gap> ps := PG(7,131);
ProjectiveSpace(7, 131)
gap> RandomSubspace(ps,1);
<a line in ProjectiveSpace(7, 131)>
gap> RandomSubspace(ps,2);
<a plane in ProjectiveSpace(7, 131)>
gap> RandomSubspace(ps,3);
<a solid in ProjectiveSpace(7, 131)>
gap> RandomSubspace(ps,4);
<a proj. 4-space in ProjectiveSpace(7, 131)>
gap> RandomSubspace(ps,5);
<a proj. 5-space in ProjectiveSpace(7, 131)>
gap> RandomSubspace(ps,6);
<a proj. 6-space in ProjectiveSpace(7, 131)>
gap> STOP_TEST("randomsubspace.tst", 10000 );
