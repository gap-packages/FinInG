gap> START_TEST("Forms: ambientspaceel.tst");
gap> ps := PG(5,29^2);
ProjectiveSpace(5, 841)
gap> p := VectorSpaceToElement(ps,[23,2,17,0,5,8]*Z(29^3)^0);
<a point in ProjectiveSpace(5, 841)>
gap> AmbientSpace(p);
ProjectiveSpace(5, 841)
gap> e := EmptySubspace(ps);
< empty subspace >
gap> AmbientSpace(e);
ProjectiveSpace(5, 841)
gap> hyp := HyperplaneByDualCoordinates(ps,[23,2,17,0,5,8]*Z(29^3)^0);
<a proj. 4-space in ProjectiveSpace(5, 841)>
gap> AmbientSpace(hyp);
ProjectiveSpace(5, 841)
gap> STOP_TEST("ambientspaceel.tst", 10000 );
