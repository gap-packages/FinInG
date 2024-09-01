gap> START_TEST("Forms: basefieldel.tst");
gap> ps := PG(11,37^3);
ProjectiveSpace(11, 50653)
gap> p := VectorSpaceToElement(ps,[10,32,1,10,5,14,28,-1,19,2,65,8]*Z(37^3)^0);
<a point in ProjectiveSpace(11, 50653)>
gap> BaseField(p);
GF(37^3)
gap> e := EmptySubspace(ps);
< empty subspace >
gap> BaseField(e);
GF(37^3)
gap> hyp := HyperplaneByDualCoordinates(ps,[10,32,1,10,5,14,28,-1,19,2,65,8]*Z(37^3)^0);
<a proj. 10-space in ProjectiveSpace(11, 50653)>
gap> BaseField(hyp);
GF(37^3)
gap> STOP_TEST("basefieldel.tst", 10000 );
