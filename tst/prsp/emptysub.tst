gap> START_TEST("Forms: emptysub.tst");
gap> ps := PG(3,9);
ProjectiveSpace(3, 9)
gap> e := EmptySubspace(ps);
< empty subspace >
gap> p := VectorSpaceToElement(ps,[1,1,0,0]*Z(9)^0);
<a point in ProjectiveSpace(3, 9)>
gap> e in p;
true
gap> STOP_TEST("emptysub.tst", 10000 );
