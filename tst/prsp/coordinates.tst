gap> START_TEST("Forms: coordinates.tst");
gap> ps := PG(1,5);
ProjectiveSpace(1, 5)
gap> p := VectorSpaceToElement(ps,[4,3]*Z(5)^0);
<a point in ProjectiveSpace(1, 5)>
gap> c := Coordinates(p);
[ Z(5)^0, Z(5) ]
gap> STOP_TEST("coordinates.tst", 10000 );
