gap> START_TEST("Forms: inc.tst");
gap> ps := ProjectiveSpace(5,81);
ProjectiveSpace(5, 81)
gap> p := VectorSpaceToElement(ps,[1,1,1,1,0,0]*Z(9)^0);
<a point in ProjectiveSpace(5, 81)>
gap> l := VectorSpaceToElement(ps,[[1,1,1,1,0,0],[0,0,0,0,1,0]]*Z(9)^0);
<a line in ProjectiveSpace(5, 81)>
gap> plane := VectorSpaceToElement(ps,[[1,0,0,0,0,0],[0,1,0,0,0,0],[0,0,1,0,0,0]]*Z(9)^0);
<a plane in ProjectiveSpace(5, 81)>
gap> p * l;
true
gap> l * p;
true
gap> IsIncident(p,l);
true
gap> p in l;
true
gap> l in p;
false
gap> p * plane;
false
gap> l * plane;
false
gap> l in plane;
false
gap> e := EmptySubspace(ps);
< empty subspace >
gap> e in l;
true
gap> l in ps;
true
gap> STOP_TEST("inc.tst", 10000 );
