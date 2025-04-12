gap> START_TEST("Forms: empty.tst");
gap> pg := PG(5,7);
ProjectiveSpace(5, 7)
gap> p := VectorSpaceToElement(pg,[1,0,0,0,0,0]*Z(7)^0);
<a point in ProjectiveSpace(5, 7)>
gap> e := EmptySubspace(pg);
< empty subspace >
gap> e in e;
true
gap> p in e;
false
gap> e in p;
true
gap> e in pg;
true
gap> pg in e;
false
gap> Span(p,e);
<a point in ProjectiveSpace(5, 7)>
gap> Span(e,e);
< empty subspace >
gap> Meet(e,e);
< empty subspace >
gap> Meet(e,p);
< empty subspace >
gap> Meet(p,e);
< empty subspace >
gap> STOP_TEST("empty.tst", 10000 );
