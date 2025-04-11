gap> START_TEST("Forms: span.tst");
gap> pg := PG(3,5);
ProjectiveSpace(3, 5)
gap> p := VectorSpaceToElement(pg,[1,2,0,1]*Z(5)^0);
<a point in ProjectiveSpace(3, 5)>
gap> q := VectorSpaceToElement(pg,[0,3,3,2]*Z(5)^0);
<a point in ProjectiveSpace(3, 5)>
gap> line := Span(p,q);
<a line in ProjectiveSpace(3, 5)>
gap> Span(p,p) = p;
true
gap> r := VectorSpaceToElement(pg,[0,0,0,1]*Z(5)^0);
<a point in ProjectiveSpace(3, 5)>
gap> plane := Span(line,r);
<a plane in ProjectiveSpace(3, 5)>
gap> plane = Span([p,q,r]);
true
gap> t := Span(EmptySubspace(pg),p);
<a point in ProjectiveSpace(3, 5)>
gap> t = p;
true
gap> Span(pg,p);
ProjectiveSpace(3, 5)
gap> STOP_TEST("span.tst", 10000 );
