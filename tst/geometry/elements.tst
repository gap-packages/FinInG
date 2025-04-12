gap> START_TEST("Forms: elements.tst");
gap> ps := ParabolicQuadric(2,59);
Q(2, 59)
gap> pg := PG(2,59);
ProjectiveSpace(2, 59)
gap> p := VectorSpaceToElement(ps,[0,1,0]*Z(59)^0);
<a point in Q(2, 59)>
gap> ElementToElement(pg,p);
<a point in ProjectiveSpace(2, 59)>
gap> obj := UnderlyingObject(p);
<immutable cvec over GF(59,1) of length 3>
gap> ObjectToElement(pg,1,obj);
<a point in ProjectiveSpace(2, 59)>
gap> STOP_TEST("elements.tst", 10000 );
