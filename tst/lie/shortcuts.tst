gap> START_TEST("Forms: shortcuts.tst");
gap> pg := PG(6,47);
ProjectiveSpace(6, 47)
gap> p := VectorSpaceToElement(pg,[1,0,0,0,0,0,0]*Z(47)^0);
<a point in ProjectiveSpace(6, 47)>
gap> r := VectorSpaceToElement(pg,[0,1,0,0,0,0,0]*Z(47)^0);
<a point in ProjectiveSpace(6, 47)>
gap> l := Span(p,r);
<a line in ProjectiveSpace(6, 47)>
gap> Points(l);
<shadow points in ProjectiveSpace(6, 47)>
gap> Points(pg,l);
<shadow points in ProjectiveSpace(6, 47)>
gap> Lines(l);
<shadow lines in ProjectiveSpace(6, 47)>
gap> Lines(pg,l);
<shadow lines in ProjectiveSpace(6, 47)>
gap> s := VectorSpaceToElement(pg,[0,0,1,0,0,0,0]*Z(47)^0);
<a point in ProjectiveSpace(6, 47)>
gap> plane := Span([p,r,s]);
<a plane in ProjectiveSpace(6, 47)>
gap> Planes(plane);
<shadow planes in ProjectiveSpace(6, 47)>
gap> Planes(pg,plane);
<shadow planes in ProjectiveSpace(6, 47)>
gap> Hyperplanes(l);
<shadow points in ProjectiveSpace(6, 47)>
gap> Hyperplanes(pg,l);
<shadow proj. 5-subspaces in ProjectiveSpace(6, 47)>
gap> Solids(pg,l);
<shadow solids in ProjectiveSpace(6, 47)>
gap> Solids(pg,plane);
<shadow solids in ProjectiveSpace(6, 47)>
gap> l in p;
false
gap> p in Points(l);
true
gap> STOP_TEST("shortcuts.tst", 10000 );
