gap> START_TEST("Forms: shortnames.tst");
gap> ps := PG(6,13);
ProjectiveSpace(6, 13)
gap> plane := VectorSpaceToElement(ps,[[1,2,3,4,5,6,7],[8,9,10,11,12,0,1],
> [2,3,4,0,6,7,8]]*Z(13)^0);
<a plane in ProjectiveSpace(6, 13)>
gap> Points(plane);
<shadow points in ProjectiveSpace(6, 13)>
gap> Lines(plane);
<shadow lines in ProjectiveSpace(6, 13)>
gap> Solids(plane);
<shadow solids in ProjectiveSpace(6, 13)>
gap> Hyperplanes(plane);
<shadow lines in ProjectiveSpace(6, 13)>
gap> ElementsIncidentWithElementOfIncidenceStructure(plane,6);
<shadow proj. 5-subspaces in ProjectiveSpace(6, 13)>
gap> STOP_TEST("shortnames.tst", 10000 );
