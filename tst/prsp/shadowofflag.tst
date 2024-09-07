gap> START_TEST("Forms: shadowofflag.tst");
gap> ps := PG(5,7);
ProjectiveSpace(5, 7)
gap> p := VectorSpaceToElement(ps,[1,0,0,0,0,0]*Z(7)^0);
<a point in ProjectiveSpace(5, 7)>
gap> l := VectorSpaceToElement(ps,[[1,0,0,0,0,0],[0,1,0,0,0,0]]*Z(7)^0);
<a line in ProjectiveSpace(5, 7)>
gap> v := VectorSpaceToElement(ps,[[1,0,0,0,0,0],[0,1,0,0,0,0],[0,0,1,0,0,0]]*Z(7)^0);
<a plane in ProjectiveSpace(5, 7)>
gap> flag := FlagOfIncidenceStructure(ps,[v,l,p]);
<a flag of ProjectiveSpace(5, 7)>
gap> s := ShadowOfFlag(ps,flag,4);
<shadow solids in ProjectiveSpace(5, 7)>
gap> s := ShadowOfFlag(ps,flag,"solids");
<shadow solids in ProjectiveSpace(5, 7)>
gap> STOP_TEST("shadowofflag.tst", 10000 );
