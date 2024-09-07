gap> START_TEST("Forms: flagofinc.tst");
gap> ps := PG(3,13);
ProjectiveSpace(3, 13)
gap> plane := HyperplaneByDualCoordinates(ps,[1,0,0,0]*Z(13)^0);
<a plane in ProjectiveSpace(3, 13)>
gap> line := VectorSpaceToElement(ps,[[0,1,0,0],[0,0,1,0]]*Z(13)^0);
<a line in ProjectiveSpace(3, 13)>
gap> pt := VectorSpaceToElement(ps,[0,1,0,0]*Z(13)^0);
<a point in ProjectiveSpace(3, 13)>
gap> flag := FlagOfIncidenceStructure(ps,[pt,line,plane]);
<a flag of ProjectiveSpace(3, 13)>
gap> IsEmptyFlag(flag);
false
gap> IsChamberOfIncidenceStructure(flag);
true
gap> STOP_TEST("flagofinc.tst", 10000 );
