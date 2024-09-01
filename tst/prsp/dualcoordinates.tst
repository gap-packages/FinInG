gap> START_TEST("Forms: dualcoordinates.tst");
gap> ps := PG(3,19);
ProjectiveSpace(3, 19)
gap> hyp := VectorSpaceToElement(ps,[[1,3,5,7],[0,2,4,0],[8,12,17,0]]*Z(19)^0);
<a plane in ProjectiveSpace(3, 19)>
gap> DualCoordinatesOfHyperplane(hyp);
[ Z(19)^0, Z(19)^7, Z(19)^15, Z(19)^6 ]
gap> STOP_TEST("dualcoordinates.tst", 10000 );
