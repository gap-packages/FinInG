gap> START_TEST("Forms: duality.tst");
gap> pg := PG(2,2);
ProjectiveSpace(2, 2)
gap> delta := StandardDualityOfProjectiveSpace(pg);
StandardDuality( AllElementsOfIncidenceStructure( ProjectiveSpace(2,GF(2)) ) )
gap> phi := delta^2;
IdentityMapping( <All elements of ProjectiveSpace(2, 2)> )
gap> p := VectorSpaceToElement(pg,[1,1,1]*Z(2));
<a point in ProjectiveSpace(2, 2)>
gap> line := p^delta;
<a line in ProjectiveSpace(2, 2)>
gap> Unpack(UnderlyingObject(line));
[ [ Z(2)^0, 0*Z(2), Z(2)^0 ], [ 0*Z(2), Z(2)^0, Z(2)^0 ] ]
gap> DualCoordinatesOfHyperplane(line);
[ Z(2)^0, Z(2)^0, Z(2)^0 ]
gap> STOP_TEST("duality.tst", 10000 );
