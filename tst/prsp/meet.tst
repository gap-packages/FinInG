gap> START_TEST("Forms: meet.tst");
gap> ps := PG(8,73^2);
ProjectiveSpace(8, 5329)
gap> hyp := HyperplaneByDualCoordinates(ps,[1,0,0,0,0,0,0,0,0]*Z(73)^0);
<a proj. 7-space in ProjectiveSpace(8, 5329)>
gap> line := VectorSpaceToElement(ps,[[1,1,0,0,0,0,0,0,0],[1,0,1,0,0,0,0,0,0]]*Z(73)^0);
<a line in ProjectiveSpace(8, 5329)>
gap> p := Meet(hyp,line);
<a point in ProjectiveSpace(8, 5329)>
gap> Unpack(UnderlyingObject(p));
[ 0*Z(73), Z(73)^0, Z(73)^36, 0*Z(73), 0*Z(73), 0*Z(73), 0*Z(73), 0*Z(73), 
  0*Z(73) ]
gap> hyp2 := HyperplaneByDualCoordinates(ps,[0,1,0,0,0,0,0,0,0]*Z(73)^0);
<a proj. 7-space in ProjectiveSpace(8, 5329)>
gap> sec := Meet(hyp,hyp2);
<a proj. 6-space in ProjectiveSpace(8, 5329)>
gap> q := Meet(hyp2,line);
<a point in ProjectiveSpace(8, 5329)>
gap> Meet(p,q);
< empty subspace >
gap> Meet([hyp,hyp2,line]);
< empty subspace >
gap> STOP_TEST("meet.tst", 10000 );
