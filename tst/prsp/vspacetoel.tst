gap> START_TEST("Forms: vspacetoel.tst");
gap> ps := ProjectiveSpace(6,7);
ProjectiveSpace(6, 7)
gap> v := [3,5,6,0,3,2,3]*Z(7)^0;
[ Z(7), Z(7)^5, Z(7)^3, 0*Z(7), Z(7), Z(7)^2, Z(7) ]
gap> p := VectorSpaceToElement(ps,v);
<a point in ProjectiveSpace(6, 7)>
gap> Display(p);
[142.131]
gap> ps := ProjectiveSpace(3,4);
ProjectiveSpace(3, 4)
gap> v := [1,1,0,1]*Z(4)^0;
[ Z(2)^0, Z(2)^0, 0*Z(2), Z(2)^0 ]
gap> p := VectorSpaceToElement(ps,v);
<a point in ProjectiveSpace(3, 4)>
gap> mat := [[1,0,0,1],[0,1,1,0]]*Z(4)^0;
[ [ Z(2)^0, 0*Z(2), 0*Z(2), Z(2)^0 ], [ 0*Z(2), Z(2)^0, Z(2)^0, 0*Z(2) ] ]
gap> line := VectorSpaceToElement(ps,mat);
<a line in ProjectiveSpace(3, 4)>
gap> STOP_TEST("vspacetoel.tst", 10000 );
