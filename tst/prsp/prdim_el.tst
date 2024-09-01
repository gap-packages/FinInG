gap> START_TEST("Forms: prdim_el.tst");
gap> ps := PG(6,5);
ProjectiveSpace(6, 5)
gap> v := [[1,1,0,0,0,0,0],[0,0,0,3,2,0,0]]*Z(5)^0;
[ [ Z(5)^0, Z(5)^0, 0*Z(5), 0*Z(5), 0*Z(5), 0*Z(5), 0*Z(5) ], 
  [ 0*Z(5), 0*Z(5), 0*Z(5), Z(5)^3, Z(5), 0*Z(5), 0*Z(5) ] ]
gap> line := VectorSpaceToElement(ps,v);
<a line in ProjectiveSpace(6, 5)>
gap> ProjectiveDimension(line);
1
gap> Dimension(line);
1
gap> p := VectorSpaceToElement(ps,[1,2,3,0,0,0,0]*Z(5)^0);
<a point in ProjectiveSpace(6, 5)>
gap> ProjectiveDimension(p);
0
gap> Dimension(p);
0
gap> ProjectiveDimension(EmptySubspace(ps));
-1
gap> STOP_TEST("prdim_el.tst", 10000 );
