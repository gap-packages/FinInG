gap> START_TEST("Forms: projectivity.tst");
gap> mat := [[1,0,0],[0,1,0],[0,0,1]]*Z(25)^0;
[ [ Z(5)^0, 0*Z(5), 0*Z(5) ], [ 0*Z(5), Z(5)^0, 0*Z(5) ], 
  [ 0*Z(5), 0*Z(5), Z(5)^0 ] ]
gap> Projectivity(mat,GF(25));
< a collineation: <cmat 3x3 over GF(5,2)>, F^0>
gap> Projectivity(mat,GF(625));
< a collineation: <cmat 3x3 over GF(5,4)>, F^0>
gap> Projectivity(PG(2,25),mat);
< a collineation: <cmat 3x3 over GF(5,2)>, F^0>
gap> Projectivity(PG(2,625),mat);
< a collineation: <cmat 3x3 over GF(5,4)>, F^0>
gap> STOP_TEST("projectivity.tst", 10000 );
