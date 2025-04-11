gap> START_TEST("Forms: polaritiesnice.tst");
gap> mat := IdentityMat(4,GF(3));
[ [ Z(3)^0, 0*Z(3), 0*Z(3), 0*Z(3) ], [ 0*Z(3), Z(3)^0, 0*Z(3), 0*Z(3) ], 
  [ 0*Z(3), 0*Z(3), Z(3)^0, 0*Z(3) ], [ 0*Z(3), 0*Z(3), 0*Z(3), Z(3)^0 ] ]
gap> phi := PolarityOfProjectiveSpace(mat,GF(3));
<polarity of PG(3, GF(3)) >
gap> points := AbsolutePoints(phi);
<points of Q+(3, 3): x_1^2+x_2^2+x_3^2+x_4^2=0>
gap> List(points);
[ <a point in Q+(3, 3): x_1^2+x_2^2+x_3^2+x_4^2=0>, 
  <a point in Q+(3, 3): x_1^2+x_2^2+x_3^2+x_4^2=0>, 
  <a point in Q+(3, 3): x_1^2+x_2^2+x_3^2+x_4^2=0>, 
  <a point in Q+(3, 3): x_1^2+x_2^2+x_3^2+x_4^2=0>, 
  <a point in Q+(3, 3): x_1^2+x_2^2+x_3^2+x_4^2=0>, 
  <a point in Q+(3, 3): x_1^2+x_2^2+x_3^2+x_4^2=0>, 
  <a point in Q+(3, 3): x_1^2+x_2^2+x_3^2+x_4^2=0>, 
  <a point in Q+(3, 3): x_1^2+x_2^2+x_3^2+x_4^2=0>, 
  <a point in Q+(3, 3): x_1^2+x_2^2+x_3^2+x_4^2=0>, 
  <a point in Q+(3, 3): x_1^2+x_2^2+x_3^2+x_4^2=0>, 
  <a point in Q+(3, 3): x_1^2+x_2^2+x_3^2+x_4^2=0>, 
  <a point in Q+(3, 3): x_1^2+x_2^2+x_3^2+x_4^2=0>, 
  <a point in Q+(3, 3): x_1^2+x_2^2+x_3^2+x_4^2=0>, 
  <a point in Q+(3, 3): x_1^2+x_2^2+x_3^2+x_4^2=0>, 
  <a point in Q+(3, 3): x_1^2+x_2^2+x_3^2+x_4^2=0>, 
  <a point in Q+(3, 3): x_1^2+x_2^2+x_3^2+x_4^2=0> ]
gap> mat := [[0,1,0,0],[1,0,0,0],[0,0,0,1],[0,0,1,0]]*Z(5)^0;
[ [ 0*Z(5), Z(5)^0, 0*Z(5), 0*Z(5) ], [ Z(5)^0, 0*Z(5), 0*Z(5), 0*Z(5) ], 
  [ 0*Z(5), 0*Z(5), 0*Z(5), Z(5)^0 ], [ 0*Z(5), 0*Z(5), Z(5)^0, 0*Z(5) ] ]
gap> phi := HermitianPolarityOfProjectiveSpace(mat,GF(25));
<polarity of PG(3, GF(5^2)) >
gap> mat2 := IdentityMat(4,GF(5));
[ [ Z(5)^0, 0*Z(5), 0*Z(5), 0*Z(5) ], [ 0*Z(5), Z(5)^0, 0*Z(5), 0*Z(5) ], 
  [ 0*Z(5), 0*Z(5), Z(5)^0, 0*Z(5) ], [ 0*Z(5), 0*Z(5), 0*Z(5), Z(5)^0 ] ]
gap> psi := PolarityOfProjectiveSpace(mat2,GF(25));
<polarity of PG(3, GF(5^2)) >
gap> phi*psi = psi*phi;
true
gap> g := CorrelationCollineationGroup(PG(3,25));
The FinInG correlation-collineation group PGammaL(4,25) : 2
gap> h := CollineationGroup(PG(3,25));
The FinInG collineation group PGammaL(4,25)
gap> hom := Embedding(h,g);
MappingByFunction( The FinInG collineation group PGammaL(4,25), The FinInG cor\
relation-collineation group PGammaL(4,25) : 2, function( y ) ... end )
gap> coll := PreImagesRepresentative(hom,phi*psi);
< a collineation: <cmat 4x4 over GF(5,2)>, F^5>
gap> mat := IdentityMat(4,GF(16));
[ [ Z(2)^0, 0*Z(2), 0*Z(2), 0*Z(2) ], [ 0*Z(2), Z(2)^0, 0*Z(2), 0*Z(2) ], 
  [ 0*Z(2), 0*Z(2), Z(2)^0, 0*Z(2) ], [ 0*Z(2), 0*Z(2), 0*Z(2), Z(2)^0 ] ]
gap> phi := HermitianPolarityOfProjectiveSpace(mat,GF(16));
<polarity of PG(3, GF(2^4)) >
gap> geom := GeometryOfAbsolutePoints(phi);
<polar space in ProjectiveSpace(3,GF(2^4)): x_1^5+x_2^5+x_3^5+x_4^5=0 >
gap> mat := [[1,0,0,0],[0,0,1,1],[0,1,1,0],[0,1,0,0]]*Z(32)^0;
[ [ Z(2)^0, 0*Z(2), 0*Z(2), 0*Z(2) ], [ 0*Z(2), 0*Z(2), Z(2)^0, Z(2)^0 ], 
  [ 0*Z(2), Z(2)^0, Z(2)^0, 0*Z(2) ], [ 0*Z(2), Z(2)^0, 0*Z(2), 0*Z(2) ] ]
gap> phi := PolarityOfProjectiveSpace(mat,GF(32));
<polarity of PG(3, GF(2^5)) >
gap> geom := GeometryOfAbsolutePoints(phi);
<a plane in ProjectiveSpace(3, 32)>
gap> mat := [[1,0,0,0],[0,0,1,1],[0,1,1,0],[0,1,0,0]]*Z(32)^0;
[ [ Z(2)^0, 0*Z(2), 0*Z(2), 0*Z(2) ], [ 0*Z(2), 0*Z(2), Z(2)^0, Z(2)^0 ], 
  [ 0*Z(2), Z(2)^0, Z(2)^0, 0*Z(2) ], [ 0*Z(2), Z(2)^0, 0*Z(2), 0*Z(2) ] ]
gap> phi := PolarityOfProjectiveSpace(mat,GF(32));
<polarity of PG(3, GF(2^5)) >
gap> mat := IdentityMat(5,GF(7));
[ [ Z(7)^0, 0*Z(7), 0*Z(7), 0*Z(7), 0*Z(7) ], 
  [ 0*Z(7), Z(7)^0, 0*Z(7), 0*Z(7), 0*Z(7) ], 
  [ 0*Z(7), 0*Z(7), Z(7)^0, 0*Z(7), 0*Z(7) ], 
  [ 0*Z(7), 0*Z(7), 0*Z(7), Z(7)^0, 0*Z(7) ], 
  [ 0*Z(7), 0*Z(7), 0*Z(7), 0*Z(7), Z(7)^0 ] ]
gap> phi := PolarityOfProjectiveSpace(mat,GF(7));
<polarity of PG(4, GF(7)) >
gap> ps := PolarSpace(phi);
<polar space in ProjectiveSpace(4,GF(7)): x_1^2+x_2^2+x_3^2+x_4^2+x_5^2=0 >
gap> STOP_TEST("polaritiesnice.tst", 10000 );
