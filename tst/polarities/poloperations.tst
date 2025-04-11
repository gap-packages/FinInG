gap> START_TEST("Forms: poloperations.tst");
gap> mat := [[1,0,0],[0,0,2],[0,2,0]]*Z(5)^0;
[ [ Z(5)^0, 0*Z(5), 0*Z(5) ], [ 0*Z(5), 0*Z(5), Z(5) ], 
  [ 0*Z(5), Z(5), 0*Z(5) ] ]
gap> phi := PolarityOfProjectiveSpace(mat,GF(25));
<polarity of PG(2, GF(5^2)) >
gap> BaseField(phi);
GF(5^2)
gap> mat := [[1,0,0],[0,0,3],[0,3,0]]*Z(11)^0;
[ [ Z(11)^0, 0*Z(11), 0*Z(11) ], [ 0*Z(11), 0*Z(11), Z(11)^8 ], 
  [ 0*Z(11), Z(11)^8, 0*Z(11) ] ]
gap> phi := PolarityOfProjectiveSpace(mat,GF(11));
<polarity of PG(2, GF(11)) >
gap> GramMatrix(phi);
<immutable cmat 3x3 over GF(11,1)>
gap> mat := [[0,2,7,1],[2,0,3,0],[7,3,0,1],[1,0,1,0]]*Z(19)^0;
[ [ 0*Z(19), Z(19), Z(19)^6, Z(19)^0 ], [ Z(19), 0*Z(19), Z(19)^13, 0*Z(19) ],
  [ Z(19)^6, Z(19)^13, 0*Z(19), Z(19)^0 ], 
  [ Z(19)^0, 0*Z(19), Z(19)^0, 0*Z(19) ] ]
gap> frob := FrobeniusAutomorphism(GF(19^4));
FrobeniusAutomorphism( GF(19^4) )
gap> phi := PolarityOfProjectiveSpace(mat,frob^2,GF(19^4));
<polarity of PG(3, GF(19^4)) >
gap> IsHermitianPolarityOfProjectiveSpace(phi);
true
gap> mat := [[1,0,2,0],[0,2,0,1],[2,0,0,0],[0,1,0,0]]*Z(9)^0;
[ [ Z(3)^0, 0*Z(3), Z(3), 0*Z(3) ], [ 0*Z(3), Z(3), 0*Z(3), Z(3)^0 ], 
  [ Z(3), 0*Z(3), 0*Z(3), 0*Z(3) ], [ 0*Z(3), Z(3)^0, 0*Z(3), 0*Z(3) ] ]
gap> phi := PolarityOfProjectiveSpace(mat,GF(9));
<polarity of PG(3, GF(3^2)) >
gap> IsOrthogonalPolarityOfProjectiveSpace(phi);
true
gap> mat := [[1,0,1,0],[0,1,0,1],[1,0,0,0],[0,1,0,0]]*Z(16)^0;
[ [ Z(2)^0, 0*Z(2), Z(2)^0, 0*Z(2) ], [ 0*Z(2), Z(2)^0, 0*Z(2), Z(2)^0 ], 
  [ Z(2)^0, 0*Z(2), 0*Z(2), 0*Z(2) ], [ 0*Z(2), Z(2)^0, 0*Z(2), 0*Z(2) ] ]
gap> phi := PolarityOfProjectiveSpace(mat,GF(16));
<polarity of PG(3, GF(2^4)) >
gap> IsPseudoPolarityOfProjectiveSpace(phi);
true
gap> mat := [[0,0,1,0],[0,0,0,1],[1,0,0,0],[0,1,0,0]]*Z(8)^0;
[ [ 0*Z(2), 0*Z(2), Z(2)^0, 0*Z(2) ], [ 0*Z(2), 0*Z(2), 0*Z(2), Z(2)^0 ], 
  [ Z(2)^0, 0*Z(2), 0*Z(2), 0*Z(2) ], [ 0*Z(2), Z(2)^0, 0*Z(2), 0*Z(2) ] ]
gap> phi := PolarityOfProjectiveSpace(mat,GF(8));
<polarity of PG(3, GF(2^3)) >
gap> IsSymplecticPolarityOfProjectiveSpace(phi);
true
gap> mat := [[0,-2,0,1],[2,0,3,0],[0,-3,0,1],[-1,0,-1,0]]*Z(19)^0;
[ [ 0*Z(19), Z(19)^10, 0*Z(19), Z(19)^0 ], 
  [ Z(19), 0*Z(19), Z(19)^13, 0*Z(19) ], 
  [ 0*Z(19), Z(19)^4, 0*Z(19), Z(19)^0 ], 
  [ Z(19)^9, 0*Z(19), Z(19)^9, 0*Z(19) ] ]
gap> phi := PolarityOfProjectiveSpace(mat,GF(19));
<polarity of PG(3, GF(19)) >
gap> form := SesquilinearForm(phi);
< non-degenerate bilinear form >
gap> STOP_TEST("poloperations.tst", 10000 );
