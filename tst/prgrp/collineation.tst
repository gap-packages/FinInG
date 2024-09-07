gap> START_TEST("Forms: collineation.tst");
gap> mat:=
> [[Z(2^3)^6,Z(2^3),Z(2^3)^3,Z(2^3)^3],[Z(2^3)^6,Z(2)^0,Z(2^3)^2,Z(2^3)^3],
> [0*Z(2),Z(2^3)^4,Z(2^3),Z(2^3)],[Z(2^3)^6,Z(2^3)^5,Z(2^3)^3,Z(2^3)^5 ]];
[ [ Z(2^3)^6, Z(2^3), Z(2^3)^3, Z(2^3)^3 ], 
  [ Z(2^3)^6, Z(2)^0, Z(2^3)^2, Z(2^3)^3 ], 
  [ 0*Z(2), Z(2^3)^4, Z(2^3), Z(2^3) ], 
  [ Z(2^3)^6, Z(2^3)^5, Z(2^3)^3, Z(2^3)^5 ] ]
gap> frob := FrobeniusAutomorphism(GF(8));
FrobeniusAutomorphism( GF(2^3) )
gap> phi := CollineationOfProjectiveSpace(mat,frob,GF(8));
< a collineation: <cmat 4x4 over GF(2,3)>, F^2>
gap> psi := CollineationOfProjectiveSpace(mat,GF(8));
< a collineation: <cmat 4x4 over GF(2,3)>, F^0>
gap> phi = psi;
false
gap> phi := CollineationOfProjectiveSpace(PG(3,8),mat,frob);
< a collineation: <cmat 4x4 over GF(2,3)>, F^2>
gap> psi := CollineationOfProjectiveSpace(PG(3,8),mat);
< a collineation: <cmat 4x4 over GF(2,3)>, F^0>
gap> phi = psi;
false
gap> phi := Collineation(PG(3,8),mat,frob);
< a collineation: <cmat 4x4 over GF(2,3)>, F^2>
gap> psi := Collineation(PG(3,8),mat);
< a collineation: <cmat 4x4 over GF(2,3)>, F^0>
gap> phi = psi;
false
gap> STOP_TEST("collineation.tst", 10000 );
