gap> START_TEST("Forms: projsemilinear.tst");
gap> mat := [[Z(2^8)^31,Z(2^8)^182,Z(2^8)^49],[Z(2^8)^224,Z(2^8)^25,Z(2^8)^45],
> [Z(2^8)^128,Z(2^8)^165,Z(2^8)^217]];
[ [ Z(2^8)^31, Z(2^8)^182, Z(2^8)^49 ], [ Z(2^8)^224, Z(2^8)^25, Z(2^8)^45 ], 
  [ Z(2^8)^128, Z(2^8)^165, Z(2^8)^217 ] ]
gap> frob := FrobeniusAutomorphism(GF(2^8));
FrobeniusAutomorphism( GF(2^8) )
gap> phi := ProjectiveSemilinearMap(mat,frob^2,GF(2^8));
< a collineation: <cmat 3x3 over GF(2,8)>, F^4>
gap> xi := ProjectiveSemilinearMap(mat,frob^0,GF(2^8));
< a collineation: <cmat 3x3 over GF(2,8)>, F^0>
gap> STOP_TEST("projsemilinear.tst", 10000 );
