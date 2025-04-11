gap> START_TEST("Forms: representatives.tst");
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
gap> Representative(phi);
[ <immutable cmat 4x4 over GF(2,3)>, FrobeniusAutomorphism( GF(2^3) ) ]
gap> MatrixOfCollineation(phi);
<cmat 4x4 over GF(2,3)>
gap> BaseField(phi);
GF(2^3)
gap> FieldAutomorphism(phi);
FrobeniusAutomorphism( GF(2^3) )
gap> Order(phi);
21
gap> mat := [[1,0,0],[3,0,2],[0,5,4]]*Z(7^5);
[ [ Z(7^5), 0*Z(7), 0*Z(7) ], [ Z(7^5)^2802, 0*Z(7), Z(7^5)^5603 ], 
  [ 0*Z(7), Z(7^5)^14006, Z(7^5)^11205 ] ]
gap> field := GF(7^5);
GF(7^5)
gap> frob := FrobeniusAutomorphism(field);
FrobeniusAutomorphism( GF(7^5) )
gap> pg := PG(2,field);
ProjectiveSpace(2, 16807)
gap> delta := StandardDualityOfProjectiveSpace(pg);
StandardDuality( AllElementsOfIncidenceStructure( ProjectiveSpace(
2,GF(7^5)) ) )
gap> psi := CorrelationOfProjectiveSpace(mat,frob^3,field,delta);
<projective element with Frobenius with projectivespace isomorphism: <cmat 3x
3 over GF(7,5)>, F^
343, StandardDuality( AllElementsOfIncidenceStructure( ProjectiveSpace(
2,GF(7^5)) ) ) >
gap> Representative(psi);
[ <immutable cmat 3x3 over GF(7,5)>, FrobeniusAutomorphism( GF(7^5) )^3, 
  StandardDuality( AllElementsOfIncidenceStructure( ProjectiveSpace(
    2,GF(7^5)) ) ) ]
gap> MatrixOfCorrelation(psi);
<cmat 3x3 over GF(7,5)>
gap> BaseField(psi);
GF(7^5)
gap> FieldAutomorphism(psi);
FrobeniusAutomorphism( GF(7^5) )^3
gap> ProjectiveSpaceIsomorphism(psi);
StandardDuality( AllElementsOfIncidenceStructure( ProjectiveSpace(
2,GF(7^5)) ) )
gap> Order(psi);
80
gap> STOP_TEST("representatives.tst", 10000 );
