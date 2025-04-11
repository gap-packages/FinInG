gap> START_TEST("Forms: correlation.tst");
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
gap> CorrelationOfProjectiveSpace(mat,field);
<projective element with Frobenius with projectivespace isomorphism: <cmat 3x
3 over GF(7,5)>, F^0, IdentityMapping( <All elements of ProjectiveSpace(2, 
16807)> ) >
gap> CorrelationOfProjectiveSpace(mat,frob,field);
<projective element with Frobenius with projectivespace isomorphism: <cmat 3x
3 over GF(7,5)>, F^7, IdentityMapping( <All elements of ProjectiveSpace(2, 
16807)> ) >
gap> CorrelationOfProjectiveSpace(mat,frob^2,field);
<projective element with Frobenius with projectivespace isomorphism: <cmat 3x
3 over GF(7,5)>, F^49, IdentityMapping( <All elements of ProjectiveSpace(2, 
16807)> ) >
gap> CorrelationOfProjectiveSpace(mat,field,delta);
<projective element with Frobenius with projectivespace isomorphism: <cmat 3x
3 over GF(7,
5)>, F^0, StandardDuality( AllElementsOfIncidenceStructure( ProjectiveSpace(
2,GF(7^5)) ) ) >
gap> CorrelationOfProjectiveSpace(mat,frob^3,field,delta);
<projective element with Frobenius with projectivespace isomorphism: <cmat 3x
3 over GF(7,5)>, F^
343, StandardDuality( AllElementsOfIncidenceStructure( ProjectiveSpace(
2,GF(7^5)) ) ) >
gap> CorrelationOfProjectiveSpace(mat,frob^4,field,delta);
<projective element with Frobenius with projectivespace isomorphism: <cmat 3x
3 over GF(7,5)>, F^
2401, StandardDuality( AllElementsOfIncidenceStructure( ProjectiveSpace(
2,GF(7^5)) ) ) >
gap> Correlation(pg,mat,frob,delta);
<projective element with Frobenius with projectivespace isomorphism: <cmat 3x
3 over GF(7,5)>, F^
7, StandardDuality( AllElementsOfIncidenceStructure( ProjectiveSpace(
2,GF(7^5)) ) ) >
gap> STOP_TEST("correlation.tst", 10000 );
