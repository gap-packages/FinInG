gap> START_TEST("Forms: onprojsubspacesext.tst");
gap> ps := ProjectiveSpace(3,27);
ProjectiveSpace(3, 27)
gap> mat := IdentityMat(4,GF(27));
[ [ Z(3)^0, 0*Z(3), 0*Z(3), 0*Z(3) ], [ 0*Z(3), Z(3)^0, 0*Z(3), 0*Z(3) ], 
  [ 0*Z(3), 0*Z(3), Z(3)^0, 0*Z(3) ], [ 0*Z(3), 0*Z(3), 0*Z(3), Z(3)^0 ] ]
gap> delta := StandardDualityOfProjectiveSpace(ps);
StandardDuality( AllElementsOfIncidenceStructure( ProjectiveSpace(
3,GF(3^3)) ) )
gap> frob := FrobeniusAutomorphism(GF(27));
FrobeniusAutomorphism( GF(3^3) )
gap> phi := CorrelationOfProjectiveSpace(mat,frob,GF(27),delta);
<projective element with Frobenius with projectivespace isomorphism: <cmat 4x
4 over GF(3,3)>, F^
3, StandardDuality( AllElementsOfIncidenceStructure( ProjectiveSpace(
3,GF(3^3)) ) ) >
gap> p := VectorSpaceToElement(ps,[Z(3)^0,Z(3^3)^9,Z(3^3)^11,Z(3)]);
<a point in ProjectiveSpace(3, 27)>
gap> OnProjSubspacesExtended(p,phi);
<a plane in ProjectiveSpace(3, 27)>
gap> l := VectorSpaceToElement(ps,[[Z(3)^0,0*Z(3),Z(3),Z(3^3)^6],
> [0*Z(3),Z(3)^0,0*Z(3),Z(3^3)^20]]);
<a line in ProjectiveSpace(3, 27)>
gap> OnProjSubspacesExtended(l,phi);
<a line in ProjectiveSpace(3, 27)>
gap> psi := CorrelationOfProjectiveSpace(mat,frob^2,GF(27));
<projective element with Frobenius with projectivespace isomorphism: <cmat 4x
4 over GF(3,3)>, F^9, IdentityMapping( <All elements of ProjectiveSpace(3, 
27)> ) >
gap> OnProjSubspacesExtended(p,psi);
<a point in ProjectiveSpace(3, 27)>
gap> OnProjSubspacesExtended(l,psi);
<a line in ProjectiveSpace(3, 27)>
gap> STOP_TEST("onprojsubspacesext.tst", 10000 );
