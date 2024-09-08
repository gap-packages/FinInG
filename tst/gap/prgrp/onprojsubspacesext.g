#OnProjSubspacesExtended
ps := ProjectiveSpace(3,27);
mat := IdentityMat(4,GF(27));
delta := StandardDualityOfProjectiveSpace(ps);
frob := FrobeniusAutomorphism(GF(27));
phi := CorrelationOfProjectiveSpace(mat,frob,GF(27),delta);
p := VectorSpaceToElement(ps,[Z(3)^0,Z(3^3)^9,Z(3^3)^11,Z(3)]);
OnProjSubspacesExtended(p,phi);
l := VectorSpaceToElement(ps,[[Z(3)^0,0*Z(3),Z(3),Z(3^3)^6],
[0*Z(3),Z(3)^0,0*Z(3),Z(3^3)^20]]);
OnProjSubspacesExtended(l,phi);
psi := CorrelationOfProjectiveSpace(mat,frob^2,GF(27));
OnProjSubspacesExtended(p,psi);
OnProjSubspacesExtended(l,psi);
quit;
