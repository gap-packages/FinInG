#onprojsubspacesreversing
ps := ProjectiveSpace(3,27);
mat := IdentityMat(4,GF(27));
delta := StandardDualityOfProjectiveSpace(ps);
frob := FrobeniusAutomorphism(GF(27));
phi := CorrelationOfProjectiveSpace(mat,frob,GF(27),delta);
p := Random(Points(ps));
OnProjSubspacesReversing(p,phi);
l := Random(Lines(ps));
OnProjSubspacesReversing(p,phi);
psi := CorrelationOfProjectiveSpace(mat,frob^2,GF(27));
OnProjSubspacesReversing(p,psi);
OnProjSubspacesReversing(l,psi);
quit;
