#onprojsubspacesextended demonstration
ps := ProjectiveSpace(3,27);
mat := IdentityMat(4,GF(27));
delta := StandardDualityOfProjectiveSpace(ps);
frob := FrobeniusAutomorphism(GF(27));
phi := CorrelationOfProjectiveSpace(mat,frob,GF(27),delta);
p := Random(Points(ps));
OnProjSubspacesExtended(p,phi);
l := Random(Lines(ps));
OnProjSubspacesExtended(p,phi);
psi := CorrelationOfProjectiveSpace(mat,frob^2,GF(27));
OnProjSubspacesExtended(p,psi);
OnProjSubspacesExtended(l,psi);
quit;
