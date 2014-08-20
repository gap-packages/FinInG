#collineation group of classical hexagon
mat := IdentityMat(7,GF(9));
form := BilinearFormByMatrix(mat,GF(9));
ps := PolarSpace(form);
gh := SplitCayleyHexagon(ps);
group := CollineationGroup(gh);
time;
HasNiceMonomorphism(group);
gh := TwistedTrialityHexagon(2^3);
group := CollineationGroup(gh);
quit;

