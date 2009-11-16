#constructing hermitian polarities of a projective space.
mat := [[Z(11)^0,0*Z(11),0*Z(11)],[0*Z(11),0*Z(11),Z(11)],
    [0*Z(11),Z(11),0*Z(11)]];
frob := FrobeniusAutomorphism(GF(121));
phi := PolarityOfProjectiveSpace(mat,frob,GF(121));
psi := HermitianPolarityOfProjectiveSpace(mat,GF(121));
phi = psi;
quit;
