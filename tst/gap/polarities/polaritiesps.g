#Constructing Polarities of projective spaces
mat := [[0,1,0],[1,0,0],[0,0,1]]*Z(169)^0;
phi := PolarityOfProjectiveSpace(mat,GF(169));
mat := [[Z(11)^0,0*Z(11),0*Z(11)],[0*Z(11),0*Z(11),Z(11)],
    [0*Z(11),Z(11),0*Z(11)]];
frob := FrobeniusAutomorphism(GF(121));
phi := PolarityOfProjectiveSpace(mat,frob,GF(121));
psi := HermitianPolarityOfProjectiveSpace(mat,GF(121));
phi = psi;
q := 9;
mat := [[0,1/2,0],[1/2,0,0],[0,0,1]]*Z(q)^0;
form := HermitianFormByMatrix(mat,GF(9));
phi := PolarityOfProjectiveSpace(form);
mat := [[0,1,0,0],[1,0,0,0],[0,0,0,1],[0,0,1,0]]*Z(16)^0;
form := BilinearFormByMatrix(mat,GF(16));
phi := PolarityOfProjectiveSpace(form);
ps := HermitianPolarSpace(4,64);
phi := PolarityOfProjectiveSpace(ps);
quit;
