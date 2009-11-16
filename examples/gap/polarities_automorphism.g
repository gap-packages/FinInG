#basefield of a polarity.
mat := [[0,2,0,0],[2,0,0,0],[0,0,0,5],[0,0,5,0]]*Z(7)^0;
phi := HermitianPolarityOfProjectiveSpace(mat,GF(49));
CompanionAutomorphism(phi);
quit;
