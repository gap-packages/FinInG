#getting the form from the polarity.
mat := [[0,2,7,1],[2,0,3,0],[7,3,0,1],[1,0,1,0]]*Z(19)^0;
frob := FrobeniusAutomorphism(GF(19^4));
phi := PolarityOfProjectiveSpace(mat,frob^2,GF(19^4));
IsHermitianPolarityOfProjectiveSpace(phi);
quit;
