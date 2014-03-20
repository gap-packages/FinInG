#checking the polarity (pseudo).
mat := [[1,0,1,0],[0,1,0,1],[1,0,0,0],[0,1,0,0]]*Z(16)^0;
phi := PolarityOfProjectiveSpace(mat,GF(16));
IsPseudoPolarityOfProjectiveSpace(phi);
quit;
