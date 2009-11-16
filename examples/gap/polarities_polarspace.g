#absolute points wrt polarity
mat := [[1,0,0,0],[0,0,1,1],[0,1,1,0],[0,1,0,0]]*Z(32)^0;
phi := PolarityOfProjectiveSpace(mat,GF(32));
ps := PolarSpace(phi);
quit;
mat := IdentityMat(5,GF(7));
phi := PolarityOfProjectiveSpace(mat,GF(7));
ps := PolarSpace(phi);
quit;
