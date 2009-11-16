#gram matrix of a polarity.
mat := [[1,0,0],[0,0,3],[0,3,0]]*Z(11)^0;
phi := PolarityOfProjectiveSpace(mat,GF(11));
GramMatrix(phi);
quit;
