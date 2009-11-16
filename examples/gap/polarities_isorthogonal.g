#checking the polarity (orthogonal).
mat := [[1,0,2,0],[0,2,0,1],[2,0,0,0],[0,1,0,0]]*Z(9)^0;
phi := PolarityOfProjectiveSpace(mat,GF(9));
IsOrthogonalPolarityOfProjectiveSpace(phi);
quit;
