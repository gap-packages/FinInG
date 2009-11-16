#checking the polarity (symplectic).
mat := [[0,0,1,0],[0,0,0,1],[1,0,0,0],[0,1,0,0]]*Z(8)^0;
phi := PolarityOfProjectiveSpace(mat,GF(8));
IsSymplecticPolarityOfProjectiveSpace(phi);
quit;
