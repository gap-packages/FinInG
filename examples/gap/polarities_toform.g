#getting the form from the polarity.
mat := [[0,-2,0,1],[2,0,3,0],[0,-3,0,1],[-1,0,-1,0]]*Z(19)^0;
phi := PolarityOfProjectiveSpace(mat,GF(19));
form := SesquilinearForm(phi);
quit;
