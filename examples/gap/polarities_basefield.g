#basefield of a polarity.
mat := [[1,0,0],[0,0,2],[0,2,0]]*Z(5)^0;
phi := PolarityOfProjectiveSpace(mat,GF(25));
BaseField(phi);
quit;
