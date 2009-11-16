#elementary example how to construct a projectivity
mat := [[1,0,0],[0,1,0],[0,0,1]]*Z(9)^0;
Projectivity(mat,GF(9));
quit;
