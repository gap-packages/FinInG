#basefield of a projectivity
mat := [[0,1,0],[1,0,0],[0,0,2]]*Z(3)^0;
g := Projectivity(mat,GF(3^6));
BaseField(g);
quit;
