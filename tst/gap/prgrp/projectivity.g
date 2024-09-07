#Projectivity
mat := [[1,0,0],[0,1,0],[0,0,1]]*Z(25)^0;
Projectivity(mat,GF(25));
Projectivity(mat,GF(625));
Projectivity(PG(2,25),mat);
Projectivity(PG(2,625),mat);
quit;


