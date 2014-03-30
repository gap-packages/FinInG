#check whether a polar space is canonical
mat := [[0,1,0,0],[0,0,0,0],[0,0,0,1],[0,0,0,0]]*Z(5)^0;
form := QuadraticFormByMatrix(mat,GF(5));
ps := PolarSpace(form);
IsCanonicalPolarSpace(ps);
ps;
mat := [[1,0,0],[0,0,1],[0,1,0]]*Z(3)^0;
form := QuadraticFormByMatrix(mat,GF(3));
ps := PolarSpace(form);
IsCanonicalPolarSpace(ps);
ps;
quit;

