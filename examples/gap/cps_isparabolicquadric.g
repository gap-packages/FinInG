#is elliptic quadric property
mat := IdentityMat(5,GF(9));
form := BilinearFormByMatrix(mat,GF(9));
ps := PolarSpace(form);
IsParabolicQuadric(ps);
mat := [[1,0,0,0,0],[0,0,1,0,0],[0,0,0,0,0],[0,0,0,0,1],[0,0,0,0,0]]*Z(2)^0;
form := QuadraticFormByMatrix(mat,GF(8));
ps := PolarSpace(form);
IsParabolicQuadric(ps);
quit;
