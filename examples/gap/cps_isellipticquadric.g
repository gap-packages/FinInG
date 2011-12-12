#is elliptic quadric property
mat := IdentityMat(6,GF(5));
form := BilinearFormByMatrix(mat,GF(5));
ps := PolarSpace(form);
IsEllipticQuadric(ps);
mat := IdentityMat(6,GF(7));
form := BilinearFormByMatrix(mat,GF(7));
ps := PolarSpace(form);
IsEllipticQuadric(ps);
quit;
