#constructing a polar space using forms
mat := [[0,0,0,1],[0,0,-2,0],[0,2,0,0],[-1,0,0,0]]*Z(5)^0;
form := BilinearFormByMatrix(mat,GF(25));
ps := PolarSpace(form);
r := PolynomialRing(GF(32),4);
poly := r.3*r.2+r.1*r.4;
form := QuadraticFormByPolynomial(poly,r);
ps := PolarSpace(form);
mat := IdentityMat(5,GF(7));
phi := PolarityOfProjectiveSpace(mat,GF(7));
ps := PolarSpace(phi);
quit;
