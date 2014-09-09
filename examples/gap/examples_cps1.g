#constructing some polar spaces.
ps := HermitianPolarSpace(4,9);
EquationForPolarSpace(ps);
ps := HyperbolicQuadric(5,7);
EquationForPolarSpace(ps);
ps := SymplecticSpace(3,3);
EquationForPolarSpace(ps);
mat := IdentityMat(4,GF(11));
form := BilinearFormByMatrix(mat,GF(11));
ps := PolarSpace(form);
Rank(ps);
ps;
quit;

