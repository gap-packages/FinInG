q := 9;
mat := [[0,1/2,0],[1/2,0,0],[0,0,1]]*Z(q)^0;
form := HermitianFormByMatrix(mat,GF(9));
phi := PolarityOfProjectiveSpace(form);
Print(phi);
