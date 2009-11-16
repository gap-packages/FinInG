#getting the polarity from a form.
mat := [[0,1,0,0],[1,0,0,0],[0,0,0,1],[0,0,1,0]]*Z(16)^0;
form := BilinearFormByMatrix(mat,GF(16));
phi := PolarityOfProjectiveSpace(form);
quit;
