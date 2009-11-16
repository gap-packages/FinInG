#constructing polarities (1)
mat := [[0,1,0],[1,0,0],[0,0,1]]*Z(169)^0;
phi := PolarityOfProjectiveSpace(mat,GF(169));
quit;
