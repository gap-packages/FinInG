#ambient space of an element of an affine space
as := AG(5,7);
solid := AffineSubspace(as,[1,0,0,1,0]*Z(7)^3,[[1,0,0,0,0],[0,1,1,1,0]]*Z(7)^0);
AmbientSpace(solid);
quit;
