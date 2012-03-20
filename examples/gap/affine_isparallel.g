# example: affine_isparallel.g
as := AffineSpace(3, 3);
l := AffineSubspace(as,[0,0,0]*Z(3)^0,[[1,0,0]]*Z(3)^0);
m := AffineSubspace(as,[1,0,0]*Z(3)^0,[[1,0,0]]*Z(3)^0);
n := AffineSubspace(as,[1,0,0]*Z(3)^0,[[0,1,0]]*Z(3)^0);
IsParallel(l,m);
IsParallel(m,n);
IsParallel(l,n);
quit;
