#basefield of an element of an affine space	
as := AG(5,11);
sub := AffineSubspace(as,[1,4,3,1,0]*Z(11)^5,[[1,0,0,0,0],[0,1,1,1,0],
[0,0,0,0,1]]*Z(11)^0);
BaseField(sub);
quit;
