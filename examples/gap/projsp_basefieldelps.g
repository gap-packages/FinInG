#base field of a projective space
ps := PG(5,8);
p := VectorSpaceToElement(ps,[1,1,1,0,0,1]*Z(2));
BaseField(p);
quit;
