#BaseField (of an element)
ps := PG(11,37^3);
p := VectorSpaceToElement(ps,[10,32,1,10,5,14,28,-1,19,2,65,8]*Z(37^3)^0);
BaseField(p);
e := EmptySubspace(ps);
BaseField(e);
hyp := HyperplaneByDualCoordinates(ps,[10,32,1,10,5,14,28,-1,19,2,65,8]*Z(37^3)^0);
BaseField(hyp);
quit;
