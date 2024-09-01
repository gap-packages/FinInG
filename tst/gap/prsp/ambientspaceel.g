#AmbientSpace (of an element)
ps := PG(5,29^2);
p := VectorSpaceToElement(ps,[23,2,17,0,5,8]*Z(29^3)^0);
AmbientSpace(p);
e := EmptySubspace(ps);
AmbientSpace(e);
hyp := HyperplaneByDualCoordinates(ps,[23,2,17,0,5,8]*Z(29^3)^0);
AmbientSpace(hyp);
quit;
