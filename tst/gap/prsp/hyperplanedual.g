#HyperplaneByDualCoorindates
ps := PG(8,13);
hyp := HyperplaneByDualCoordinates(ps,[1,2,3,4,5,6,7,8,9]*Z(13)^0);
vect := UnderlyingObject(hyp);
Display(Unpack(vect));
quit;

