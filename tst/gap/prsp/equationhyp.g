#EquationOfHyperplane
pg := PG(4,23);
hyp := HyperplaneByDualCoordinates(pg,[2,8,16,11,5]*Z(23)^0);
vect := UnderlyingObject(hyp);
Display(Unpack(vect));
EquationOfHyperplane(hyp);
quit;
