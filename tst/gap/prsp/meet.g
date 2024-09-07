#Meet
ps := PG(8,73^2);
hyp := HyperplaneByDualCoordinates(ps,[1,0,0,0,0,0,0,0,0]*Z(73)^0);
line := VectorSpaceToElement(ps,[[1,1,0,0,0,0,0,0,0],[1,0,1,0,0,0,0,0,0]]*Z(73)^0);
p := Meet(hyp,line);
Unpack(UnderlyingObject(p));
hyp2 := HyperplaneByDualCoordinates(ps,[0,1,0,0,0,0,0,0,0]*Z(73)^0);
sec := Meet(hyp,hyp2);
q := Meet(hyp2,line);
Meet(p,q);
Meet([hyp,hyp2,line]);
quit;
