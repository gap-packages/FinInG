#FlagOfIncidenceStructure.g
ps := PG(3,13);
plane := HyperplaneByDualCoordinates(ps,[1,0,0,0]*Z(13)^0);
line := VectorSpaceToElement(ps,[[0,1,0,0],[0,0,1,0]]*Z(13)^0);
pt := VectorSpaceToElement(ps,[0,1,0,0]*Z(13)^0);
flag := FlagOfIncidenceStructure(ps,[pt,line,plane]);
IsEmptyFlag(flag);
IsChamberOfIncidenceStructure(flag);
quit;


