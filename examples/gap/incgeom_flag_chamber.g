#a flag and chamber of some incidence structure
ps := PG(3,7);
point := VectorSpaceToElement(ps,[1,2,0,0]*Z(7)^0);
line := VectorSpaceToElement(ps,[[1,0,0,0],[0,1,0,0]]*Z(7)^0);
plane := VectorSpaceToElement(ps,[[1,0,0,0],[0,1,0,0],[0,0,0,1]]*Z(7)^0);
flag1 := FlagOfIncidenceStructure(ps,[point,plane]);
IsChamberOfIncidenceStructure(flag1);
flag2 := FlagOfIncidenceStructure(ps,[point,line,plane]);
IsChamberOfIncidenceStructure(flag2);
quit;
