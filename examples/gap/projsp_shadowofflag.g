#shadow of a flag
ps := PG(5,7);
p := VectorSpaceToElement(ps,[1,0,0,0,0,0]*Z(7)^0);
l := VectorSpaceToElement(ps,[[1,0,0,0,0,0],[0,1,0,0,0,0]]*Z(7)^0);
v := VectorSpaceToElement(ps,[[1,0,0,0,0,0],[0,1,0,0,0,0],[0,0,1,0,0,0]]*Z(7)^0);
flag := FlagOfIncidenceStructure(ps,[v,l,p]);
s := ShadowOfFlag(ps,flag,4);
s := ShadowOfFlag(ps,flag,"solids");
quit;
