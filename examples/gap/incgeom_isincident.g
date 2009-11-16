#testing incidence: to be done
ps := ProjectiveSpace(4,7);
p := VectorSpaceToElement(ps,[1,0,1,0,1]*Z(7)^0);
l := VectorSpaceToElement(ps,[[0,0,1,0,0],[1,0,0,0,1]]*Z(7)^0);
pl := VectorSpaceToElement(ps,[[1,1,1,0,0],[0,1,0,0,0],
                               [0,-1,0,0,1]]*Z(7)^0);
p in l;
l in pl;
p in pl;
IsIncident(p,l);
IsIncident(l,p);
IsIncident(pl,p);
pl in p;
quit;

