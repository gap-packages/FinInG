#incidence in a polar space
ps := HyperbolicQuadric(7,7);
p := VectorSpaceToElement(ps,[1,0,1,0,0,0,0,0]*Z(7)^0);
l := VectorSpaceToElement(ps,[[1,0,1,0,0,0,0,0],[0,-1,0,1,0,0,0,0]]*Z(7)^0);
p * l;
l * p;
IsIncident(p,l);
p in l;
l in p;
e := EmptySubspace(ps);
e * l;
quit;
e in l;
l in ps;
quit;

