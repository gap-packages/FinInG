#Incidence en Containment
ps := ProjectiveSpace(5,81);
p := VectorSpaceToElement(ps,[1,1,1,1,0,0]*Z(9)^0);
l := VectorSpaceToElement(ps,[[1,1,1,1,0,0],[0,0,0,0,1,0]]*Z(9)^0);
plane := VectorSpaceToElement(ps,[[1,0,0,0,0,0],[0,1,0,0,0,0],[0,0,1,0,0,0]]*Z(9)^0);
p * l;
l * p;
IsIncident(p,l);
p in l;
l in p;
p * plane;
l * plane;
l in plane;
e := EmptySubspace(ps);
e in l;
l in ps;
quit;

