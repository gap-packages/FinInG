#EmptysSubspace
ps := PG(3,9);
e := EmptySubspace(ps);
p := VectorSpaceToElement(ps,[1,1,0,0]*Z(9)^0);
e in p;
quit;
