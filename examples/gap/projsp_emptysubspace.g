#the empty subspace
e := EmptySubspace(PG(5,9));
p := VectorSpaceToElement(PG(5,9),[1,0,0,0,0,0]*Z(9)^0);
e*p;
quit;
e in p;
quit;
