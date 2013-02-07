#the empty subspace
ps := HermitianPolarSpace(10,49);
e := EmptySubspace(ps);
p := VectorSpaceToElement(ps,[1,1,1,0,1,1,1,0,1,0,0]*Z(7)^0);
e*p;
quit;
e in p;
quit;
