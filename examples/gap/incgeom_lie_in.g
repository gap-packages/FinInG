#set theoretic containment.
p := VectorSpaceToElement(PG(3,3),[1,0,0,0]*Z(3)^0);
l := VectorSpaceToElement(PG(3,3),[[1,0,0,0],[0,1,0,0]]*Z(3)^0);
p in l;
p in p;
l in p;
l in PG(3,3);
quit;
