#projective dimension of an element
ps := PG(2,5);
v := [[1,1,0],[0,3,2]]*Z(5)^0;
line := VectorSpaceToElement(ps,v);
ProjectiveDimension(line);
Dimension(line);
p := VectorSpaceToElement(ps,[1,2,3]*Z(5)^0);
ProjectiveDimension(p);
Dimension(p);
ProjectiveDimension(EmptySubspace(ps));
quit;
