#projective dimension of an element
ps := EllipticQuadric(7,8);
mat := [[0,0,1,0,0,0,0,0],[0,0,0,0,1,0,0,0]]*Z(8)^0;
line := VectorSpaceToElement(ps,mat);
ProjectiveDimension(line);
Dimension(line);
e := EmptySubspace(ps);
ProjectiveDimension(e);
quit;

