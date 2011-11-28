# coordinates of a point of a polar space
ps := ParabolicQuadric(6,5);
p := VectorSpaceToElement(ps,[0,1,0,0,0,0,0]*Z(5)^0);
Coordinates(p);
quit;
