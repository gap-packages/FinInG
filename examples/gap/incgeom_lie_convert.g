#convert element
p := VectorSpaceToElement(PG(3,7),[0,1,0,0]*Z(7)^0);
ConvertElement(HyperbolicQuadric(3,7),p);
r := VectorSpaceToElement(PG(3,7),[1,1,0,0]*Z(7)^0);
ConvertElement(HyperbolicQuadric(3,7),r);
quit;
quit;
