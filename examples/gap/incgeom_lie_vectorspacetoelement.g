#vectorspace to element for a Lie geometry
v := [1,1,1,0,0,0]*Z(7)^0;
w := [0,0,0,1,1,1]*Z(7)^0;
VectorSpaceToElement(PG(5,7),v);
VectorSpaceToElement(PG(5,7),[v,w]);
VectorSpaceToElement(SymplecticSpace(5,7),v);
VectorSpaceToElement(SymplecticSpace(5,7),[v,w]);
quit;
VectorSpaceToElement(HyperbolicQuadric(5,7),v);
quit;
VectorSpaceToElement(HyperbolicQuadric(5,7),0*v);
quit;
