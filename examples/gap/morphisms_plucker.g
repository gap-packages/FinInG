#easy example of PluckerCoordinates
pg := PG(3,169);
l := Random(Lines(pg));
vec := PluckerCoordinates(l);
mat := [[0,0,0,0,0,1],[0,0,0,0,1,0],[0,0,0,1,0,0],
[0,0,0,0,0,0],[0,0,0,0,0,0],[0,0,0,0,0,0]]*Z(13)^0;
form := QuadraticFormByMatrix(mat,GF(169));
klein := PolarSpace(form);
VectorSpaceToElement(klein,vec);
quit;

