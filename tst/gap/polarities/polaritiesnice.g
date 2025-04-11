#Nice examples with polarities
mat := IdentityMat(4,GF(3));
phi := PolarityOfProjectiveSpace(mat,GF(3));
points := AbsolutePoints(phi);
List(points);
mat := [[0,1,0,0],[1,0,0,0],[0,0,0,1],[0,0,1,0]]*Z(5)^0;
phi := HermitianPolarityOfProjectiveSpace(mat,GF(25));
mat2 := IdentityMat(4,GF(5));
psi := PolarityOfProjectiveSpace(mat2,GF(25));
phi*psi = psi*phi;
g := CorrelationCollineationGroup(PG(3,25));
h := CollineationGroup(PG(3,25));
hom := Embedding(h,g);
coll := PreImagesRepresentative(hom,phi*psi);
mat := IdentityMat(4,GF(16));
phi := HermitianPolarityOfProjectiveSpace(mat,GF(16));
geom := GeometryOfAbsolutePoints(phi);
mat := [[1,0,0,0],[0,0,1,1],[0,1,1,0],[0,1,0,0]]*Z(32)^0;
phi := PolarityOfProjectiveSpace(mat,GF(32));
geom := GeometryOfAbsolutePoints(phi);
mat := [[1,0,0,0],[0,0,1,1],[0,1,1,0],[0,1,0,0]]*Z(32)^0;
phi := PolarityOfProjectiveSpace(mat,GF(32));
mat := IdentityMat(5,GF(7));
phi := PolarityOfProjectiveSpace(mat,GF(7));
ps := PolarSpace(phi);
quit;

