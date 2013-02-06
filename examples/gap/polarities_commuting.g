#commuting polarities
mat := [[0,1,0,0],[1,0,0,0],[0,0,0,1],[0,0,1,0]]*Z(5)^0;
phi := HermitianPolarityOfProjectiveSpace(mat,GF(25));
mat2 := IdentityMat(4,GF(5));
psi := PolarityOfProjectiveSpace(mat2,GF(25));
phi*psi = psi*phi;
g := CorrelationCollineationGroup(PG(3,25));
h := CollineationGroup(PG(3,25));
hom := Embedding(h,g);
coll := PreImagesRepresentative(hom,phi*psi);
quit;
