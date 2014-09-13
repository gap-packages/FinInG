# isomorphic polar spaces
mat1 := IdentityMat(4,GF(16));
form1 := HermitianFormByMatrix(mat1,GF(16));
ps1 := PolarSpace(form1);
mat2 := [[0,1,0,0],[1,0,0,0],[0,0,0,1],[0,0,1,0]]*Z(16)^0;
form2 := HermitianFormByMatrix(mat2,GF(16));
ps2 := PolarSpace(form2);
map := IsomorphismPolarSpaces(ps1,ps2);
p := Random(Points(ps1));
p^map;
l := Random(Lines(ps2));
PreImageElm(map,l);
quit;

