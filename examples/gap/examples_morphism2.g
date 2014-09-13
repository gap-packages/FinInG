# intertwiner
mat1 := IdentityMat(4,GF(16));
form1 := HermitianFormByMatrix(mat1,GF(16));
ps1 := PolarSpace(form1);
mat2 := [[0,1,0,0],[1,0,0,0],[0,0,0,1],[0,0,1,0]]*Z(16)^0;
form2 := HermitianFormByMatrix(mat2,GF(16));
ps2 := PolarSpace(form2);
CollineationGroup(ps1);
map := IsomorphismPolarSpaces(ps1,ps2);
phi := Intertwiner(map);
g := Random(CollineationGroup(ps1));
h := g^phi;
h in CollineationGroup(ps2);
h := Random(CollineationGroup(ps2));
g := PreImageElm(phi,h);
g in CollineationGroup(ps1);
quit;

