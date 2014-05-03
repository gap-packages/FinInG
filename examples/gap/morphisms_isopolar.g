#morphisms_isopolar.g
mat1 := IdentityMat(6,GF(5));
form1 := BilinearFormByMatrix(mat1,GF(5));
ps1 := PolarSpace(form1);
mat2 := [[0,0,0,0,0,1],[0,0,0,0,1,0],[0,0,0,1,0,0],
[0,0,0,0,0,0],[0,0,0,0,0,0],[0,0,0,0,0,0]]*Z(5)^0;
form2 := QuadraticFormByMatrix(mat2,GF(5));
ps2 := PolarSpace(form2);
iso := IsomorphismPolarSpaces(ps1,ps2,true);
CollineationGroup(ps1);
CollineationGroup(ps2);
iso := IsomorphismPolarSpaces(ps1,ps2,true);
hom := Intertwiner( iso );
ps1 := ParabolicQuadric(6,8);
ps2 := SymplecticSpace(5,8);
em := IsomorphismPolarSpaces(ps1,ps2);
hom := Intertwiner(em);
quit;
