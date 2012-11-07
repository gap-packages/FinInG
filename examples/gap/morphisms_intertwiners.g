#morphisms: Intertwiners
form := BilinearFormByMatrix( IdentityMat(3,GF(3)), GF(3) );
ps := PolarSpace(form);
pq := ParabolicQuadric(2,3);
iso := IsomorphismPolarSpaces(ps, pq);
KnownAttributesOfObject(iso);
hom := Intertwiner(iso);
quit;

