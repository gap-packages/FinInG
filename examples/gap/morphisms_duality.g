#morphisms_duality.g
w := SymplecticSpace(3,5);
lines:=AsList(Lines(w));;
duality := NaturalDuality(w);
l:=lines[1];
l^duality;
PreImageElm(duality,last);
hom := Intertwiner(duality);
q := 5;
q5q := EllipticQuadric(5,q);
mat := [[0,1,0,0],[1,0,0,0],[0,0,0,Z(q)],[0,0,Z(q),0]]*Z(q)^0;
hform := HermitianFormByMatrix(mat,GF(q^2));
herm := PolarSpace(hform);
duality := NaturalDuality(q5q,herm,true);
hom := Intertwiner(duality);
g := Random(CollineationGroup(q5q));
g^hom;
quit;
