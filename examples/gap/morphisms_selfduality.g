# selfduality
q := 16;
mat := [[0,1,0,0,0],[0,0,0,0,0],[0,0,1,0,0],[0,0,0,0,0],[0,0,0,1,0]]*Z(q)^0;
form := QuadraticFormByMatrix(mat,GF(q));
q4q := PolarSpace(form);
em := SelfDuality(q4q);
CollineationGroup(q4q);
em := SelfDuality(q4q);
hom := Intertwiner(em);
q := 16;
w := SymplecticSpace(3,q);
em := SelfDuality(w);
quit;

