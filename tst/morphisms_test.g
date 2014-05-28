####### symplectic case########

q := 8;

gq1 := SymplecticSpace(3,q);
gq2 := ParabolicQuadric(4,q);

em := NaturalDuality(gq1,gq2,false);
em := NaturalDuality(gq1,gq2,true);
em := NaturalDuality(gq1,gq2);
hom := Intertwiner(em);

group1 := CollineationGroup(gq1);
gens := GeneratorsOfGroup(group1);
pts := AsList(Points(gq1));
List(gens,g->Collected(List(pts,x->(x^g)^em = (x^em)^(g^hom))));

group2 := CollineationGroup(gq2);
gens := GeneratorsOfGroup(group2);
pts := AsList(Points(gq2));
List(gens,g->Collected(List(pts,x->PreImageElm(em,x^g)=PreImageElm(em,x)^(PreImageElm(hom,g)))));

#switch role of gq1 and gq2.

gq2 := SymplecticSpace(3,q);
gq1 := ParabolicQuadric(4,q);

em := NaturalDuality(gq1,gq2,false);
em := NaturalDuality(gq1,gq2,true);
hom := Intertwiner(em);

group1 := CollineationGroup(gq1);
gens := GeneratorsOfGroup(group1);
pts := AsList(Points(gq1));
List(gens,g->Collected(List(pts,x->(x^g)^em = (x^em)^(g^hom))));

group2 := CollineationGroup(gq2);
gens := GeneratorsOfGroup(group2);
pts := AsList(Points(gq2));
List(gens,g->Collected(List(pts,x->PreImageElm(em,x^g)=PreImageElm(em,x)^(PreImageElm(hom,g)))));

#use one argument

gq1 := SymplecticSpace(3,q);
em := NaturalDuality(gq1);
gq2 := AmbientGeometry(Range(em));

hom := Intertwiner(em);

group1 := CollineationGroup(gq1);
gens := GeneratorsOfGroup(group1);
pts := AsList(Points(gq1));
List(gens,g->Collected(List(pts,x->(x^g)^em = (x^em)^(g^hom))));

group2 := CollineationGroup(gq2);
gens := GeneratorsOfGroup(group2);
pts := AsList(Points(gq2));
List(gens,g->Collected(List(pts,x->PreImageElm(em,x^g)=PreImageElm(em,x)^(PreImageElm(hom,g)))));

# non canonical polar spaces.

q4q := ParabolicQuadric(4,q);
form := QuadraticForm(q4q);
qmat := TransposedMat(GramMatrix(form));
nqmat := TransposedMat(qmat{[2,3,4,5,1]});
nqmat := nqmat{[2,3,4,5,1]};
qform := QuadraticFormByMatrix(nqmat,GF(q));
gq1 := PolarSpace(qform);

mat := [[0,1,0,0],[-1,0,0,0],[0,0,0,-1],[0,0,1,0]]*Z(q)^0;
wform := BilinearFormByMatrix(mat,GF(q));
gq2 := PolarSpace(wform);

em := NaturalDuality(gq1,gq2,false);
em := NaturalDuality(gq1,gq2,true);
em := NaturalDuality(gq1,gq2);
hom := Intertwiner(em);

group1 := CollineationGroup(gq1);
gens := GeneratorsOfGroup(group1);
pts := AsList(Points(gq1));
List(gens,g->Collected(List(pts,x->(x^g)^em = (x^em)^(g^hom))));

group2 := CollineationGroup(gq2);
gens := GeneratorsOfGroup(group2);
pts := AsList(Points(gq2));
List(gens,g->Collected(List(pts,x->PreImageElm(em,x^g)=PreImageElm(em,x)^(PreImageElm(hom,g)))));

#switch role of gq1 and gq2.

gq1 := PolarSpace(wform);
gq2 := PolarSpace(qform);
em := NaturalDuality(gq1,gq2);
hom := Intertwiner(em);

group1 := CollineationGroup(gq1);
gens := GeneratorsOfGroup(group1);
pts := AsList(Points(gq1));
List(gens,g->Collected(List(pts,x->(x^g)^em = (x^em)^(g^hom))));

group2 := CollineationGroup(gq2);
gens := GeneratorsOfGroup(group2);
pts := AsList(Points(gq2));
List(gens,g->Collected(List(pts,x->PreImageElm(em,x^g)=PreImageElm(em,x)^(PreImageElm(hom,g)))));

#use one argument (1) 
gq1 := PolarSpace(qform);
em := NaturalDuality(gq1);
gq2 := AmbientGeometry(Range(em));

hom := Intertwiner(em);

group1 := CollineationGroup(gq1);
gens := GeneratorsOfGroup(group1);
pts := AsList(Points(gq1));
List(gens,g->Collected(List(pts,x->(x^g)^em = (x^em)^(g^hom))));

group2 := CollineationGroup(gq2);
gens := GeneratorsOfGroup(group2);
pts := AsList(Points(gq2));
List(gens,g->Collected(List(pts,x->PreImageElm(em,x^g)=PreImageElm(em,x)^(PreImageElm(hom,g)))));

#use one argument (2) 
gq1 := PolarSpace(wform);
em := NaturalDuality(gq1);
gq2 := AmbientGeometry(Range(em));

hom := Intertwiner(em);

group1 := CollineationGroup(gq1);
gens := GeneratorsOfGroup(group1);
pts := AsList(Points(gq1));
List(gens,g->Collected(List(pts,x->(x^g)^em = (x^em)^(g^hom))));

group2 := CollineationGroup(gq2);
gens := GeneratorsOfGroup(group2);
pts := AsList(Points(gq2));
List(gens,g->Collected(List(pts,x->PreImageElm(em,x^g)=PreImageElm(em,x)^(PreImageElm(hom,g)))));


####### hermitian case########

q := 4;

gq1 := HermitianPolarSpace(3,q^2);
gq2 := EllipticQuadric(5,q);

em := NaturalDuality(gq1,gq2,false);
em := NaturalDuality(gq1,gq2,true);
em := NaturalDuality(gq1,gq2);
hom := Intertwiner(em);

group1 := CollineationGroup(gq1);
gens := GeneratorsOfGroup(group1);
pts := AsList(Lines(gq1));
List(gens,g->Collected(List(pts,x->(x^g)^em = (x^em)^(g^hom))));

group2 := CollineationGroup(gq2);
gens := GeneratorsOfGroup(group2);
pts := AsList(Points(gq2));
List(gens,g->Collected(List(pts,x->PreImageElm(em,x^g)=PreImageElm(em,x)^(PreImageElm(hom,g)))));

#switch role of gq1 and gq2.

gq1 := EllipticQuadric(5,q);
gq2 := HermitianPolarSpace(3,q^2);

em := NaturalDuality(gq1,gq2,false);
em := NaturalDuality(gq1,gq2,true);
hom := Intertwiner(em);

group1 := CollineationGroup(gq1);
gens := GeneratorsOfGroup(group1);
pts := AsList(Points(gq1));
List(gens,g->Collected(List(pts,x->(x^g)^em = (x^em)^(g^hom))));

group2 := CollineationGroup(gq2);
gens := GeneratorsOfGroup(group2);
pts := AsList(Lines(gq2));
List(gens,g->Collected(List(pts,x->PreImageElm(em,x^g)=PreImageElm(em,x)^(PreImageElm(hom,g)))));

#use one argument

gq1 := HermitianPolarSpace(3,q^2);
em := NaturalDuality(gq1);
gq2 := AmbientGeometry(Range(em));

hom := Intertwiner(em);

group1 := CollineationGroup(gq1);
gens := GeneratorsOfGroup(group1);
pts := AsList(Lines(gq1));
List(gens,g->Collected(List(pts,x->(x^g)^em = (x^em)^(g^hom))));

group2 := CollineationGroup(gq2);
gens := GeneratorsOfGroup(group2);
pts := AsList(Points(gq2));
List(gens,g->Collected(List(pts,x->PreImageElm(em,x^g)=PreImageElm(em,x)^(PreImageElm(hom,g)))));

# non canonical polar spaces.

q := 4;

q5q := EllipticQuadric(5,q);
form := QuadraticForm(q5q);
qmat := TransposedMat(GramMatrix(form));
nqmat := TransposedMat(qmat{[3,4,5,6,1,2]});
nqmat := nqmat{[3,4,5,6,1,2]};
qform := QuadraticFormByMatrix(nqmat,GF(q));
gq1 := PolarSpace(qform);

mat := [[0,1,0,0],[1,0,0,0],[0,0,0,Z(q)],[0,0,Z(q),0]]*Z(q)^0;
hform := HermitianFormByMatrix(mat,GF(q^2));
gq2 := PolarSpace(hform);

em := NaturalDuality(gq1,gq2,false);
em := NaturalDuality(gq1,gq2,true);
CollineationGroup(gq1);
em := NaturalDuality(gq1,gq2);
hom := Intertwiner(em);

group1 := CollineationGroup(gq1);
gens := GeneratorsOfGroup(group1);
pts := AsList(Points(gq1));
List(gens,g->Collected(List(pts,x->(x^g)^em = (x^em)^(g^hom))));

group2 := CollineationGroup(gq2);
gens := GeneratorsOfGroup(group2);
pts := AsList(Lines(gq2));
List(gens,g->Collected(List(pts,x->PreImageElm(em,x^g)=PreImageElm(em,x)^(PreImageElm(hom,g)))));

#switch role of gq1 and gq2.

gq1 := PolarSpace(hform);
gq2 := PolarSpace(qform);
CollineationGroup(gq1);
em := NaturalDuality(gq1,gq2);
hom := Intertwiner(em);

group1 := CollineationGroup(gq1);
gens := GeneratorsOfGroup(group1);
pts := AsList(Lines(gq1));
List(gens,g->Collected(List(pts,x->(x^g)^em = (x^em)^(g^hom))));

group2 := CollineationGroup(gq2);
gens := GeneratorsOfGroup(group2);
pts := AsList(Points(gq2));
List(gens,g->Collected(List(pts,x->PreImageElm(em,x^g)=PreImageElm(em,x)^(PreImageElm(hom,g)))));

#use one argument (1) 
gq1 := PolarSpace(qform);
CollineationGroup(gq1);
em := NaturalDuality(gq1);
gq2 := AmbientGeometry(Range(em));

hom := Intertwiner(em);

group1 := CollineationGroup(gq1);
gens := GeneratorsOfGroup(group1);
pts := AsList(Points(gq1));
List(gens,g->Collected(List(pts,x->(x^g)^em = (x^em)^(g^hom))));

group2 := CollineationGroup(gq2);
gens := GeneratorsOfGroup(group2);
pts := AsList(Lines(gq2));
List(gens,g->Collected(List(pts,x->PreImageElm(em,x^g)=PreImageElm(em,x)^(PreImageElm(hom,g)))));

#use one argument (2) 
gq1 := PolarSpace(hform);
CollineationGroup(gq1);
em := NaturalDuality(gq1);
gq2 := AmbientGeometry(Range(em));

hom := Intertwiner(em);

group1 := CollineationGroup(gq1);
gens := GeneratorsOfGroup(group1);
pts := AsList(Lines(gq1));
List(gens,g->Collected(List(pts,x->(x^g)^em = (x^em)^(g^hom))));

group2 := CollineationGroup(gq2);
gens := GeneratorsOfGroup(group2);
pts := AsList(Points(gq2));
List(gens,g->Collected(List(pts,x->PreImageElm(em,x^g)=PreImageElm(em,x)^(PreImageElm(hom,g)))));



