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

####### more testing #######

q5q := EllipticQuadric(5,q);
form := QuadraticForm(q5q);
qmat := TransposedMat(GramMatrix(form));
nqmat := TransposedMat(qmat{[3,4,5,6,1,2]});
nqmat := nqmat{[3,4,5,6,1,2]};
qform := QuadraticFormByMatrix(nqmat,GF(q));
gq1 := PolarSpace(qform);
gq2 := q5q;
em := IsomorphismPolarSpaces(gq1,gq2,true);

em := IsomorphismPolarSpacesNC(gq1,gq2,true);

hom := Intertwiner(em);

group1 := CollineationGroup(gq1);
gens := GeneratorsOfGroup(group1);
pts := AsList(Points(gq1));
List(gens,g->Collected(List(pts,x->(x^g)^em = (x^em)^(g^hom))));

group2 := CollineationGroup(gq2);
gens := GeneratorsOfGroup(group2);
pts := AsList(Points(gq2));
List(gens,g->Collected(List(pts,x->PreImageElm(em,x^g)=PreImageElm(em,x)^(PreImageElm(hom,g)))));


mat := [[0,1,0,0],[1,0,0,0],[0,0,0,Z(q)],[0,0,Z(q),0]]*Z(q)^0;
hform := HermitianFormByMatrix(mat,GF(q^2));
gq2 := PolarSpace(hform);
gq1 := HermitianPolarSpace(3,q^2);

em := IsomorphismPolarSpaces(gq1,gq2,true);

em := IsomorphismPolarSpacesNC(gq1,gq2,true);

hom := Intertwiner(em);

group1 := CollineationGroup(gq1);
gens := GeneratorsOfGroup(group1);
pts := AsList(Points(gq1));
List(gens,g->Collected(List(pts,x->(x^g)^em = (x^em)^(g^hom))));

group2 := CollineationGroup(gq2);
gens := GeneratorsOfGroup(group2);
pts := AsList(Points(gq2));
List(gens,g->Collected(List(pts,x->PreImageElm(em,x^g)=PreImageElm(em,x)^(PreImageElm(hom,g)))));

###### naturalembedding by field reduction test ###########

q := 4;
q3q := HyperbolicQuadric(3,q^2);
q7q := HyperbolicQuadric(7,q);
em := NaturalEmbeddingByFieldReduction(q3q,q7q);

###### naturalembedding by field reduction projective case test ###########

n := 2;
q := 4;
h := 3;

basis := Basis(AsVectorSpace(GF(q),GF(q^h)));

pg1 := PG(n,q^h);
em := NaturalEmbeddingByFieldReduction(pg1,GF(q));
hom := Intertwiner(em);
group1 := HomographyGroup(pg1);
gens := GeneratorsOfGroup(group1);
pts := AsList(Points(pg1));;
List(gens,g->Collected(List(pts,x->(x^g)^em = (x^em)^(g^hom))));

gens2 := List(gens,x->x^hom);
els2 := List(pts,x->x^em);;
group2 := Group(gens2);
List(gens2,g->Collected(List(els2,x->PreImageElm(em,x^g)=PreImageElm(em,x)^(PreImageElm(hom,g)))));

List(gens,x->ShrinkMat(basis,BlownUpMat(basis,Unpack(x!.mat)))=Unpack(x!.mat));

## BlownUpMat - ShrinkMat tests.

n := 1;
q := 3;
h := 3;

basis := Basis(AsVectorSpace(GF(q),GF(q^h)));

group1 := HomographyGroup(PG(1,q^h));
mats := List(group1,x->x!.mat);;

bmats := List(mats,x->BlownUpMat(basis,x));;

###### naturalembedding by subfield projective case test ###########

n := 3;
q := 5;
h := 3;

pg1 := PG(n,q);
pg2 := PG(n,q^h);

em := NaturalEmbeddingBySubfield(pg1,pg2);
hom := Intertwiner(em);
group1 := HomographyGroup(pg1);
gens := GeneratorsOfGroup(group1);
els := AsList(Points(pg1));;
els := AsList(Lines(pg1));;
List(gens,g->Collected(List(els,x->(x^g)^em = (x^em)^(g^hom))));

gens2 := List(gens,x->x^hom);
els := List(els,x->x^em);
group2 := Group(gens2);
List(gens2,g->Collected(List(els,x->PreImageElm(em,x^g)=PreImageElm(em,x)^(PreImageElm(hom,g)))));

###### naturalembedding by field reduction polar spaces ###########

q := 3;
n := 2;
t := 2;
f := GF(q);
ps := SymplecticSpace(2*n-1,q^t);
alpha := Z(q^t);
em := NaturalEmbeddingByFieldReduction(ps,GF(q));

els := Points(ps);
elsem := List(els,x->x^em);
List(elsem,x->PreImageElm(em,x));
els := Lines(ps);
elsem := List(els,x->x^em);
List(elsem,x->PreImageElm(em,x));


