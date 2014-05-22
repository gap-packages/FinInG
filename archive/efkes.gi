psdim := ps!.dimension;
f := ps!.basefield;
b := Unpack(v!.obj); #cmat unpack necessary for infra.
vdim := v!.type;  
if vdim = 1 then b:=[b]; fi;
vs := ps!.vectorspace;
sub := Subspace(vs, b);  

group1 := List(coll1);;
objs := List(Points(quadric));;
List(objs,p->List(group1,g->(p^g)^em = (p^em)^(g^hom)));

group2 := List(coll2);;
objs := List(Points(w));;
List(objs,p->List(group2,g->PreImageElm(em,(p^g)) = (PreImageElm(em,p)^(PreImageElm(hom,g)))));

List(AsList(Points(quadric)),x->x^em);
List(AsList(Lines(quadric)),x->x^em);
List(AsList(Planes(quadric)),x->x^em);
List(AsList(Points(w)),x->PreImageElm(em,x));
List(AsList(Lines(w)),x->PreImageElm(em,x));
List(AsList(Planes(w)),x->PreImageElm(em,x));

#mat: matrix van een element van PGammaL(4,q);
q := 25;
group := CollineationGroup(PG(3,q));
g := Random(group);
mat := Unpack(g!.mat);

newmat := [];
newmat[1] := PluckerCoordinates([mat[2],mat[1]]);
newmat[2] := PluckerCoordinates([mat[3],mat[1]]);
newmat[3] := PluckerCoordinates([mat[4],mat[1]]);
newmat[4] := PluckerCoordinates([mat[3],mat[2]]);
newmat[5] := PluckerCoordinates([mat[4],-mat[2]]);
newmat[6] := -PluckerCoordinates([-mat[4],mat[3]]);

systemmat := ShallowCopy(newmat);
o := One(GF(q));
z := Z(q);
p := Characteristic(GF(q));
vec := [o,-o,-o,o*(p-1)/2,o*(p-1)/2,o*(p-1)];

#image of vec via klein correspondence
ipl := InversePluckerCoordinates(vec);
line := ipl*mat;
pl := PluckerCoordinates(line);

for i in [1..6] do
for j in [1..6] do
systemmat[j][i] := systemmat[j][i]*vec[j];
od;
od;

cs := SolutionMat(systemmat,pl);

twiner := function(g)
local mat,newmat;
mat := Unpack(g!.mat);
newmat := [];
newmat[1] := PluckerCoordinates([mat[2],mat[1]]);
newmat[2] := PluckerCoordinates([mat[3],mat[1]]);
newmat[3] := PluckerCoordinates([mat[4],mat[1]]);
newmat[4] := PluckerCoordinates([mat[3],mat[2]]);
newmat[5] := PluckerCoordinates([mat[4],-mat[2]]);
newmat[6] := -PluckerCoordinates([-mat[4],mat[3]]);
return ProjElWithFrob(newmat,g!.frob,f);
end;

q := 16;
pg := PG(3,q);
group := CollineationGroup(pg);
test := [];
em := KleinCorrespondence(q);
hom := Intertwiner(em);
for i in [1..10000] do
x := Random(group);
line := Random(Lines(pg));
Add(test,(line^x)^em = (line^em)^(x^hom));
od;
Collected(test);


q := 7;
f := GF(q);
em := KleinCorrespondence(q);
ps := AmbientGeometry(Range(em));
group := CollineationGroup(ps);
g := Random(group);
mat := Unpack(g!.mat);

id := IdentityMat(4,f);
lines := [];
lines[1] := [[id[1],id[2]],[id[1],id[3]]];
lines[2] := [[id[2],id[3]],[id[2],id[4]]];
lines[3] := [[id[3],id[4]],[id[3],id[1]]];
lines[4] := [[id[4],id[1]],[id[4],id[2]]];

pts := List(lines,x->[PluckerCoordinates(x[1]),PluckerCoordinates(x[2])]);
ipts := List(pts,x->[x[1],x[2]]*mat);
ilines := List(ipts,x->[InversePluckerCoordinates(x[1]),InversePluckerCoordinates(x[2])]);
ipts := List(ilines, x->SumIntersectionMat(x[1],x[2])[2]);

newmat := List(ipts,x->x[1]);

newmat := [];
newmat[1] := ipts[1][1];
newmat[2] := 2*ipts[2][1];
newmat[3] := -2*ipts[3][1];
newmat[4] := -ipts[4][1];


e := [1,1,1,1]*Z(q)^0;
x := [[e,id[1]],[e,id[2]],[e,id[3]]];
ept := [PluckerCoordinates(x[1]),PluckerCoordinates(x[2]),PluckerCoordinates(x[3])];
iept := List(ept,x->x*mat);
#iept := ShallowCopy(ept);
ielines := List(iept,x->InversePluckerCoordinates(x));
ie := SumIntersectionMat(ielines[1],ielines[2])[2];
ie := SumIntersectionMat(ielines[3],ie)[2];
ie := ie[1];

systemmat := ShallowCopy(newmat);
cs := SolutionMat(systemmat,ie);

e := [1,1,1,1]*Z(q)^0;
ijk := VectorSpaceToElement(PG(3,q),e);
line1 := Random(Lines(ijk));
line2 := Random(Lines(ijk));
pt1 := line1^em;
pt2 := line2^em;
ipt1 := pt1^g;
ipt2 := pt2^g;
iline1 := PreImageElm(em,ipt1);
iline2 := PreImageElm(em,ipt2);
ijk_image := Meet(iline1,iline2);

ie := Unpack(UnderlyingObject(ijk_image));
systemmat := ShallowCopy(newmat);
cs := SolutionMat(systemmat,ie);


pts := List(id,x->VectorSpaceToElement(PG(3,q),x));
combs := Combinations([1..4],3);
hyps := List(combs,x->Span(pts{x}));
ipts := List(pts,x->PreImageElm(em,(x^em)^g));

#shit maat.
q := 7;
f := GF(q);
em := KleinCorrespondenceExtended(f);
ps := AmbientGeometry(Range(em));
group := CollineationGroup(ps);
g := Random(group);

id := IdentityMat(4,GF(q));
basepts := List(id,x->VectorSpaceToElement(PG(3,q),x));
ibasepts := List(basepts,x->PreImageElm(em,(x^em)^g));

mat := Unpack(g!.mat);
lines := [];
lines[1] := [[id[1],id[2]],[id[1],id[3]]];
lines[2] := [[id[2],id[3]],[id[2],id[4]]];
lines[3] := [[id[3],id[4]],[id[3],id[1]]];
lines[4] := [[id[4],id[1]],[id[4],id[2]]];

pts := List(lines,x->[PluckerCoordinates(x[1]),PluckerCoordinates(x[2])]);
ipts := List(pts,x->[x[1],x[2]]*mat);
ilines := List(ipts,x->[InversePluckerCoordinates(x[1]),InversePluckerCoordinates(x[2])]);
ipts := List(ilines, x->SumIntersectionMat(x[1],x[2])[2]);

newmat := List(ipts,x->x[1]);

e := [1,1,1,1]*Z(q)^0;
ijk := VectorSpaceToElement(PG(3,q),e);
line1 := Random(Lines(ijk));
line2 := Random(Lines(ijk));
pt1 := line1^em;
pt2 := line2^em;
ipt1 := pt1^g;
ipt2 := pt2^g;
iline1 := PreImageElm(em,ipt1);
iline2 := PreImageElm(em,ipt2);
ijk_image := Meet(iline1,iline2);

ie := Unpack(UnderlyingObject(ijk_image));
systemmat := ShallowCopy(newmat);
cs := SolutionMat(systemmat,ie);

id := IdentityMat(4,f);

twinerprefun := function( el )
local mat, newmat, id, lines, 
mat := Unpack(el!.mat);
lines := [];
lines[1] := [[id[1],id[2]],[id[1],id[3]],[id[1],id[4]]];
lines[2] := [[id[2],id[3]],[id[2],id[4]]];
lines[3] := [[id[3],id[4]],[id[3],id[1]]];
lines[4] := [[id[4],id[1]],[id[4],id[2]]];
pts := List(lines,x->List(x,y->PluckerCoordinates(y)));
ipts := List(pts,x->x*mat);
ilines := List(ipts,x->List(x,y->InversePluckerCoordinates(y)));
if Rank(Union(ilines[1])) = 3 then
	Error( "<el> is not inducing a collineation of PG(3,q)" );
fi;
ipts := List(ilines, x->SumIntersectionMat(x[1],x[2])[2]);
newmat := List(ipts,x->x[1]);
e := [1,1,1,1]*Z(q)^0;
x := [[e,id[1]],[e,id[2]]];
ept := List(x,y->PluckerCoordinates(y));
iept := List(ept,x->x*mat);
ielines := List(iept,x->InversePluckerCoordinates(x));
ie := SumIntersectionMat(ielines[1],ielines[2])[2];
ie := SumIntersectionMat(ielines[3],ie)[2];
ie := ie[1];
cs := SolutionMat(newmat,ie);
for i in [1..4] do
	newmat[i] := newmat[i]*cs[i];
od;
return ProjElWithFrob(newmat,el!.frob,f);

#####testing ####


q := 5;
em := KleinCorrespondence(GF(q),true);
hom := Intertwiner(em);
ps := AmbientGeometry(Range(em));

group := CollineationGroup(PG(3,q));
gens := GeneratorsOfGroup(group);
lines := Lines(PG(3,q));

g := Random(group);
Collected(List(lines,x->(x^g)^em = (x^em)^(g^hom)));
List(gens,g->Collected(List(lines,x->(x^g)^em = (x^em)^(g^hom))));

Collected(List(pts,x->(x^g)^em = (x^em)^h))));

group := CollineationGroup(ps);
gens := GeneratorsOfGroup(group);
pts := Points(ps);

g := Random(group);
gens := GeneratorsOfGroup(group);
g := gens[2];
PreImageElm(hom,g);

Collected(List(pts,x->PreImageElm(em,x^g)=PreImageElm(em,x)^(PreImageElm(hom,g))));
List(gens{[1,3,4]},g->Collected(List(pts,x->PreImageElm(em,x^g)=PreImageElm(em,x)^(PreImageElm(hom,g)))));

List(gens,g->Collected(List(pts,x->PreImageElm(em,x^g)=PreImageElm(em,x)^(PreImageElm(hom,g)))));


q := 2;
mat := [[0,0,0,0,0,1],[0,0,0,0,1,0],[0,0,0,1,0,0],[0,0,1,0,0,0],[0,1,0,0,0,0],[1,0,0,0,0,0]]*Z(q)^0;
mat := [[0,0,0,1],[0,0,1,0],[0,1,0,0],[1,0,0,0]]*Z(q)^0;
form := BilinearFormByMatrix(mat,GF(q));
w := PolarSpace(form);
mat := [[0,1,0,0,0],[0,0,0,0,0],[0,0,0,1,0],[0,0,0,0,0],[0,0,0,0,1]]*Z(q)^0;
form := QuadraticFormByMatrix(mat,GF(q));
quadric := PolarSpace(form);
em := IsomorphismPolarSpaces(quadric,w,true);
hom := Intertwiner(em);

group := CollineationGroup(quadric);
gens := GeneratorsOfGroup(group);
g := Random(group);
pts := AsList(Lines(quadric));
List(gens,g->Collected(List(pts,x->(x^g)^em = (x^em)^(g^hom))));

group := CollineationGroup(w);
gens := GeneratorsOfGroup(group);
pts := AsList(Lines(w));
List(gens,g->Collected(List(pts,x->PreImageElm(em,x^g)=PreImageElm(em,x)^(PreImageElm(hom,g)))));

twinerfunc := function(g)
local mat,newmat,frob;
mat := Unpack(g!.mat);
frob := g!.frob;
newmat := [];
newmat[2] := PluckerCoordinates([mat[3],mat[1]]){[1..5]};
newmat[3] := PluckerCoordinates([mat[4],mat[1]]){[1..5]};
newmat[4] := -PluckerCoordinates([-mat[3],mat[2]]){[1..5]};
newmat[5] := PluckerCoordinates([mat[4],-mat[2]]){[1..5]};
newmat[1] := PluckerCoordinates([mat[2]+mat[3],mat[1]+mat[4]]){[1..5]} - newmat[2] - newmat[5];
return ProjElWithFrob(newmat,frob,f); #base change is here.
end;




f := GF(5);
one := One(f);
mat := NullMat(5, 5, f);
mat[1][1] := one;
mat[2][5] := -one;
mat[3][4] := -one;
form_quadric := QuadraticFormByMatrix(mat, f);
ps := PolarSpace( form_quadric );

vec1 := [1,1,0,0,1,-1]*Z(5)^0;

vec2 := [0,1,0,0,0,0]*Z(5)^0;
vec3 := [0,0,1,0,0,0]*Z(5)^0;
vec4 := [0,0,0,1,0,0]*Z(5)^0;
vec5 := [0,0,0,0,1,0]*Z(5)^0;

vec6 := [0,1,1,-1,1,0]*Z(5)^0;

iv1 := InversePluckerCoordinates(vec1);
iv2 := InversePluckerCoordinates(vec2);
iv3 := InversePluckerCoordinates(vec3);
iv4 := InversePluckerCoordinates(vec4);
iv5 := InversePluckerCoordinates(vec5);
iv6 := InversePluckerCoordinates(vec6);


p1 := VectorSpaceToElement(ps,vec1{[1..5]});
p2 := VectorSpaceToElement(ps,vec2{[1..5]});
p3 := VectorSpaceToElement(ps,vec3{[1..5]});
p4 := VectorSpaceToElement(ps,vec4{[1..5]});
p5 := VectorSpaceToElement(ps,vec5{[1..5]});


#zelfde probleem als boven.

q := 5;
group := CollineationGroup(PG(3,q));
g := Random(group);
mat := Unpack(g!.mat);

newmat := [];
newmat[1] := PluckerCoordinates([mat[2],mat[1]]);
newmat[2] := PluckerCoordinates([mat[3],mat[1]]);
newmat[3] := PluckerCoordinates([mat[4],mat[1]]);
newmat[4] := PluckerCoordinates([mat[3],mat[2]]);
newmat[5] := PluckerCoordinates([mat[4],-mat[2]]);
newmat[6] := -PluckerCoordinates([-mat[4],mat[3]]);

systemmat := ShallowCopy(newmat);
o := One(GF(q));
z := Z(q);
p := Characteristic(GF(q));
vec := [o,-o,-o,o*(p-1)/2,o*(p-1)/2,o*(p-1)];

#image of vec via klein correspondence
ipl := InversePluckerCoordinates(vec);
line := ipl*mat;
pl := PluckerCoordinates(line);

for i in [1..6] do
for j in [1..6] do
systemmat[j][i] := systemmat[j][i]*vec[j];
od;
od;

cs := SolutionMat(systemmat,pl);

id := IdentityMat(4,f);

        twinerprefun := function( g )
			local mat, newmat, lines, pts, ipts, ilines, e, x, ept,
				ielines, ie, cs, iept, frob;
			frob := g!.frob;
            #mat := cw * Unpack(g!.mat) * cwinv^frob;
			#mat := cq4q * Unpack(g!.mat) * cq4qinv^frob;
            mat := Unpack(g!.mat);
            lines:= [];
            lines[1] := [[id[1],id[3]],[id[1],id[4]]];
            lines[2] := [[id[2],id[3]],[id[2],id[4]]];
            lines[3] := [[id[3],id[1]],[id[3],id[2]]];
            lines[4] := [[id[4],id[1]],[id[4],id[2]]];
            pts := List(lines,x->List(x,y->PluckerCoordinates(y){[1..5]}));
            ipts := List(pts,x->x*mat);
            for i in [1..Length(ipts)] do
                for j in [1..2] do
                    ipts[i][j][6] := -ipts[i][j][1];
                od;
            od;
            ilines := List(ipts,x->List(x,y->InversePluckerCoordinates(y)));
            ipts := List(ilines, x->SumIntersectionMat(x[1],x[2])[2]);
            newmat := List(ipts,x->x[1]);
			e := [1,1,1,1]*one;
			ept := List([[e,id[1]+id[4]],[e,id[2]+id[3]]],y->PluckerCoordinates(y){[1..5]});
			iept := List(ept,x->x*mat);
            iept[1][6] := -iept[1][1];
            iept[2][6] := -iept[2][1];

			ielines := List(iept,x->InversePluckerCoordinates(x));
			ie := SumIntersectionMat(ielines[1],ielines[2])[2];
			cs := SolutionMat(newmat,ie[1]);
			for i in [1..4] do
				newmat[i] := newmat[i]*cs[i];
			od;
			return ProjElWithFrob(newmat,g!.frob,f);
		end;


