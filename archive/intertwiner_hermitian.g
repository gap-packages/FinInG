q := 9;
f := GF(q^2);


alpha := First(f, a -> (a^q)^2 <> a^2);
e := alpha^(q-1) + alpha^(1-q);
one := One(GF(q));

mat1 := IdentityMat(6, GF(q));
mat2 := NullMat(6, 6, f);
for i in [1..3] do
mat2[2*i-1][8-2*i] := one;
od;
mat1 := mat1 - e * mat2;
form_quadric := QuadraticFormByMatrix(mat1, GF(q));

c1 := BaseChangeToCanonical( form_quadric );
if not IsCanonicalPolarSpace(q5q) then
c2 := BaseChangeToCanonical(QuadraticForm(q5q));
cq5q := c2^-1 * c1;
else
cq5q := c1;
fi;
cq5qinv := cq5q^-1;

mat1 := IdentityMat(6, f) * alpha;
mat2 := NullMat(6, 6, f);
for i in [1..6] do
mat2[i][7-i] := one;
od;
x := mat1 + mat2 * alpha^q;
xinv := x^-1;


twinerfunc := function(g)
local mat,newmat,frob,newmat2,n,frob2,j,arg;
frob := g!.frob;
mat := Unpack(g!.mat);
newmat := [];
newmat[1] := PluckerCoordinates([mat[2],mat[1]]);
newmat[2] := PluckerCoordinates([mat[3],mat[1]]);
newmat[3] := PluckerCoordinates([mat[4],mat[1]]);
newmat[4] := PluckerCoordinates([mat[3],mat[2]]);
newmat[5] := PluckerCoordinates([mat[4],-mat[2]]);
newmat[6] := -PluckerCoordinates([-mat[4],mat[3]]);
newmat2 :=  xinv*newmat*x^(frob^-1);
n := First(newmat2[1],x->not IsZero(x));
#newmat2 := List(newmat2,x->List(x,y->y/n));
newmat2 := newmat2/n;
if not IsOne(frob) then
j := Log(frob!.power,Characteristic(f));
else 
j := 0;
fi;
frob2 := FrobeniusAutomorphism(GF(q))^(j mod q);
arg := ShallowCopy(cq5q * newmat2 * cq5qinv^frob2);
#return ProjElWithFrob(arg,frob2,GF(q));
return [arg,frob2];
end;


ProjElWithFrob(newmat,g!.frob,f);
ProjElWithFrob(newmat,frob2,GF(q));
ProjElWithFrob(test,frob2,GF(q));
ProjElWithFrob(test,frob2,GF(q));



q := 9;

mat2 := [ [ 0*Z(q), Z(q)^0, 0*Z(q), 0*Z(q), 0*Z(q), 0*Z(q) ],
  [ 0*Z(q), 0*Z(q), 0*Z(q), 0*Z(q), 0*Z(q), 0*Z(q) ], 
  [ 0*Z(q), 0*Z(q), 0*Z(q), Z(q)^0, 0*Z(q), 0*Z(q) ], 
  [ 0*Z(q), 0*Z(q), 0*Z(q), 0*Z(q), 0*Z(q), 0*Z(q) ], 
  [ 0*Z(q), 0*Z(q), 0*Z(q), 0*Z(q), Z(q)^0, 0*Z(q) ], 
  [ 0*Z(q), 0*Z(q), 0*Z(q), 0*Z(q), 0*Z(q), Z(q) ] ];
  
mat := [ [ 0*Z(q), Z(q)^0, 0*Z(q), 0*Z(q) ], [ Z(q)^0, 0*Z(q), 0*Z(q), 0*Z(q) ], 
  [ 0*Z(q), 0*Z(q), 0*Z(q), Z(q)^0 ], [ 0*Z(q), 0*Z(q), Z(q)^0, 0*Z(q) ] ];


hform := HermitianFormByMatrix(mat,GF(q^2));
h := PolarSpace(hform);
qform := QuadraticFormByMatrix(mat2,GF(q));
q5q := PolarSpace(qform);

em := NaturalDualityHermitian(h,q5q,true);
hom := Intertwiner(em);

lines := Lines(h);

group := CollineationGroup(h);
g := Random(group);

Collected(List(lines,x->(x^g)^em = (x^em)^(g^hom)));
List(gens,g->Collected(List(lines,x->(x^g)^em = (x^em)^(g^hom))));


####################################################


q := 9;
f := GF(q^2);

alpha := First(f, a -> (a^q)^2 <> a^2);
e := alpha^(q-1) + alpha^(1-q);
one := One(GF(q));

mat1 := IdentityMat(6, GF(q));
mat2 := NullMat(6, 6, f);
for i in [1..3] do
mat2[2*i-1][8-2*i] := one;
od;
mat1 := mat1 - e * mat2;
form_quadric := QuadraticFormByMatrix(mat1, GF(q));

c1 := BaseChangeToCanonical( form_quadric );
if not IsCanonicalPolarSpace(q5q) then
c2 := BaseChangeToCanonical(QuadraticForm(q5q));
cq5q := c2^-1 * c1;
else
cq5q := c1;
fi;
cq5qinv := cq5q^-1;

id := IdentityMat(4,f);

mat1 := IdentityMat(6, f) * alpha;
mat2 := NullMat(6, 6, f);
for i in [1..6] do
mat2[i][7-i] := one;
od;
x := mat1 + mat2 * alpha^q;
xinv := x^-1;


		twinerprefun := function( g )
			local mat, newmat, lines, pts, ipts, ilines, e, ept,
				ielines, ie, cs, iept, frob, frob2, j;
			frob := g!.frob;
			mat := x * (cq5qinv * Unpack(g!.mat) * cq5q^(frob^-1) ) * xinv^(frob^-1);      #base change is here.
			mat := x * (cq5q * Unpack(g!.mat) * cq5qinv^(frob^-1) ) * xinv^(frob^-1);      #base change is here.
			#mat := x * Unpack(g!.mat) * xinv^(frob^-1);      #base change is here.
            if not IsOne(frob) then
                j := Log(frob!.power,Characteristic(f));
            else
                j := 0;
            fi;
            frob2 := FrobeniusAutomorphism(GF(q^2))^j;
			lines:= [];
			lines[1] := [[id[1],id[2]],[id[1],id[3]]];
			lines[2] := [[id[2],id[3]],[id[2],id[4]]];
			lines[3] := [[id[3],id[4]],[id[3],id[1]]];
			lines[4] := [[id[4],id[1]],[id[4],id[2]]];
			pts := List(lines,x->List(x,y->PluckerCoordinates(y)));
			ipts := List(pts,x->(x*mat)^frob2);
			ilines := List(ipts,x->List(x,y->InversePluckerCoordinates(y)));
			ipts := List(ilines, x->SumIntersectionMat(x[1],x[2])[2]);
			newmat := List(ipts,x->x[1]);
			e := [1,1,1,1]*one;
			ept := List([[e,id[1]],[e,id[2]]],y->PluckerCoordinates(y));
			iept := List(ept,x->(x*mat)^frob2);
			ielines := List(iept,x->InversePluckerCoordinates(x));
			ie := SumIntersectionMat(ielines[1],ielines[2])[2];
			cs := SolutionMat(newmat,ie[1]);
			for i in [1..4] do
				newmat[i] := newmat[i]*cs[i];
			od;
			return ProjElWithFrob(newmat,frob2,f);
		end;


test_em := function(g)
local frob, frob2, j, mat;
frob := g!.frob;
mat := x * (cq5qinv * Unpack(g!.mat) * cq5q^(frob^-1) ) * xinv^(frob^-1);      #base change is here.
if not IsOne(frob) then
j := Log(frob!.power,Characteristic(f));
else
j := 0;
fi;
frob2 := FrobeniusAutomorphism(GF(q^2))^j;
return ProjElWithFrob(mat,frob2,f);
end;

q := 4;
q5q := EllipticQuadric(5,q);
herm := HermitianPolarSpace(3,q^2);
em := NaturalDualityHermitian(q5q,herm,true);

group := CollineationGroup(q5q);
gens := GeneratorsOfGroup(group);
pts := Points(q5q);
hom := Intertwiner(em);

Collected(List(pts,x->PreImageElm(em,x^g)=PreImageElm(em,x)^(PreImageElm(hom,g))));
List(gens,g->Collected(List(pts,x->PreImageElm(em,x^g)=PreImageElm(em,x)^(PreImageElm(hom,g)))));

Collected(List(pts,x->(x^g)^em = (x^em)^(h)));


