
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

mat1 := IdentityMat(6, f) * alpha;
mat2 := NullMat(6, 6, f);
for i in [1..6] do
mat2[i][7-i] := one;
od;
x := mat1 + mat2 * alpha^q;
xinv := x^-1;


twinerfunc := function(g)
local mat,newmat,frob,newmat2,n;
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
newmat2 := List(newmat2,x->List(x,y->y/n));

return newmat2;
end;


ProjElWithFrob(newmat,g!.frob,f);
ProjElWithFrob(newmat,frob2,GF(q));
ProjElWithFrob(test,frob2,GF(q));


