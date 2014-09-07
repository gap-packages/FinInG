# points of varieties
F:=GF(9);
r:=PolynomialRing(F,4);
pg:=PG(3,9);
f1:=r.1*r.3-r.2^2;
f2:=r.4*r.1^2-r.4^3;
var:=AlgebraicVariety(pg,[f1,f2]);
points:=Points(var);
Size(points);
iter := Iterator(points);
for i in [1..4] do
	x := NextIterator(iter);
	Display(x);
od;
quit;

