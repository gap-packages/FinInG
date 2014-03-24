# Example for quadratic varieties
F:=GF(5);
r:=PolynomialRing(F,4);
x:=IndeterminatesOfPolynomialRing(r);
pg:=PG(3,F);
f:=x[2]*x[3]+x[4]^2;
qv:=QuadraticVariety(pg,f);
AsSet(List(Planes(pg),z->Size(Filtered(Points(z),x->x in qv))));
qf:=QuadraticForm(qv);
Display(qf);
IsDegenerateForm(qf);
PolarSpace(qv);
quit;
quit;
