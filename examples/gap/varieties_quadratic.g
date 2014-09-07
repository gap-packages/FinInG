# Example for quadratic varieties
F:=GF(5);
r:=PolynomialRing(F,4);
x:=IndeterminatesOfPolynomialRing(r);
pg:=PG(3,F);
Q:=x[2]*x[3]+x[4]^2;
qv:=QuadraticVariety(pg,Q);
AsSet(List(Planes(pg),z->Size(Filtered(Points(z),x->x in qv))));
qf:=QuadraticForm(qv);
Display(qf);
IsDegenerateForm(qf);
qv:=QuadraticVariety(3,F,"-");
PolarSpace(qv);
Display(last);
qv:=QuadraticVariety(3,F,"+");
Display(last);
quit;
