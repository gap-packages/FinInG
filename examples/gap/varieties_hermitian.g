# Example for hermitian varieties
F:=GF(25);
r:=PolynomialRing(F,3);
x:=IndeterminatesOfPolynomialRing(r);
pg:=PG(2,F);
f:=x[1]^6+x[2]^6+x[3]^6;
hv:=HermitianVariety(pg,f);
AsSet(List(Lines(pg),l->Size(Filtered(Points(l),x->x in hv))));
hv:=HermitianVariety(5,4);
hps:=PolarSpace(hv);
hf:=SesquilinearForm(hv);
PolynomialOfForm(hf);
quit;
