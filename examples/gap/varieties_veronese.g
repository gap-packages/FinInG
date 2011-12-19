#example for Veronese varieties
pg:=PG(2,5);
vv:=VeroneseVariety(pg);
Size(Points(vv))=Size(Points(pg));
vm:=VeroneseMap(vv);
r:=PolynomialRing(GF(5),3);
f:=r.1^2-r.2*r.3;
c:=AlgebraicVariety(pg,r,[f]);
pts:=List(Points(c));
Dimension(Span(ImagesSet(vm,pts)));
quit;

