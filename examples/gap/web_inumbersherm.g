# intersection numbers of a hermitian curve in PG(2,q^2)
h := HermitianVariety(2,49);
pg := AmbientSpace(h);
lines := Lines(pg);
curve := AsList(Points(h));;
Size(curve);
Collected(List(lines,t -> Number(curve,c -> c in t)));
quit;
