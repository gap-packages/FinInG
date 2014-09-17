# embedding by field reduction
pg1 := PG(1,3^2);
pg2 := PG(3,3);
em := NaturalEmbeddingByFieldReduction(pg1,pg2);
spread := List(Points(pg1),x->x^em);
Union(List(spread,x->List(Points(x))))=Set(Points(pg2));
ps1 := HermitianPolarSpace(3,3^2);
ps2 := HyperbolicQuadric(7,3);
em := NaturalEmbeddingByFieldReduction(ps1,ps2);
spread := List(Points(ps1),x->x^em);;
Union(List(spread,x->List(Points(x))))=Set(Points(ps2));
quit;
