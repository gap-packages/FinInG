# subfield embeddings
pgsub := PG(2,7);
pg := PG(2,7^2);
em := NaturalEmbeddingBySubfield(pgsub,pg);
baer := List(Points(pgsub),x->x^em);;
numbers := Collected(List(Lines(pg),x->Number(baer,y->y in x)));
mat := [[0,0,0,1],[0,0,-1,0],[0,1,0,0],[-1,0,0,0]]*Z(5)^0;
form := BilinearFormByMatrix(mat,GF(5));
symplecticspace := PolarSpace(form);
hermitianspace := HermitianPolarSpace(3,25);
em := NaturalEmbeddingBySubfield(symplecticspace,hermitianspace);
l := Random(Lines(symplecticspace));
l^em;
quit;

