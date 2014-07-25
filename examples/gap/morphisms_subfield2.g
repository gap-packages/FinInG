#morphisms_subfield2.g
w := SymplecticSpace(5, 3);
h := HermitianPolarSpace(5, 3^2);
em := NaturalEmbeddingBySubfield(w, h);
points := AsList(Points(w));;
image := ImagesSet(em, points);;
ForAll(image, x -> x in h);
hq:=HyperbolicQuadric(3,4);
eq:=EllipticQuadric(3,2);
em:=NaturalEmbeddingBySubfield(eq,hq);
eqpts:=ImagesSet(em,AsList(Points(eq)));
quit;
