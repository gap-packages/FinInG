#morphisms_subfield2.g
w := SymplecticSpace(5, 3);
h := HermitianVariety(5, 3^2);
em := NaturalEmbeddingBySubfield(w, h);
points := AsList(Points(w));;
image := ImagesSet(em, points);;
ForAll(image, x -> x in h);
quit;
