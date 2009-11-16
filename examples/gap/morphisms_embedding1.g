#morphisms_embedding1.g
geom1 := ProjectiveSpace(2, 3);
geom2 := ProjectiveSpace(3, 3);
planes := Planes(geom2);
hyp := Random(planes);
em := NaturalEmbeddingBySubspace(geom1, geom2, hyp);
points := Points(geom1);
x := Random(points);
x^em;
quit;
