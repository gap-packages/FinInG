#morphisms_subfield1.g
pg1 := ProjectiveSpace(2, 3);
pg2 := ProjectiveSpace(2, 9);
em := NaturalEmbeddingBySubfield(pg1,pg2);
points := AsList(Points( pg1 ));
image := ImagesSet(em, points);
quit;
