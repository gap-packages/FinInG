#morphisms by field reduction for projective spaces
pg1 := ProjectiveSpace(2,81);
f2 := GF(9);
em := NaturalEmbeddingByFieldReduction(pg1,f2);
f2 := GF(3);
em := NaturalEmbeddingByFieldReduction(pg1,f2);
pg2 := ProjectiveSpace(11,3);
em := NaturalEmbeddingByFieldReduction(pg1,pg2);
quit;
