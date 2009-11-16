#morphisms_fieldreduc1.g
pg1 := ProjectiveSpace(2,9);
pg2 := ProjectiveSpace(5,3);
em := NaturalEmbeddingByFieldReduction(pg1, pg2);
line := Random( Lines(pg1) );
solid := line ^ em;
l := em!.prefun(solid);
quit;
