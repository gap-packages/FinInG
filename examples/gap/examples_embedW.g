# examples_embedW.g
w3 := SymplecticSpace(3, 3);
w5 := SymplecticSpace(5, 3);
pg := AmbientSpace( w5 );
solids := ElementsOfIncidenceStructure(pg, 4);
iter := Iterator( solids );
perp := PolarityOfProjectiveSpace( w5 );
solid := NextIterator( iter );
solid^perp;
em := NaturalEmbeddingBySubspace( w3, w5, solid );
points := Points( w3 );
points2 := ImagesSet(em, AsSet(points));;
ForAll(points2, x -> x in solid);
quit;
