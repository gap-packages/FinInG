# intersection numbers of a hermitian curve in PG(2,q^2)
w3 := SymplecticSpace(3,3);
w5 := SymplecticSpace(5,3);
pg := AmbientSpace(w5);
iter := Iterator(ElementsOfIncidenceStructure(pg,4));
perp := PolarityOfProjectiveSpace(w5);
solid := NextIterator(iter);
em := NaturalEmbeddingBySubspace(w3,w5,solid);
points := Points(w3);
points2 := ImagesSet(em,AsSet(points));;
ForAll(points2,x -> x in solid);
quit;
