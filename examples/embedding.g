## Embedding of PG(2,3) in PG(3,3)

geom1 := ProjectiveSpace(2, 3);
geom2 := ProjectiveSpace(3, 3);
planes := Planes(geom2);
hyp := Random(planes);
em := NaturalEmbeddingBySubspace(geom1, geom2, hyp);
points := Points( geom1 );
x := Random(points);
x^em;
points2 := ImagesSet(em, AsSet(points));
ForAll(points2, x -> x in hyp);

geom1 := SymplecticSpace(3, 3);
geom2 := SymplecticSpace(5, 3);
pg := AmbientSpace( geom2 );
solids := Iterator( ElementsOfIncidenceStructure(pg, 4) );
perp := Polarity( geom2 );
solid := NextIterator( solids );
em := NaturalEmbeddingBySubspace(geom1, geom2, solid);
points := Points(geom1);
x := Random(points);
x^em;
points2 := ImagesSet(em, AsSet(points));;
ForAll(points2, x -> x in solid);