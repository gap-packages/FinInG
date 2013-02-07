#morphisms_fieldreduc2.g
h := HermitianPolarSpace(2, 5^2);
quadric := EllipticQuadric(5, 5);
em := NaturalEmbeddingByFieldReduction(h, quadric);
points := AsList(Points(h));;
image := ImagesSet(em, points);;
image[1];
hom := Intertwiner( em );;
g := Range( hom );
OrbitLengths(g, image);
quit;
