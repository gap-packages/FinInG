#morphisms_embedding2.g
h1 := HermitianPolarSpace(2, 3^2);
h2 := HermitianPolarSpace(3, 3^2);
pg := AmbientSpace( h2 );    
pi := VectorSpaceToElement( pg, [[1,0,0,0],[0,1,0,0],[0,0,1,0]] * Z(9)^0 );
em := NaturalEmbeddingBySubspace( h1, h2, pi );
quit;
