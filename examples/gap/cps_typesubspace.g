#morphisms_typesubspace.g
h1 := HermitianPolarSpace(2, 3^2);
h2 := HermitianPolarSpace(3, 3^2);
pg := AmbientSpace( h2 );    
pi := VectorSpaceToElement( pg, [[1,0,0,0],[0,1,0,0],[0,0,1,0]] * Z(9)^0 );
TypeOfSubspace(h2, pi);
pi := VectorSpaceToElement( pg, [[1,0,0,0],[0,1,0,0],[0,0,1,Z(9)]] * Z(9)^0 );
TypeOfSubspace(h2, pi);
quit;
