#to blow up a subspace of a projective space
pg := PG(3,5^2);
basis := Basis(AsVectorSpace(GF(5),GF(5^2)));
line := Random(Lines(pg));
solid1 := BlownUpSubspaceOfProjectiveSpace(basis,line);
BasisVectors(basis);
basis := Basis(AsVectorSpace(GF(5),GF(5^2)),[Z(5),Z(5^2)^8]);
solid2 := BlownUpSubspaceOfProjectiveSpace(basis,line);
solid1 = solid2;
quit;

