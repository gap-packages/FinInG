# underlying algebraic structures
pg := PG(3,3^6);
sub1 := CanonicalSubgeometryOfProjectiveSpace(pg,3^3);
BaseField(sub1);
UnderlyingVectorSpace(sub1);
SubfieldOfSubgeometry(sub1);
sub2 := CanonicalSubgeometryOfProjectiveSpace(pg,3^2);
BaseField(sub2);
UnderlyingVectorSpace(sub2);
SubfieldOfSubgeometry(sub2);
quit;

