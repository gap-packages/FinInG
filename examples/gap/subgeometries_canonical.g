#canonical subgeometry of a projective space.
pg := PG(2,25);
sub := CanonicalSubgeometryOfProjectiveSpace(pg,GF(5));
CategoriesOfObject(sub);
pg := PG(3,3^6);
sub := CanonicalSubgeometryOfProjectiveSpace(pg,3^2);
sub := CanonicalSubgeometryOfProjectiveSpace(pg,3^3);
sub := CanonicalSubgeometryOfProjectiveSpace(pg,3^6);
quit;
