#categories for subgeometries
sub := CanonicalSubgeometryOfProjectiveSpace(PG(3,25),GF(5));
CategoriesOfObject(sub);
planes := Planes(sub);
CategoriesOfObject(planes);
p := Random(planes);
CategoriesOfObject(p);
quit;
