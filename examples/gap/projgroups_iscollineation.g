#checks if an object in IsProjGrpEtc is a collineation
g := Random(HomographyGroup(PG(2,27)));
IsCollineation(g);
g := Random(CollineationGroup(PG(2,27)));
IsCollineation(g);
g := Random(CorrelationCollineationGroup(PG(2,27)));
IsCollineation(g);
quit;
