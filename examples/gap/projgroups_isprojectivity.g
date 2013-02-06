#checks if an object in IsProjGrpEtc is a projectivity
g := Random(HomographyGroup(PG(3,4)));
IsProjectivity(g);
g := Random(CollineationGroup(PG(3,4)));
IsProjectivity(g);
g := Random(CorrelationCollineationGroup(PG(3,4)));
IsProjectivity(g);
quit;
