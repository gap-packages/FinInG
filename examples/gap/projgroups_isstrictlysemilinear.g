#checks if an object in IsProjGrpEtc is a not linear
g := Random(HomographyGroup(PG(3,25)));
IsStrictlySemilinear(g);
g := Random(CollineationGroup(PG(3,25)));
IsStrictlySemilinear(g);
g := Random(CorrelationCollineationGroup(PG(3,25)));
IsStrictlySemilinear(g);
quit;
