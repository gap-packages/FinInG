#checks if an object in IsProjGrpEtc is a not linear
g := Random(CollineationGroup(PG(4,7)));
IsCorrelationCollineation(g);
IsCorrelation(g);
g := Random(CorrelationCollineationGroup(PG(4,7)));
IsCorrelationCollineation(g);
IsCorrelation(g);
quit;
