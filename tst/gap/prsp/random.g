#Random (take care not to generate too much output ;-)
ps := PG(9,27);
Random(Lines(ps));
Random(Points(ps));
Random(Solids(ps));
Random(Hyperplanes(ps));
elts := ElementsOfIncidenceStructure(ps,6);
Random(elts);
RandomSubspace(ps,3);
RandomSubspace(ps,7);
quit;

