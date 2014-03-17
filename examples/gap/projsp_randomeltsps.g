#random element of (a given) type of a projective space
ps := PG(9,49);
Random(Lines(ps));
Random(Points(ps));
Random(Solids(ps));
Random(Hyperplanes(ps));
elts := ElementsOfIncidenceStructure(ps,6);
Random(elts);
Display(last);
RandomSubspace(ps,3);
Display(last);
RandomSubspace(ps,7);
Display(last);
RandomSubspace(ps);
RandomSubspace(ps);
quit;
