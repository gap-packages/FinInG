#hyperplanes of an element
pg := PG(3,7);
hyp := Random(Hyperplanes(pg));
h1 := Random(Hyperplanes(hyp));
h2 := Random(Hyperplanes(h1));
ps := SymplecticSpace(7,3);
solid := Random(Solids(ps));
plane := Random(Hyperplanes(solid));
quit;

