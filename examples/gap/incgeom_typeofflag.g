# types of a flag
pg := PG(8,9);
l := Random(Lines(pg));
s := Random(Solids(l));
flag := FlagOfIncidenceStructure(pg,[l,s]);
Type(flag);
p := Random(Points(pg));
flag := FlagOfIncidenceStructure(pg,[p]);
Type(flag);
quit;

