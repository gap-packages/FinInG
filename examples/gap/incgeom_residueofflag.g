#easy example of residues of a flag.
pg := PG(4,5);
p := Random(Points(pg));
l := Random(Lines(p));
flag := FlagOfIncidenceStructure(pg,[p,l]);
res := ResidueOfFlag(flag);
gamma := IncidenceGraph(res);;
Diameter(gamma);
Girth(gamma);
quit;

