# size of flag
ps := SymplecticSpace(5,7);
p := Random(Points(ps));
plane := Random(Planes(p));
flag := FlagOfIncidenceStructure(ps,[p,p,plane]);
Size(flag);
ElementsOfFlag(flag);
quit;
