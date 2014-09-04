# incidence with flags
pg := PG(3,5);
p := Random(Points(pg));
l := Random(Lines(p));
plane := Random(Planes(l));
flag := FlagOfIncidenceStructure(pg,[l,plane]);
IsIncident(flag,l);
IsIncident(l,flag);
quit;

