#chamber of a projective space.
ps := PG(3,13);
plane := Random(Planes(ps));
line := Random(Lines(plane));
point := Random(Points(line));
flag := FlagOfIncidenceStructure(ps,[point,line,plane]);
IsChamberOfIncidenceStructure(flag);
quit;
