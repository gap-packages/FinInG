#rank of a flag
ps := ParabolicQuadric(8,3); 
l := Random(Lines(ps));
plane := Random(Planes(l));
solid := Random(Solids(plane));
flag := FlagOfIncidenceStructure(ps,[l,plane,solid]);
Rank(flag);
quit;
