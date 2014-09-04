# elements of a flag
gp := SplitCayleyHexagon(4);
p := Random(Points(gp));
l := Random(Lines(p));
flag := FlagOfIncidenceStructure(gp,[l,p]);
ElementsOfFlag(flag);
quit;

