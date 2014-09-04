# ambient geometry for flags
gp := TwistedTrialityHexagon(2^3);
p := Random(Points(gp));
l := Random(Lines(p));
flag := FlagOfIncidenceStructure(gp,[p,l]);
AmbientGeometry(flag);
quit;
