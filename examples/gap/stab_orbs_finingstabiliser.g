#finingstabiliser
ps := PG(5,4);
g := SpecialHomographyGroup(ps);
p := Random(Points(ps));
FiningStabiliser(g,p);
line := Random(Lines(ps));
FiningStabiliser(g,line);
plane := Random(Planes(ps));
FiningStabiliser(g,plane);
ps := HyperbolicQuadric(5,5);
g := IsometryGroup(ps);
p := Random(Points(ps));
FiningStabiliser(g,p);
line := Random(Lines(ps));
FiningStabiliser(g,line);
plane := Random(Planes(ps));
FiningStabiliser(g,plane);
h := SplitCayleyHexagon(3);
g := CollineationGroup(h);
p := Random(Points(h));
FiningStabiliser(g,p);
line := Random(Lines(h));
FiningStabiliser(g,line);
quit;

