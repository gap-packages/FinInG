#finingstabiliserorb
ps := PG(5,4);
g := SpecialHomographyGroup(ps);
p := Random(Points(ps));
FiningStabiliserOrb(g,p);
line := Random(Lines(ps));
FiningStabiliserOrb(g,line);
plane := Random(Planes(ps));
FiningStabiliserOrb(g,plane);
ps := HyperbolicQuadric(5,5);
g := IsometryGroup(ps);
p := Random(Points(ps));
FiningStabiliserOrb(g,p);
line := Random(Lines(ps));
FiningStabiliserOrb(g,line);
plane := Random(Planes(ps));
FiningStabiliserOrb(g,plane);
h := SplitCayleyHexagon(3);
g := CollineationGroup(h);
p := Random(Points(h));
FiningStabiliserOrb(g,p);
line := Random(Lines(h));
FiningStabiliserOrb(g,line);
quit;

