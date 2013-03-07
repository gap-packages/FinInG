#compare timing of setwise
ps := ParabolicQuadric(4,4);
g := CollineationGroup(ps);
l1 := Random(Lines(ps));
l2 := Random(Lines(ps));
g1 := Stabilizer(g,Set([l1,l2]),OnSets);
time;
g2 := FiningSetwiseStabiliser(g,Set([l1,l2]));
time;
g1=g2;
quit;

