#compare timing
ps := PG(3,8);
g := CollineationGroup(ps);
p := Random(Points(ps));
g1 := Stabilizer(g,p);
time;
g2 := FiningStabiliser(g,p);
time;
g3 := FiningStabiliserOrb(g,p);
time;
g1=g2;
g2=g3;
quit;

