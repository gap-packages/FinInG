#setwise stabilizers using generic stabilizer
ps := PG(3,4);
p := Random(Points(ps));
q := Random(Points(ps));
g := CollineationGroup(ps);
Stabilizer(g,Set([p,q]),OnSets);
time;
quit;
