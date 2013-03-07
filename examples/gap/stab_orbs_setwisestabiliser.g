#setwise stabilizers using finingsetwisestabiliser
ps := PG(3,4);
p := Random(Points(ps));
q := Random(Points(ps));
g := CollineationGroup(ps);
FiningSetwiseStabiliser(g,Set([p,q]));
ps := ParabolicQuadric(6,3);
g := CollineationGroup(ps);
l1 := Random(Lines(ps));
l2 := Random(Lines(ps));
l3 := Random(Lines(ps));
FiningSetwiseStabiliser(g,Set([l1,l2,l3]));
quit;
