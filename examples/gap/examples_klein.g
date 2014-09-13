# klein correspondence (elementary)
ps := HyperbolicQuadric(5,5);
klein := KleinCorrespondence(ps);
line1 := Random(Lines(PG(3,5)));
line2 := Random(Lines(PG(3,5)));
p := line1^klein;
q := line2^klein;
p in ps;
q in ps;
IsCollinear(ps,p,q);
Meet(line1,line2);
quit;

