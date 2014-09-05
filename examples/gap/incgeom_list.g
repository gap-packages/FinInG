# list (easy examples)
pg := PG(2,2);
List(Points(pg));
List(Lines(pg));
ps := ParabolicQuadric(6,2);
lines := List(Lines(ps));
time;
Size(lines);
quit;
