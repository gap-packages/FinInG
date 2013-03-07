#example using the generic Stabilizer
ps := PG(3,8);
g := CollineationGroup(ps);
p := Random(Points(ps));
Stabilizer(g,p,OnProjSubspaces);
time;
line := Random(Lines(ps));
Stabilizer(g,line,OnProjSubspaces);
time;
quit;
