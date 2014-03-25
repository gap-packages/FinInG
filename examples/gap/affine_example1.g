#simple example to get started
ag := CollineationGroup(AG(3,27));
pg := CollineationGroup(PG(3,27));
g := Random(ag);
g in pg;
IsSubgroup(pg,ag);
quit;
