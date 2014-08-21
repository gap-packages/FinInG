#fining orbit (1)
ps := ParabolicQuadric(6,3);
g := CollineationGroup(ps);
pg := PG(6,3);
s := First(Solids(pg),t -> TypeOfSubspace(ps,t) = "elliptic" );
orbit := FiningOrbit(g,s,OnProjSubspaces);
time;
quit;
