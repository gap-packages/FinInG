# fining orbit (2)
ps := ParabolicQuadric(4,3);
g := CollineationGroup(ps);
pg := PG(4,3);
s := First(Solids(pg),t -> TypeOfSubspace(ps,t) = "elliptic" );
orbit1 := FiningOrbit(g,s,OnProjSubspaces);
time;
spts := Filtered(Points(s),s->s in ps);
orbit2 := FiningOrbit(g,Set(spts),OnSetsProjSubspaces);
time;
quit;

