#finingorbitsdomain
pg := PG(7,2);
group:=CollineationGroup(pg);
syl127:=SylowSubgroup(group,127);
orbits := FiningOrbits(syl127,AsList(Solids(pg)));;
time;
Collected(List(orbits,x->Length(x)));
orbits := FiningOrbitsDomain(syl127,Solids(pg),OnProjSubspaces);;
time;
Collected(List(orbits,x->Length(x)));
orbits := OrbitsDomain(syl127,Solids(pg),OnProjSubspaces);;
time;
Collected(List(orbits,x->Length(x)));
ag := AG(4,5);
h := Random(CollineationGroup(ag));
group := Group(h);
orbits := FiningOrbitsDomain(group,Points(ag),OnAffineSubspaces);
quit;
