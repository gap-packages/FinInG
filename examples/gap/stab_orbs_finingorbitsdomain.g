#finingorbitsdomain
pg := PG(7,2);
group:=CollineationGroup(pg);
syl127:=SylowSubgroup(group,127);
orbits := FiningOrbits(syl127,AsList(Solids(pg)));
time;
orbits := FiningOrbitsDomain(syl127,Solids(pg),OnProjSubspaces);
time;
orbits := OrbitsDomain(syl127,Solids(pg),OnProjSubspaces);
time;

