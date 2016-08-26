#finingorbitsdomain
pg := PG(7,2);
group:=CollineationGroup(pg);
syl127:=SylowSubgroup(group,127);

pt:=Points(pg);
sol:=Solids(pg);
Factors(Size(group));
Factors(Size(pt));

orbsd:=OrbitsDomain(syl127,sol,OnProjSubspaces);;
