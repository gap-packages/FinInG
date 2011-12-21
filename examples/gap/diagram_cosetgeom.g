#Coset geometry
g:=SymmetricGroup(5);
g1:=Stabilizer(g,[1,2],OnSets);
g2:=Stabilizer(g,[1,2,3],OnSets);
cg:=CosetGeometry(g,[g1,g2]);
p:=Random(ElementsOfIncidenceStructure(cg,1));
q:=Random(ElementsOfIncidenceStructure(cg,2));
IsIncident(p,q);
IsIncident(p,p);
ParabolicSubgroups(cg);
Rank(cg) = Size(last);
BorelSubgroup(cg);
AmbientGroup(cg);
quit;
