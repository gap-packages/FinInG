#autiso example
g:=SymmetricGroup(4); g1:=Subgroup(g,[(1,2,3)]);
g2:=Subgroup(g,[(1,4)]); g3:=Subgroup(g,[(1,2,3,4)]);
cg:=CosetGeometry(g,[g1,g2,g3]);
IsFlagTransitiveGeometry(cg);
aut:=AutGroupIncidenceStructureWithNauty(cg);
Size(aut);
Size(g);
newg1:=Stabilizer(aut, 1);
newg2:=Stabilizer(aut, NrElementsOfIncidenceStructure(cg,1) + 1);
newg3:=Stabilizer(aut, NrElementsOfIncidenceStructure(cg,1) +
			NrElementsOfIncidenceStructure(cg,2) + 1);
newcg:=CosetGeometry(aut, [newg1, newg2, newg3]);
IsFlagTransitiveGeometry(newcg);
IsIsomorphicIncidenceStructureWithNauty(cg, newcg);
quit;
