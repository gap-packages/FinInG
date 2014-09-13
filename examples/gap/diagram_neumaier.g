g:=AlternatingGroup(8);
pabs:= [
  Group([ (2,4,6), (1,3,2)(4,8)(6,7) ]), 
  Group([ (1,6,7,8,4), (2,5)(3,4) ]),
  Group([ (3,6)(7,8), (2,4,5), (1,5)(2,4), (2,4)(6,7), (6,8,7), 
(1,2)(4,5), (3,7)(6,8) ]),
  Group([ (1,7,8,4)(2,5,3,6), (1,3)(2,6)(4,8)(5,7), (1,5)(2,4)(3,7)(6,8),
      (1,8)(2,7)(3,4)(5,6), (1,3)(2,6)(4,7)(5,8) ]) ];
cg:=CosetGeometry(g,pabs);
diag:=DiagramOfGeometry(cg);
DrawDiagram(diag, "neuma8");
#Exec("gv neuma8.ps");
point:=Random(ElementsOfIncidenceStructure(cg,1));
residue:=ResidueOfFlag(FlagOfIncidenceStructure(cg,[point]));
diagc3:=DiagramOfGeometry(residue);
DrawDiagram(diagc3, "a7geo");
#Exec("gv a7geo.ps");
quit;
