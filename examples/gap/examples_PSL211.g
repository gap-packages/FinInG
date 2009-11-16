# example: examples_PSL211.g
g := PSL(2,11);
g1 := Group([ (1,2,3)(4,8,12)(5,10,9)(6,11,7), (1,2)(3,4)(5,12)(6,11)(7,10)(8,9) ]);
g2 := Group([ (1,2,7)(3,9,4)(5,11,10)(6,8,12), (1,2)(3,4)(5,12)(6,11)(7,10)(8,9) ]);
g3 := Group([ (1,2,11)(3,8,7)(4,9,5)(6,10,12), (1,2)(3,12)(4,11)(5,10)(6,9)(7,8) ]);
g4 := Group([ (1,2,11)(3,8,7)(4,9,5)(6,10,12), (1,2)(3,10)(4,9)(5,8)(6,7)(11,12) ]);
cg := CosetGeometry(g, [g1,g2,g3,g4]);
SetName(cg, "Gamma");
ParabolicSubgroups(cg);
BorelSubgroup(cg);
AmbientGroup(cg);
type2 := ElementsOfIncidenceStructure( cg, 2 );
IsFlagTransitiveGeometry( cg );
DrawDiagram( DiagramOfGeometry(cg), "PSL211");
quit;
