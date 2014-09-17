# Philippe's awesome E6 example
L := SimpleLieAlgebra("E",6,Rationals);
rs := RootSystem(L);
w := WeylGroup(rs);
gens := GeneratorsOfGroup(w);;
pabs := List(gens, g -> Group(Difference(gens, [g])));
g := Group(gens);
cg := CosetGeometry(g,pabs);;
diag := DiagramOfGeometry( cg );;
DrawDiagram(diag, "E6");
#Exec("open E6.ps");
quit;

