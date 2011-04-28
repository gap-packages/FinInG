#Coset geometry
g:=SymmetricGroup(5);
g1:=Stabilizer(g,[1,2],OnSets);
g2:=Stabilizer(g,[1,2,3],OnSets);
cg:=CosetGeometry(g,[g1,g2]);
quit;
