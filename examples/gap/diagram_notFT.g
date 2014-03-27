#not Flag Transitive geometry example
g:=SymmetricGroup(4);
g1:=Subgroup(g,[(1,2,3)]);
g2:=Subgroup(g,[(1,4)]);
g3:=Subgroup(g,[(1,2,3,4)]);
cg:=CosetGeometry(g,[g1,g2,g3]);
IsFlagTransitiveGeometry(cg);
cg2:=CosetGeometry(g,[g1,g2]);
IsFlagTransitiveGeometry(cg2);
quit;
