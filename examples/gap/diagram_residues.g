#residues
pg:=SymplecticSpace(5,2);
pi:=Random(Planes(pg));
l:=Random(Lines(pi));
p:=Random(Points(l));
g:=CollineationGroup(pg);
g1:=Stabilizer(g,p);
g2:=Stabilizer(g,l);
g3:=Stabilizer(g,pi);
cg:=CosetGeometry(g, [g1,g2,g3]);
RandomFlag(cg); # Time of last command: 10745 ms
Type(last);
ResidueOfFlag(last2);
Rank(last);
NrElementsOfIncidenceStructure(last2,1);
flg:=RandomFlag(cg);;
can:=CanonicalResidueOfFlag(cg,flg);
Type(flg);
Rank(can);
res:=ResidueOfFlag(flg);
IsIsomorphicIncidenceStructureWithNauty(res,can);
quit;
