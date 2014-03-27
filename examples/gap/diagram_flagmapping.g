# flag mapping example
L:=SimpleLieAlgebra("D",8,Rationals);
rs:=RootSystem(L);
w:=WeylGroup(rs);
gens:=GeneratorsOfGroup(w);;
pabs:=List(gens, g -> Group(Difference(gens, [g])));
g:=Group(gens);
cg:=CosetGeometry(g,pabs);;
cham:=RandomChamber(cg);; # Time of last command: 23945 ms
FlagToStandardFlag(cg,cham); # Time of last command: 1720 ms
cham^last = StandardFlagOfCosetGeometry(cg); # Time of last command:1005 ms
quit;

