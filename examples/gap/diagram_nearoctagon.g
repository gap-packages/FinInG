#A nice near octagon
LoadPackage("atlasrep");
j2:=AtlasGroup("J2"); #Uses AtlasRep package
max3:=AtlasSubgroup(j2,3); #member of 3rd ATLAS class of max. subgps
max4:=AtlasSubgroup(j2,4); #member of 4th ATLAS class of max. subgps
conj3:=ConjugacyClassSubgroups(j2,max3);;
g1:=First(conj3, c -> Size(Intersection(c,max4))=384);;
g2:=max4;;
cg:=CosetGeometry(j2,[g1,g2]);;
Rank2Parameters(cg);
quit;
