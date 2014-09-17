# generalised octagon as coset geometry and generalised polygon
LoadPackage( "AtlasRep" );
titsgroup:=AtlasGroup("2F4(2)'");
g1:=AtlasSubgroup(titsgroup,3);
g2:=AtlasSubgroup(titsgroup,5);
conj:=ConjugacyClassSubgroups(titsgroup,g1);;
# Now look for the conjugate of g1 with maximal intersection
g1:=First(conj, sg -> Size(Intersection(sg,g2))=2048);
cg:=CosetGeometry(titsgroup,[g1,g2]);;
Rank2Parameters(cg);
pts := Set(ElementsOfIncidenceStructure(cg,1));;
lines := Set(ElementsOfIncidenceStructure(cg,2));;
gp := GeneralisedPolygonByElements(pts,lines,\*,titsgroup,OnCosetGeometryElement);
quit;
