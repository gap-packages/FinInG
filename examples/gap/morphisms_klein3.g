#Klein correspondence extended
ps := HyperbolicQuadric(5,7);
em := KleinCorrespondenceExtended(ps);
hom := Intertwiner(em);
mat := [[0,0,0,0,0,1],[0,0,0,0,1,0],[0,0,0,1,0,0],
	[0,0,1,0,0,0],[0,1,0,0,0,0],[1,0,0,0,0,0]]*Z(7)^0;
g := Projectivity(mat,GF(7));
g in CollineationGroup(ps);
PreImageElm(hom,g);
quit;
