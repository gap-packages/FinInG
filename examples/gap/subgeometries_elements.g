#elements of a projective space
ps := PG(6,27);
frame := RandomFrameOfProjectiveSpace(ps);
sub := SubgeometryOfProjectiveSpaceByFrame(ps,frame,GF(3));
Points(sub);
Lines(sub);
Planes(sub);
Solids(sub);
Hyperplanes(sub);
ElementsOfIncidenceStructure(ps,1);
ElementsOfIncidenceStructure(ps,2);
ElementsOfIncidenceStructure(ps,3);
ElementsOfIncidenceStructure(ps,4);
ElementsOfIncidenceStructure(ps,5);
ElementsOfIncidenceStructure(ps,6);
ElementsOfIncidenceStructure(ps,7);
quit;
quit;
