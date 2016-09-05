# ambient space of a subgeometry
pg := PG(2,5^6);
sub1 := CanonicalSubgeometryOfProjectiveSpace(pg,5);
AmbientSpace(sub1);
sub2 := CanonicalSubgeometryOfProjectiveSpace(pg,5^3);
AmbientSpace(sub2);
frame := RandomFrameOfProjectiveSpace(pg);
sub3 := SubgeometryOfProjectiveSpaceByFrame(pg,frame,5^2);
AmbientSpace(sub3);
quit;

