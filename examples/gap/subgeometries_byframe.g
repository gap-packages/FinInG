#subgeometry by frame
pg := PG(3,3^6);
frame := RandomFrameOfProjectiveSpace(pg);
sub1 := SubgeometryOfProjectiveSpaceByFrame(pg,frame,GF(3));
sub2 := SubgeometryOfProjectiveSpaceByFrame(pg,frame,3^2);
sub3 := SubgeometryOfProjectiveSpaceByFrame(pg,frame,3^3);
sub4 := SubgeometryOfProjectiveSpaceByFrame(pg,frame,3^6);
quit;

