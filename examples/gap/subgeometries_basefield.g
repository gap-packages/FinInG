# base field of a subgeometry
pg := PG(2,7^2);
frame := RandomFrameOfProjectiveSpace(pg);
sub := SubgeometryOfProjectiveSpaceByFrame(pg,frame,GF(7));
BaseField(sub);
quit;

