#defining frame
pg := PG(2,2^4);
sub := CanonicalSubgeometryOfProjectiveSpace(pg,2);
frame := DefiningFrameOfSubgeometry(sub);
List(frame,x->Unpack(UnderlyingObject(x)));
frame := RandomFrameOfProjectiveSpace(pg);
sub := SubgeometryOfProjectiveSpaceByFrame(pg,frame,2^2);
def := DefiningFrameOfSubgeometry(sub);
List(def,x->Unpack(UnderlyingObject(x)));
StandardFrame(sub);
quit;
