# Extend elements of subgeometry to an element of the ambient proj. space.
pg := PG(3,5^5);
frame := RandomFrameOfProjectiveSpace(pg);
sub := SubgeometryOfProjectiveSpaceByFrame(pg,frame,5);
p := Random(Points(sub));
l := Random(Lines(p));
p * l;
q := ExtendElementOfSubgeometry(p);
q * l;
quit;
m := ExtendElementOfSubgeometry(l);
q * m;
UnderlyingObject(q) = UnderlyingObject(p);
UnderlyingObject(l) = UnderlyingObject(m);
quit;
