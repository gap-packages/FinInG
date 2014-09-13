# underlying object
pg := PG(3,169);
p := Random(Points(pg));
UnderlyingObject(p);
Unpack(last);
l := Random(Lines(pg));
UnderlyingObject(l);
Unpack(last);
quadric := EllipticQuadric(5,2);
line := Random(Lines(quadric));
UnderlyingObject(line);
Unpack(last);
ag := AG(4,3);
plane := Random(Planes(ag));
UnderlyingObject(plane);
quit;

