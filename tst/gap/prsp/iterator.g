#Iterator
pg := PG(9,109^2);
planes := Planes(pg);
iter := Iterator(planes);
plane := NextIterator(iter);
Unpack(UnderlyingObject(plane));
plane := NextIterator(iter);
Unpack(UnderlyingObject(plane));
plane := NextIterator(iter);
Unpack(UnderlyingObject(plane));
plane := NextIterator(iter);
Unpack(UnderlyingObject(plane));
quit;
