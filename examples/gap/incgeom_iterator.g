#an iterator. a simple example
ps := PG(3,7);
planes := Planes(ps);
iter := Iterator(planes);
NextIterator(iter);
IsDoneIterator(iter);
quit;
