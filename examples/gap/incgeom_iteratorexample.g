# example of the use of an iterator
pg := PG(12,81);
pts := Points(pg);
Size(pts);
ps := ParabolicQuadric(12,81);
First(pts,x->x in ps);
time;
quit;

