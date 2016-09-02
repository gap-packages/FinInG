# A random frame of a subgeometry
pg := PG(1,5^5);
frame := RandomFrameOfProjectiveSpace(pg);
Length(frame);
pg := PG(6,2^2);
frame := RandomFrameOfProjectiveSpace(pg);
Length(frame);
quit;

