#constructing an element of a projective space
ps := ProjectiveSpace(5,9);
line := Random(Lines(ps));
pos := EllipticQuadric(5,9);
line in pos;
quit;

