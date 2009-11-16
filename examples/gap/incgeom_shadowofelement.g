#shadow of element
ps := ProjectiveSpace(3,3);
pi := Random(Planes(ps));
lines := ShadowOfElement(ps,pi,2);
Size(lines);
quit;

