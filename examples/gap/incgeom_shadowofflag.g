#shadow of flag
ps := ProjectiveSpace(3,3);
pi := Random(Planes(ps));
x := Random( ShadowOfElement(ps, pi, 1) );
IsIncident(x,pi);
lines := ShadowOfElement(ps,pi,2);
Size(lines);
quit;

