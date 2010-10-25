ps := ProjectiveSpace(3,3);
pi := Random(Planes(ps));
tau := Random(Planes(ps));
Span(pi,tau);
ps := PG(5,4);
u := Random(Points(ps));
v := Random(Points(ps));
Span(u,v);
quit;
