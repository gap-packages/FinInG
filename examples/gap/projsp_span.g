#span of elements of polar spaces
ps := ProjectiveSpace(3,3);
p := Random(Planes(ps));
q := Random(Planes(ps));
s := Span(p,q);
s = Span([p,q]);
t := Span(EmptySubspace(ps),p);
t = p;
Span(ps,p);
quit;
