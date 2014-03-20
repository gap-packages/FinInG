#Span for cps's
ps := HyperbolicQuadric(5,2);
p := Random(Planes(ps));
q := Random(Planes(ps));
s := Span(p,q);
s = Span([p,q]);
t := Span(EmptySubspace(ps),p);
t = p;
quit;

