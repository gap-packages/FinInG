#Span
pg := PG(3,5);
p := VectorSpaceToElement(pg,[1,2,0,1]*Z(5)^0);
q := VectorSpaceToElement(pg,[0,3,3,2]*Z(5)^0);
line := Span(p,q);
Span(p,p) = p;
r := VectorSpaceToElement(pg,[0,0,0,1]*Z(5)^0);
plane := Span(line,r);
plane = Span([p,q,r]);
t := Span(EmptySubspace(pg),p);
t = p;
Span(pg,p);
quit;
