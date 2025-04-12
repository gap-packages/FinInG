#empty subspaces
pg := PG(5,7);
p := VectorSpaceToElement(pg,[1,0,0,0,0,0]*Z(7)^0);
e := EmptySubspace(pg);
e in e;
p in e;
e in p;
e in pg;
pg in e;
Span(p,e);
Span(e,e);
Meet(e,e);
Meet(e,p);
Meet(p,e);
quit;

