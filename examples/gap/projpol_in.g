#constructing an element of a projective space
ps := ProjectiveSpace(5,9);
p := Random(Points(ps));
r := Random(Solids(ps));
IsIncident(p,r);
IsIncident(r,p);
p*r;
r*p;
p in r;
r in p;
EmptySubspace(ps) in r;
r in ps;
quit;

