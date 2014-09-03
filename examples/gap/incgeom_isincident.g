#testing incidence: to be done
p := Random(Points(PG(5,4)));
l := Random(Lines(p));
IsIncident(p,l);
IsIncident(l,p);
p * l;
l * p;
p * p;
l * l;
quit;

