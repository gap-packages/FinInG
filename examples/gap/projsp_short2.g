#short names for shodowelements of an incidence structure
ps := PG(6,13);
plane := Random(Planes(ps));
Points(plane);
Lines(plane);
Solids(plane);
Hyperplanes(plane);
ElementsIncidentWithElementOfIncidenceStructure(plane,6);
quit;
