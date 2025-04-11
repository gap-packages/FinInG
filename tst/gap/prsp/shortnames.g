#Short names for ElementsIncidentWithElementOfIncidenceStructure
ps := PG(6,13);
plane := VectorSpaceToElement(ps,[[1,2,3,4,5,6,7],[8,9,10,11,12,0,1],
[2,3,4,0,6,7,8]]*Z(13)^0);
Points(plane);
Lines(plane);
Solids(plane);
Hyperplanes(plane);
ElementsIncidentWithElementOfIncidenceStructure(plane,6);
quit;

