#elements incident with an element
ps := PG(6,9);
p := VectorSpaceToElement(ps,[1,0,1,0,0,0,0]*Z(9)^0);
els := ElementsIncidentWithElementOfIncidenceStructure(p,3);
line := VectorSpaceToElement(ps,[[1,1,1,1,0,0,0],[0,0,0,0,1,1,1]]*Z(9)^0);
els := ElementsIncidentWithElementOfIncidenceStructure(line,1);
List(els);
quit;
