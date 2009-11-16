#elements of type j of an incidence geometry.
ps := ProjectiveSpace(3,3);
l := ElementsOfIncidenceStructure(ps,2);
Size(l);
Random(l);
Display(last);
ps := EllipticQuadric(5,9);
lines := ElementsOfIncidenceStructure(ps,2);
planes := ElementsOfIncidenceStructure(ps,3);
quit;
quit;
