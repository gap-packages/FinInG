#elements of type j of an incidence geometry.
ps := ProjectiveSpace(3,3);
l := ElementsOfIncidenceStructure(ps,2);
ps := EllipticQuadric(5,9);
lines := ElementsOfIncidenceStructure(ps,2);
planes := ElementsOfIncidenceStructure(ps,3);
quit;
as := AffineSpace(3,9);
lines := ElementsOfIncidenceStructure(as,"lines");
quit;
