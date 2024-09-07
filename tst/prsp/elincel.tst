gap> START_TEST("Forms: elincel.tst");
gap> ps := PG(6,9);
ProjectiveSpace(6, 9)
gap> p := VectorSpaceToElement(ps,[1,0,1,0,0,0,0]*Z(9)^0);
<a point in ProjectiveSpace(6, 9)>
gap> els := ElementsIncidentWithElementOfIncidenceStructure(p,3);
<shadow planes in ProjectiveSpace(6, 9)>
gap> line := VectorSpaceToElement(ps,[[1,1,1,1,0,0,0],[0,0,0,0,1,1,1]]*Z(9)^0);
<a line in ProjectiveSpace(6, 9)>
gap> els := ElementsIncidentWithElementOfIncidenceStructure(line,1);
<shadow points in ProjectiveSpace(6, 9)>
gap> List(els);
[ <a point in ProjectiveSpace(6, 9)>, <a point in ProjectiveSpace(6, 9)>, 
  <a point in ProjectiveSpace(6, 9)>, <a point in ProjectiveSpace(6, 9)>, 
  <a point in ProjectiveSpace(6, 9)>, <a point in ProjectiveSpace(6, 9)>, 
  <a point in ProjectiveSpace(6, 9)>, <a point in ProjectiveSpace(6, 9)>, 
  <a point in ProjectiveSpace(6, 9)>, <a point in ProjectiveSpace(6, 9)> ]
gap> STOP_TEST("elincel.tst", 10000 );
