gap> START_TEST("Forms: shadowofelement.tst");
gap> ps := PG(4,3);
ProjectiveSpace(4, 3)
gap> plane := VectorSpaceToElement(ps,[[1,1,0,0,0],[1,0,1,0,0],[0,0,0,0,1]]*Z(3)^0);
<a plane in ProjectiveSpace(4, 3)>
gap> shadowpoints := ShadowOfElement(ps,plane,1);
<shadow points in ProjectiveSpace(4, 3)>
gap> List(shadowpoints);
[ <a point in ProjectiveSpace(4, 3)>, <a point in ProjectiveSpace(4, 3)>, 
  <a point in ProjectiveSpace(4, 3)>, <a point in ProjectiveSpace(4, 3)>, 
  <a point in ProjectiveSpace(4, 3)>, <a point in ProjectiveSpace(4, 3)>, 
  <a point in ProjectiveSpace(4, 3)>, <a point in ProjectiveSpace(4, 3)>, 
  <a point in ProjectiveSpace(4, 3)>, <a point in ProjectiveSpace(4, 3)>, 
  <a point in ProjectiveSpace(4, 3)>, <a point in ProjectiveSpace(4, 3)>, 
  <a point in ProjectiveSpace(4, 3)> ]
gap> shadowlines := ShadowOfElement(ps,plane,2);
<shadow lines in ProjectiveSpace(4, 3)>
gap> List(shadowlines);
[ <a line in ProjectiveSpace(4, 3)>, <a line in ProjectiveSpace(4, 3)>, 
  <a line in ProjectiveSpace(4, 3)>, <a line in ProjectiveSpace(4, 3)>, 
  <a line in ProjectiveSpace(4, 3)>, <a line in ProjectiveSpace(4, 3)>, 
  <a line in ProjectiveSpace(4, 3)>, <a line in ProjectiveSpace(4, 3)>, 
  <a line in ProjectiveSpace(4, 3)>, <a line in ProjectiveSpace(4, 3)>, 
  <a line in ProjectiveSpace(4, 3)>, <a line in ProjectiveSpace(4, 3)>, 
  <a line in ProjectiveSpace(4, 3)> ]
gap> STOP_TEST("shadowofelement.tst", 10000 );
