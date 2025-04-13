gap> START_TEST("Forms: gpbyelements.tst");
gap> struc := PG(2,2);
ProjectiveSpace(2, 2)
gap> pts := Set(List(Points(struc)));;
gap> lines := Set(List(Lines(struc)));;
gap> inc := \*;
<Operation "*">
gap> gp := GeneralisedPolygonByElements(lines,pts,inc);
<projective plane order 2>
gap> p := ObjectToElement(gp,2,pts[1]);
<a line in <projective plane order 2>>
gap> q := ObjectToElement(gp,2,pts[2]);
<a line in <projective plane order 2>>
gap> r := Meet(p,q);
<a point in <projective plane order 2>>
gap> t := ObjectToElement(gp,1,Span(pts[1],pts[2]));
<a point in <projective plane order 2>>
gap> t = r;
true
gap> l := ObjectToElement(gp,1,lines[1]);
<a point in <projective plane order 2>>
gap> m := ObjectToElement(gp,1,lines[2]);
<a point in <projective plane order 2>>
gap> s := Span(l,m);
<a line in <projective plane order 2>>
gap> t := ObjectToElement(gp,2,Meet(lines[1],lines[2]));
<a line in <projective plane order 2>>
gap> t = s;
true
gap> List(Points(gp));
[ <a point in <projective plane order 2>>, 
  <a point in <projective plane order 2>>, 
  <a point in <projective plane order 2>>, 
  <a point in <projective plane order 2>>, 
  <a point in <projective plane order 2>>, 
  <a point in <projective plane order 2>>, 
  <a point in <projective plane order 2>> ]
gap> List(Lines(gp));
[ <a line in <projective plane order 2>>, <a line in <projective plane order 
    2>>, <a line in <projective plane order 2>>, 
  <a line in <projective plane order 2>>, <a line in <projective plane order 
    2>>, <a line in <projective plane order 2>>, 
  <a line in <projective plane order 2>> ]
gap> List(Lines(l));
[ <a line in <projective plane order 2>>, <a line in <projective plane order 
    2>>, <a line in <projective plane order 2>> ]
gap> List(Points(p));
[ <a point in <projective plane order 2>>, 
  <a point in <projective plane order 2>>, 
  <a point in <projective plane order 2>> ]
gap> DistanceBetweenElements(p,q);
2
gap> DistanceBetweenElements(m,l);
2
gap> DistanceBetweenElements(p,l);
1
gap> STOP_TEST("gpbyelements.tst", 10000 );
