gap> START_TEST("Forms: gpbyelements2.tst");
gap> struc := PG(2,3);
ProjectiveSpace(2, 3)
gap> pts := Set(List(Points(struc)));;
gap> lines := Set(List(Lines(struc)));;
gap> inc := \*;
<Operation "*">
gap> group := CollineationGroup(struc);
The FinInG collineation group PGL(3,3)
gap> act := OnProjSubspaces;
function( var, el ) ... end
gap> gp := GeneralisedPolygonByElements(lines,pts,inc,group,act);
<projective plane order 3>
gap> p := ObjectToElement(gp,2,pts[1]);
<a line in <projective plane order 3>>
gap> q := ObjectToElement(gp,2,pts[2]);
<a line in <projective plane order 3>>
gap> r := Meet(p,q);
<a point in <projective plane order 3>>
gap> t := ObjectToElement(gp,1,Span(pts[1],pts[2]));
<a point in <projective plane order 3>>
gap> t = r;
true
gap> l := ObjectToElement(gp,1,lines[1]);
<a point in <projective plane order 3>>
gap> m := ObjectToElement(gp,1,lines[2]);
<a point in <projective plane order 3>>
gap> s := Span(l,m);
<a line in <projective plane order 3>>
gap> t := ObjectToElement(gp,2,Meet(lines[1],lines[2]));
<a line in <projective plane order 3>>
gap> t = s;
true
gap> List(Points(gp));
[ <a point in <projective plane order 3>>, 
  <a point in <projective plane order 3>>, 
  <a point in <projective plane order 3>>, 
  <a point in <projective plane order 3>>, 
  <a point in <projective plane order 3>>, 
  <a point in <projective plane order 3>>, 
  <a point in <projective plane order 3>>, 
  <a point in <projective plane order 3>>, 
  <a point in <projective plane order 3>>, 
  <a point in <projective plane order 3>>, 
  <a point in <projective plane order 3>>, 
  <a point in <projective plane order 3>>, 
  <a point in <projective plane order 3>> ]
gap> List(Lines(gp));
[ <a line in <projective plane order 3>>, <a line in <projective plane order 
    3>>, <a line in <projective plane order 3>>, 
  <a line in <projective plane order 3>>, <a line in <projective plane order 
    3>>, <a line in <projective plane order 3>>, 
  <a line in <projective plane order 3>>, <a line in <projective plane order 
    3>>, <a line in <projective plane order 3>>, 
  <a line in <projective plane order 3>>, <a line in <projective plane order 
    3>>, <a line in <projective plane order 3>>, 
  <a line in <projective plane order 3>> ]
gap> List(Lines(l));
[ <a line in <projective plane order 3>>, <a line in <projective plane order 
    3>>, <a line in <projective plane order 3>>, 
  <a line in <projective plane order 3>> ]
gap> List(Points(p));
[ <a point in <projective plane order 3>>, 
  <a point in <projective plane order 3>>, 
  <a point in <projective plane order 3>>, 
  <a point in <projective plane order 3>> ]
gap> DistanceBetweenElements(p,q);
2
gap> DistanceBetweenElements(m,l);
2
gap> DistanceBetweenElements(p,l);
1
gap> STOP_TEST("gpbyelements2.tst", 10000 );
