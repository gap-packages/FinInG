#GP by elements.
struc := PG(2,2);
pts := Set(List(Points(struc)));;
lines := Set(List(Lines(struc)));;
inc := \*;
gp := GeneralisedPolygonByElements(lines,pts,inc);
p := ObjectToElement(gp,2,pts[1]);
q := ObjectToElement(gp,2,pts[2]);
r := Meet(p,q);
t := ObjectToElement(gp,1,Span(pts[1],pts[2]));
t = r;
l := ObjectToElement(gp,1,lines[1]);
m := ObjectToElement(gp,1,lines[2]);
s := Span(l,m);
t := ObjectToElement(gp,2,Meet(lines[1],lines[2]));
t = s;
List(Points(gp));
List(Lines(gp));
List(Lines(l));
List(Points(p));
quit;

