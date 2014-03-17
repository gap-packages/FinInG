#an example of an incidence structure that is not an incidence geometry
pg := PG(4,2);
pg2 := PG(9,2);
points := List(Lines(pg),x->VectorSpaceToElement(pg2,GrassmannCoordinates(x)));;
flags := Concatenation(List(Points(pg),x->List(Planes(x),y->FlagOfIncidenceStructure(pg,[x,y]))));;
prelines := List(flags,flag->ShadowOfFlag(pg,flag,2));;
lines := List(prelines,x->VectorSpaceToElement(pg2,List(x,y->GrassmannCoordinates(y))));;
flags2 := Concatenation(List(Points(pg),x->List(Solids(x),y->FlagOfIncidenceStructure(pg,[x,y]))));;
preplanes := List(flags2,flag->ShadowOfFlag(pg,flag,2));;
planes := List(preplanes,x->VectorSpaceToElement(pg2,List(x,y->GrassmannCoordinates(y))));;
maximals1 := List(Planes(pg),x->VectorSpaceToElement(pg2,List(Lines(x),y->GrassmannCoordinates(y))));;
maximals2 := List(Points(pg),x->VectorSpaceToElement(pg2,List(Lines(x),y->GrassmannCoordinates(y))));;
elements := Union(points,lines,planes,maximals1,maximals2);;
Length(elements);
type := x -> ProjectiveDimension(x)+1;
inc_rel := \*;
inc := IncidenceStructure(elements,inc_rel,type,[1,2,3,4]);
Rank(inc);
quit;
