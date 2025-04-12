gap> START_TEST("Forms: incstrucflag.tst");
gap> pg := PG(4,2);
ProjectiveSpace(4, 2)
gap> pg2 := PG(9,2);
ProjectiveSpace(9, 2)
gap> points := List(Lines(pg),x->VectorSpaceToElement(pg2,GrassmannCoordinates(x)));;
gap> flags := Concatenation(List(Points(pg),x->List(Planes(x),y->FlagOfIncidenceStructure(pg,[x,y]))));;
gap> prelines := List(flags,flag->ShadowOfFlag(pg,flag,2));;
gap> lines := List(prelines,x->VectorSpaceToElement(pg2,List(x,y->GrassmannCoordinates(y))));;
gap> flags2 := Concatenation(List(Points(pg),x->List(Solids(x),y->FlagOfIncidenceStructure(pg,[x,y]))));;
gap> preplanes := List(flags2,flag->ShadowOfFlag(pg,flag,2));;
gap> planes := List(preplanes,x->VectorSpaceToElement(pg2,List(x,y->GrassmannCoordinates(y))));;
gap> maximals1 := List(Planes(pg),x->VectorSpaceToElement(pg2,List(Lines(x),y->GrassmannCoordinates(y))));;
gap> maximals2 := List(Points(pg),x->VectorSpaceToElement(pg2,List(Lines(x),y->GrassmannCoordinates(y))));;
gap> elements := Union(points,lines,planes,maximals1,maximals2);;
gap> Length(elements);
1891
gap> type := x -> ProjectiveDimension(x)+1;
function( x ) ... end
gap> inc_rel := \*;
<Operation "*">
gap> inc := IncidenceStructure(elements,inc_rel,type,[1,2,3,4]);
Incidence structure of rank 4
gap> Rank(inc);
4
gap> pt := ObjectToElement(inc,1,points[1]);
<a element of type 1 in Incidence structure of rank 4>
gap> line := ObjectToElement(inc,2,lines[2]);
<a element of type 2 in Incidence structure of rank 4>
gap> IsIncident(pt,line);
true
gap> flag1 := FlagOfIncidenceStructure(inc,[]);
<a flag of Incidence structure of rank 4>
gap> flag2 := FlagOfIncidenceStructure(inc,[pt,line]);
<a flag of Incidence structure of rank 4>
gap> AmbientGeometry(flag2);
Incidence structure of rank 4
gap> Size(flag2);
2
gap> Rank(flag2);
2
gap> ResidueOfFlag(flag2);
Incidence structure of rank 2
gap> flag1 = flag2;
false
gap> flag1 < flag2;
true
gap> pt < flag1;
true
gap> IsIncident(pt,flag1);
true
gap> IsIncident(flag1,line);
true
gap> pt in flag1;
false
gap> line in flag2;
true
gap> shad := ShadowOfElement(inc,pt,3);
<shadow elements of type 3 in Incidence structure of rank 4>
gap> shad2 := ShadowOfFlag(inc,flag2,3);
<shadow elements of type 3 in Incidence structure of rank 4>
gap> shad3 := ShadowOfFlag(inc,[pt,line],4);
<shadow elements of type 4 in Incidence structure of rank 4>
gap> Iterator(shad3);
<iterator>
gap> IsConfiguration(inc);
true
gap> IsConstellation(inc);
true
gap> STOP_TEST("incstrucflag.tst", 10000 );
