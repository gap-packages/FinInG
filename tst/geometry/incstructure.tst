gap> START_TEST("Forms: incstructure.tst");
gap> ps := SymplecticSpace(3,2);
W(3, 2)
gap> pts := List(Points(ps));;
gap> lines := List(Lines(ps));;
gap> flags := Union(List(pts,x->List(Lines(x),y->FlagOfIncidenceStructure(ps,[x,y]))));;
gap> inc := function(x,y)
> if x = y then
>     return true;
> elif IsFlagOfIncidenceStructure(x) and IsElementOfIncidenceStructure(y) then
>     return IsIncident(x,y);
> elif IsElementOfIncidenceStructure(x) and IsElementOfIncidenceStructure(y) then
>     return false;
> elif IsFlagOfIncidenceStructure(x) and IsFlagOfIncidenceStructure(y) then   
>     return false;
> else 
>     return inc(y,x);
> fi;
> end;
function( x, y ) ... end
gap> type := function(x)
> if IsList(Type(x)) then
>     return 2;
> else
>     return 1;
> fi;
> end;
function( x ) ... end
gap> els := Union(pts,lines,flags);;
gap> struc := IncidenceStructure(els,inc,type,[1,2]);
Incidence structure of rank 2
gap> gamma := IncidenceGraph(struc);;
gap> type1s := ElementsOfIncidenceStructure(struc,1);
<Elements of type 1 of Incidence structure of rank 2>
gap> type2s := ElementsOfIncidenceStructure(struc,2);
<Elements of type 2 of Incidence structure of rank 2>
gap> Iterator(type2s);
<iterator>
gap> ElementsOfIncidenceStructure(ps,"points");
<points of W(3, 2)>
gap> ElementsOfIncidenceStructure(ps,"lines");
<lines of W(3, 2)>
gap> NrElementsOfIncidenceStructure(struc,2);
45
gap> NrElementsOfIncidenceStructure(ps,"points");
15
gap> p := pts[1];
<a point in W(3, 2)>
gap> pt := Wrap(struc,1,p);
<a element of type 1 in Incidence structure of rank 2>
gap> Type(pt);
1
gap> AmbientGeometry(pt);
Incidence structure of rank 2
gap> AmbientGeometry(type1s);
Incidence structure of rank 2
gap> Type(type2s);
2
gap> UnderlyingObject(pt);
<a point in W(3, 2)>
gap> ObjectToElement(struc,1,p);
<a element of type 1 in Incidence structure of rank 2>
gap> shad := ShadowOfElement(ps,p,"lines");
<shadow lines in W(3, 2)>
gap> STOP_TEST("incstructure.tst", 10000 );
