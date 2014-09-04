# incidence graph of an incidence structure
ps := SymplecticSpace(3,2);
pts := List(Points(ps));;
lines := List(Lines(ps));;
flags := Union(List(pts,x->List(Lines(x),y->FlagOfIncidenceStructure(ps,[x,y]))));;
inc := function(x,y)
if x = y then
    return true;
elif IsFlagOfIncidenceStructure(x) and IsElementOfIncidenceStructure(y) then
    return IsIncident(x,y);
elif IsElementOfIncidenceStructure(x) and IsElementOfIncidenceStructure(y) then
    return false;
elif IsFlagOfIncidenceStructure(x) and IsFlagOfIncidenceStructure(y) then   
    return false;
else 
    return inc(y,x);
fi;
end;
type := function(x)
if IsList(Type(x)) then
    return 2;
else
    return 1;
fi;
end;
els := Union(pts,lines,flags);;
struc := IncidenceStructure(els,inc,type,[1,2]);
gamma := IncidenceGraph(struc);
Diameter(gamma);
Girth(gamma);
quit;
