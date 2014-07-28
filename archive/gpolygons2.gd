DeclareOperation( "GeneralisedPolygonByObjects", [ IsList ] );


DeclareOperation( "GeneralisedPolygonByIncidenceMatrix", [ IsMatrix ] );

DeclareOperation( "GeneralisedPolygonByElements", [ IsSet, IsSet, IsFunction ] );

DeclareOperation( "GeneralisedPolygonByElements", [ IsSet, IsSet, IsFunction, IsGroup ] );

DeclareCategory( "IsShadowElementsOfGeneralisedPolygon", IsElementsOfIncidenceStructure );

DeclareRepresentation( "IsShadowElementsOfGeneralisedPolygonRep", IsElementsOfIncidenceStructure, [ "geometry", "type", "element", "func" ]);




sp := Size(pts);
sl := Size(lines);

    adj := function(x,y)
    if x in [1..sp] and y in [1..sp] then
        return false;
    elif x in [sp+1..sl] and y in [sp+1..sl] then
        return false;
    elif x in [1..sp] then
        return inc(pts[x],lines[y-sp]);
    elif y in [1..sp] then
        return inc(lines[x-sp],pts[y]);
    fi;
    end;

group := Action(group,Union(pts,lines));

graph := Graph(group,[1..sp+sl],OnPoints,adj);