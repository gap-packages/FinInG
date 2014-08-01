q := 5;
ps := ParabolicQuadric(4,q);
ps := EllipticQuadric(5,q);
pts := Set(AsList(Points(ps)));;
lines := Set(AsList(Lines(ps)));;
inc := \*;
group := CollineationGroup(ps);
gp := GeneralisedPolygonByElements(pts,lines,inc);
gp := GeneralisedPolygonByElements(pts,lines,inc,group, \^);
graph := IncidenceGraphOfGeneralisedPolygon(gp);;
vn := VertexNames(graph);;

p := Random(Points(gp));
test1 := Set(List(Lines(p)));;
test2 := Set(List(vn{Adjacency(graph,Position(vn,Unwrap(p)))},x->Wrap(gp,2,x)));;


#### our best friend T_2(0)
conic := Set(Points(ParabolicQuadric(2,q)));
pg := PG(3,q);
hyp :=  HyperplaneByDualCoordinates(pg,[1,0,0,0]*Z(q)^0);
em := NaturalEmbeddingBySubspace(PG(2,q),pg,hyp);
O := List(conic,x->x^em);
group := CollineationGroup(pg);
stab := FiningSetwiseStabiliser(group,O);
points1 := Set(Filtered(Points(pg),x->not x in hyp));;
phi := PolarityOfProjectiveSpace(ParabolicQuadric(2,q));
tangents := List(conic,x->x^phi);
lines := List(tangents,x->x^em);
planes := List(lines,x->Filtered(Planes(x),y->not y in hyp));
points2 := Union(planes);
points3 := [hyp];
linesa := Union(List(O,x->Filtered(Lines(x),y->not y in hyp)));
linesb := Set(O);
pts := Union(points1,points2,points3);
lns := Union(linesa,linesb);
inc := \*;
gp := GeneralisedPolygonByElements(pts,lns,inc,stab,\^);

###### our best friend again #####
q := 5;
conic := Set(Points(ParabolicQuadric(2,q)));
pg := PG(3,q);
hyp :=  HyperplaneByDualCoordinates(pg,[1,0,0,0]*Z(q)^0);
em := NaturalEmbeddingBySubspace(PG(2,q),pg,hyp);
O := List(conic,x->x^em);
group := CollineationGroup(pg);
stab := FiningSetwiseStabiliser(group,O);
points1 := Set(Filtered(Points(pg),x->not x in hyp));;
tangents := List(conic,x->TangentSpace(x)^em);
planes := List(tangents,x->Filtered(Planes(x),y->not y in hyp));
points2 := Union(planes);
points3 := [hyp];
linesa := Union(List(O,x->Filtered(Lines(x),y->not y in hyp)));
linesb := Set(O);
pts := Union(points1,points2,points3);
lns := Union(linesa,linesb);
inc := \*;
gp := GeneralisedPolygonByElements(pts,lns,inc,stab,\^);




########## old stuff

InstallMethod( GeneralisedPolygonByElements2,
    "for two sets (points and lines), and an incidence function",
    [ IsSet, IsSet, IsFunction, IsGroup, IsFunction ],
    function( pts, lns, inc, group, act )
    local adj, graph, ty, girth, shadpoint, shadline, s, t, gp, permaction, sp, sl;

    sp := Size(pts);
    sl := Size(lns);
    adj := function(x,y)
    if x in [1..sp] and y in [1..sp] then
        return false;
    elif x in [sp+1..sl+sp] and y in [sp+1..sl+sp] then
        return false;
    elif x in [1..sp] then
        return inc(pts[x],lns[y-sp]);
    elif y in [1..sp] then
        return inc(lns[x-sp],pts[y]);
    fi;
    end;

    permaction := Action(group,Union(pts,lns),act);

    graph := Graph(permaction, [1..sp+sl], OnPoints, adj, true );
    girth := Girth(graph);

    if IsBipartite(graph) then
        if not girth = 2*Diameter(graph) then
            Error("<blocks> are not defining a generalised polygon");
        fi;
    else
        Error("elements are not defining a generalised polygon");
    fi;
        
    if girth = 6 then
        ty := NewType( GeometriesFamily, IsProjectivePlane and IsGeneralisedPolygonRep );
    elif girth = 8 then
        ty := NewType( GeometriesFamily, IsGeneralisedQuadrangle and IsGeneralisedPolygonRep );
    elif girth = 12 then
        ty := NewType( GeometriesFamily, IsGeneralisedHexagon and IsGeneralisedPolygonRep );
    elif girth = 16 then
        ty := NewType( GeometriesFamily, IsGeneralisedOctagon and IsGeneralisedPolygonRep );
    else
        Error("<points>, <lines> and <inc> do not define a thick finite generalised polygon");
    fi;

	s := Length(Adjacency(graph,sp+1))-1; # number of points on a line minus 1.
	t := Length(Adjacency(graph,1))-1; # number of linbes on a point minus 1.
    
    shadpoint := function( pt )
        return List(Filtered(lns,x->inc(pt!.obj,x)),y->Wrap(pt!.geo,2,y));
    end;

    shadline := function( line )
        return List(Filtered(pts,x->inc(line!.obj,x)),y->Wrap(line!.geo,1,y));
    end;

    gp := rec( points := pts, lines := lns, incidence := inc, shadowofpoint := shadpoint,
            shadowofline := shadline );

    Objectify( ty, gp );
	SetOrder(gp, [s,t] );
    SetTypesOfElementsOfIncidenceStructure(gp, ["point","line"]);
    Setter( IncidenceGraphOfGeneralisedPolygonAttr )( gp, graph );
    return gp;
end );
