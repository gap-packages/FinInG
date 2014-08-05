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




InstallMethod( GeneralisedPolygonByBlocks,
    "for a homogeneous list",
    [ IsHomogeneousList ],
    function( blocks )
        local pts, gp, ty, i, graph, sz, adj, girth, shadpoint, shadline, s, t, dist, vn, 
		listels, objs;
        pts := Union(blocks);
        s := Size(blocks[1]) - 1;
        if not ForAll(blocks, b -> Size(b) = s + 1 ) then
            Error("Not every block has size ", s + 1);
        fi;
        
        i := function( x, y )
        if IsSet( x!.obj ) and not IsSet( y!.obj ) then
            return y!.obj in x!.obj;
        elif IsSet( y!.obj ) and not IsSet( x!.obj ) then
            return x!.obj in y!.obj;
        else
            return false;
        fi;
        end;
        
        sz := Size(pts);
        
        adj := function(x,y)
             if x <= sz and y > sz then
                return x in blocks[y-sz];
             elif y <= sz and x > sz then
                return y in blocks[x-sz];
             else
                return false;
             fi;
        end;

        graph := Graph(Group(()), [1..sz+Size(blocks)], OnPoints, adj );
        girth := Girth(graph);

        if IsBipartite(graph) then
            if not girth = 2*Diameter(graph) then
                Error("<blocks> are not defining a generalised polygon");
            fi;
        else
            Error("<blocks are not defining a generalised polygon");
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
            Error("<blocks> do not define a thick finite generalised polygon");
        fi;
        
        listels := function( geom, i )
			if i = 1 then
				return List(pts,x->Wrap(geom,i,x));
			else
				return List(blocks,x->Wrap(geom,i,x));
			fi;
		end;
				
		vn := VertexNames(graph);
		
        shadpoint := function( pt )
            return List(Filtered(blocks,x->pt!.obj in x),y->Wrap(pt!.geo,2,y));
        end;
        
        shadline := function( line )
            return List(line!.obj,x->Wrap(line!.geo,1,x));
        end;
		
		#we have the graph now, the following is efficient.
		t := Length(Adjacency(graph,1)); # number of linbes on a point minus 1.

        dist := function( el1, el2 )
			if el1!.type=1 and el2!.type=1 then
				return Distance(graph,Position(pts,el1!.obj),Position(pts,el2!.obj));
			elif el1!.type=1 and el2!.type=2 then
				return Distance(graph,Position(pts,el1!.obj),sz+Position(blocks,el2!.obj));
			elif el1!.type=2 and el2!.type=1 then
				return Distance(graph,sz+Position(blocks,el1!.obj),Position(pts,el2!.obj));
			else 
				return Distance(graph,sz+Position(blocks,el1!.obj),sz+Position(blocks,el2!.obj));
			fi;
        end;

        gp := rec( pointsobj := pts, linesobj := blocks, incidence := i, listelements := listels, 
					shadowofpoint := shadpoint, shadowofline := shadline, distance := dist );

        Objectify( ty, gp );
        SetTypesOfElementsOfIncidenceStructure(gp, ["point","line"]);
        SetOrder(gp, [s, t]);
        SetRankAttr(gp, 2);
        Setter( IncidenceGraphOfGeneralisedPolygonAttr )( gp, graph );
        return gp;
  end );

############


#############################################################################
#O  CollineationGroup( <gp> )
###
InstallMethod( CollineationGroup, 
    "for a generalised polygon",
    [ IsElationGQ and IsGeneralisedPolygonRep ],
    function( gp )
        local graph, aut, act, stab, coll, ptsn;
        graph := IncidenceGraphOfGeneralisedPolygon( gp );
        aut := AutomorphismGroup( graph );
        ptsn := Set(gp!.pointsobj,x->Position(VertexNames(graph),x));
        stab := Stabilizer(aut, ptsn, OnSets);
        coll := Action(stab, ptsn, OnPoints);
		act := function(el,g)
			local src,img;
			if el!.type = 1 then
				src := Position(VertexNames(graph),el); #change wrt generic function which would be el!.obj
				img := src^g;
				return VertexNames(graph)[img];
			elif el!.type = 2 then
				src := Position(VertexNames(graph),el); #change wrt generic funciton ... 
				img := src^g;
				return VertexNames(graph)[img];
			fi;
		end;
        SetCollineationAction( coll, act );
		return coll;
    end );
############



gp := egq;
coll := CollineationGroup(gp);
p := Random(Points(gp));
l := Random(Lines(gp));
g := Random(coll);
act := CollineationAction(coll);
act(p,g);
act(l,g);

