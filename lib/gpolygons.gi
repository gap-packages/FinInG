#############################################################################
##
##  gpolygons.gi              FinInG package
##                                                              John Bamberg
##                                                              Anton Betten
##                                                              Jan De Beule
##                                                             Philippe Cara
##                                                            Michel Lavrauw
##                                                           Max Neunhoeffer
##
##  Copyright 2018	Colorado State University
##                  Sabancı Üniversitesi
##					Università degli Studi di Padova
##					Universiteit Gent
##					University of St. Andrews
##					University of Western Australia
##                  Vrije Universiteit Brussel
##
##
##  Implementation stuff for generalised polygons.
##
#############################################################################

#############################################################################
# Part I: generic part
# This section is generic, i.e. if someone constructs a GP and obeys the representation,
# these methods will work.
#############################################################################
# about IsGeneralisedPolygonRep
# objects belonging to IsGeneralisedPolygonRep should have several fields in their record:
# pointsobj, linesobj, incidence, listelements, shadowofpoint, shadowofline, distance.
#
# pointsobj: a list containing the *underlying objects* for the points of the GP
#
# linesobj: a list containing the *underlying objects* for the lines of the GP
#
# incidence: a function, taking two *elements of the GP* as argument, returning true or false
#			 we assume that the elements that are argument of this built in function, belong
#			 to the same geometry. See generic method IsIncident.
#
# listelements: a function taking an integer as argument, returning
#	a list of all elements of the GP of the given type. This list will be turned into an iterator
#	by the method installed for Iterator for elements of GPs.
#
# shadowofpoint: a function, taking a *point of a GP* as argument, returning a list
#	of lines incident with the given point.
#
# shadowofline: a function, taking a *line of a GP* as argument, returning a list
#	of points incident with the given line.
#		The method for ShadowOfElement should do the necessary checks and pass the appropriate
#		function to the method installed for Iterator for shadow objects.
#
# span: a function taking two *points of a GP* returning fail or the unique line incident with the two points.
#
# meet: a function taking two *lines of a GP* returning fail or the unique point incident with the two lines.
#
# distance: a function taking two *elements of a GP* and returning their distance in the incidence graph.
#
# action: a function describing an action on the *underlying objects*.
#
# gonality: it is not very explicitely documented, but one can actually construct
#   generalised n-gons for n different than 3,4,6,8. To avoid checking the diameter of the underlying
#   graph e.g. for ViewObj, in these case the field gonality is set upon creation. One could say
#   that this field replaces the categories IsProjectivePlaneCategory, IsGeneralisedQuadrangle, IsGeneralisedHexagon
#   and IsGeneralisedOctagon for these arbitrary cases.
#
# Note: - If an object belongs to IsGeneralisedPolygon, then the "generic operations" to explore the GP
#			are *applicable* (does not imply that a specific method is installed or will be working).
#		- If a GP is constructed using a "Generic method", the above fields are created as described, making
#			all the methods for the "generic operations" working.
#		- If a GP is created as an object in IsGeneralisedPolygon and IsGeneralisedPolygonRep, with some of
#			these fields lacking or different, than separate methods need to be installed for certain operations
#			This is not problematic. A typical example are the hexagons: they belong also to IsLieGeometry,
#			so it is easy to get the right methods selected. Furthermore, typical methods for Lie geometries become
#			applicable (but might also need separate methods).
#		- If a GP is constructed using the "generic construction methods", there is always an underlying graph (to
#			check whether the user really constructs a GP. Creating the graph can be time consuming, but there is
#			the possibility to use a group. There is no NC version, either you are a developper, and you want to make
#			a particular GP (e.g. the hexagons) and then you know what you do and there is no need to check whether your
#			developed GP is really a GP, or you are a user and must be protected against yourself.
#			The computed graph is stored as a mutable attribute.
#		- For the particular GPs: of course we know that they are a GP, so on construction, we do not compute
#			the underlying graph.
#		- The classical GQs belong to IsGeneralisedPolygon but *not* to IsGeneralisedPolygonRep. For them there is
#			either an atrtibute set on creation (e.g. Order), or a seperate method for other generic operations.
#
#############################################################################

#############################################################################
#
# Construction of GPs
#
#############################################################################

#############################################################################
#O  GeneralisedPolygonByBlocks( <list> )
# returns a GP, the points are Union(blocks), the lines are the blocks. functions
# for shadows are installed. It is checked whether this is really a GP.
# Note that we allow weak GPs: these are GPs "without order", i.e. bipartite graph,
# grith = 2*diameter, but no constant valency for the vertices in (on of) the components
# of the graph. We can do the necessary checks with LocalParameters.
##
InstallMethod( GeneralisedPolygonByBlocks,
    "for a homogeneous list",
    [ IsHomogeneousList ],
    function( blocks )
        local pts, gp, ty, i, graph, sz, adj, girth, shadpoint, shadline, s, t, dist, vn,
		listels, objs, act, spanoftwopoints, meetoftwolines, bicomp, pointvertices, lp, linevertices;
        pts := Union(blocks);

        i := function( x, y )
        if IsSet( x!.obj ) and not IsSet( y!.obj ) then
            return y!.obj in x!.obj;
        elif IsSet( y!.obj ) and not IsSet( x!.obj ) then
            return x!.obj in y!.obj;
        else
            return x!.obj = y!.obj;
        fi;
        end;

        sz := Size(pts);

		adj := function(x,y)
			if IsSet(x) and not IsSet(y) then
				return y in x;
			elif IsSet(y) and not IsSet(x) then
				return x in y;
			else
				return false;
			fi;
		end;

		act := function(x,g)
			return x;
		end;

        graph := Graph(Group(()), Concatenation(pts,blocks), act, adj );
        girth := Girth(graph);

        if IsBipartite(graph) then
            if not girth = 2*Diameter(graph) then
                Error("<blocks> are not defining a generalised polygon");
            fi;
        else
            Error("<blocks are not defining a generalised polygon");
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

		#we have the graph now, the following is easy.
		bicomp := Bicomponents(graph);
		pointvertices := First(bicomp,x->1 in x); #just take all points
		lp := LocalParameters(graph,pointvertices);
		t := lp[1][3]-1; #is the number of lines on a point, is -1 if there is no order, subtract to have t
		linevertices := First(bicomp,x->not 1 in x); #just take all lines
		lp := LocalParameters(graph,linevertices);
		s := lp[1][3]-1; #is the number of points on a line, is -1 if there is no orde, subtract to have s

		dist := function( el1, el2 )
			return Distance(graph,Position(vn,el1!.obj),Position(vn,el2!.obj));
		end;

        spanoftwopoints := function(x,y) #x and y are elements
            local i,j,span,el;
            i := Position(vn,x!.obj);
            j := Position(vn,y!.obj);
            el := Intersection(DistanceSet(graph,[1],i), DistanceSet(graph,[1],j));
            if not Length(el) = 0 then
                span := vn{el};
                return Wrap(x!.geo,2,span[1]);
            else
                Info(InfoFinInG, 1, "<x> and <y> do not span a line of gp");
                return fail;
            fi;
        end;

        meetoftwolines := function(x,y) #x and y are elements
            local i,j,meet,el;
            i := Position(vn,x!.obj);
            j := Position(vn,y!.obj);
            el := Intersection(DistanceSet(graph,[1],i), DistanceSet(graph,[1],j));
            if not Length(el) = 0 then
                meet := vn{el};
                return Wrap(x!.geo,1,meet[1]);
            else
                Info(InfoFinInG, 1, "<x> and <y> do meet in a common point of gp");
                return fail;
            fi;
        end;

        gp := rec( pointsobj := pts, linesobj := blocks, incidence := i, listelements := listels,
					shadowofpoint := shadpoint, shadowofline := shadline, distance := dist,
                    span := spanoftwopoints, meet := meetoftwolines );

        if t = -2 or s = -2 then
			ty := NewType( GeometriesFamily, IsWeakGeneralisedPolygon and IsGeneralisedPolygonRep );
            gp!.gonality := girth/2;
		elif girth = 6 then
            ty := NewType( GeometriesFamily, IsProjectivePlaneCategory and IsGeneralisedPolygonRep );
        elif girth = 8 then
            ty := NewType( GeometriesFamily, IsGeneralisedQuadrangle and IsGeneralisedPolygonRep );
        elif girth = 12 then
            ty := NewType( GeometriesFamily, IsGeneralisedHexagon and IsGeneralisedPolygonRep );
        elif girth = 16 then
            ty := NewType( GeometriesFamily, IsGeneralisedOctagon and IsGeneralisedPolygonRep );
        else
            ty := NewType( GeometriesFamily, IsGeneralisedPolygon and IsGeneralisedPolygonRep );
            gp!.gonality := girth/2;
        fi;

        Objectify( ty, gp );
        SetTypesOfElementsOfIncidenceStructure(gp, ["point","line"]);
        if s <> -2 and t <> -2 then
			SetOrder(gp, [s, t]);
		fi;
        SetRankAttr(gp, 2);
        Setter( IncidenceGraphAttr )( gp, graph );
        Setter( HasGraphWithUnderlyingObjectsAsVertices )( gp, true);
        return gp;
  end );

#############################################################################
#O  GeneralisedPolygonByIncidenceMatrix( <matrix> )
# returns a GP. points are [1..NrRows(matrix)], blocks are sets of entries equal to one.
# Blocks are then used through GeneralisedPolygonByBlocks.
# the commented out check dates from the times that this was only use to construct
# projective planes.
##
InstallMethod( GeneralisedPolygonByIncidenceMatrix,
    "for a matrix",
    [ IsMatrix ],
    function( mat )
    ## Rows represent blocks and columns represent points...
    local v, q, row, blocks, gp;
    v := NrRows(mat);
    #if not ForAll(mat, t->Size(t)=v) then
    #   Error("Matrix is not square");
    #fi;

    blocks := [];
    for row in mat do
        Add(blocks, Positions(row,1));
    od;

    gp := GeneralisedPolygonByBlocks( blocks );
    Setter( IncidenceMatrixOfGeneralisedPolygon )( gp, mat );
    return gp;
  end );

#############################################################################
#O  GeneralisedPolygonByElements( <pts>, <lns>, <inc> )
# <pts>: set of elements of some incidence structure, representing points of GP.
# <lns>: set of elements of some incidence structure, representing points of GP.
# <inc>: incidence function.
# it is checked throug the graph that the incidence structure is a GP.
##
InstallMethod( GeneralisedPolygonByElements,
    "for two sets (points and lines), and an incidence function",
    [ IsSet, IsSet, IsFunction ],
    function( pts, lns, inc )
    local adj, act, graph, ty, girth, shadpoint, shadline, s, t,
	gp, vn, dist, listels, wrapped_incidence, spanoftwopoints, meetoftwolines,
	bicomp, pointvertices, linevertices, lp;

    adj := function(x,y)
    if x in pts and y in pts then
        return false;
    elif x in lns and y in lns then
        return false;
    else
        return inc(x,y);
    fi;
    end;

    # this is the situation where the user gives no group at all. So action function is trivial too.
    act := function(x,g)
        return x;
    end;

    graph := Graph(Group(()), Concatenation(pts,lns), act, adj, true );
    girth := Girth(graph);

    if IsBipartite(graph) then
        if not girth = 2*Diameter(graph) then
            Error("<pts>, <lns>, <inc> are not defining a generalised polygon");
        fi;
    else
        Error("elements are not defining a generalised polygon");
    fi;

	bicomp := Bicomponents(graph);
	pointvertices := First(bicomp,x->1 in x); #just take all points
	lp := LocalParameters(graph,pointvertices);
	t := lp[1][3]-1; #is the number of lines on a point, is -1 if there is no order, subtract to have t
	linevertices := First(bicomp,x->(Size(pts)+1) in x); #just take all lines
	lp := LocalParameters(graph,linevertices);
	s := lp[1][3]-1; #is the number of points on a line, is -1 if there is no order, subtract to have s

	# inc takes in fact underlying objects as arguments. So we must make a new function that takes
	# as arguments elements of this geometry and pipes the underlying objects to inc.

	wrapped_incidence := function(x,y)
		return inc(x!.obj,y!.obj);
	end;

    listels := function( geom, i )
		if i = 1 then
			return List(pts,x->Wrap(geom,i,x));
		else
			return List(lns,x->Wrap(geom,i,x));
		fi;
	end;

    shadpoint := function( pt )
        return List(vn{Adjacency(graph,Position(vn,pt!.obj))},x->Wrap(gp,2,x));
    end;

    shadline := function( line )
        return List(vn{Adjacency(graph,Position(vn,line!.obj))},x->Wrap(gp,1,x));
    end;

    vn := VertexNames(graph);
	dist := function( el1, el2 )
        return Distance(graph,Position(vn,el1!.obj),Position(vn,el2!.obj));
    end;

    spanoftwopoints := function(x,y) #x and y are elements
        local i,j,span,el;
        i := Position(vn,x!.obj);
        j := Position(vn,y!.obj);
        el := Intersection(DistanceSet(graph,[1],i), DistanceSet(graph,[1],j));
        if not Length(el) = 0 then
            span := vn{el};
            return Wrap(x!.geo,2,span[1]);
        else
            Info(InfoFinInG, 1, "<x> and <y> do not span a line of gp");
            return fail;
        fi;
    end;

    meetoftwolines := function(x,y) #x and y are elements
        local i,j,meet,el;
        i := Position(vn,x!.obj);
        j := Position(vn,y!.obj);
        el := Intersection(DistanceSet(graph,[1],i), DistanceSet(graph,[1],j));
        if not Length(el) = 0 then
            meet := vn{el};
            return Wrap(x!.geo,1,meet[1]);
        else
            Info(InfoFinInG, 1, "<x> and <y> do meet in a common point of gp");
            return fail;
        fi;
    end;

    gp := rec( pointsobj := pts, linesobj := lns, incidence := wrapped_incidence, listelements := listels,
				shadowofpoint := shadpoint, shadowofline := shadline, distance := dist,
                span := spanoftwopoints, meet := meetoftwolines );

	if t = -2 or s = -2 then
		ty := NewType( GeometriesFamily, IsWeakGeneralisedPolygon and IsGeneralisedPolygonRep );
		gp!.gonality := girth/2;
	elif girth = 6 then
        ty := NewType( GeometriesFamily, IsProjectivePlaneCategory and IsGeneralisedPolygonRep );
    elif girth = 8 then
        ty := NewType( GeometriesFamily, IsGeneralisedQuadrangle and IsGeneralisedPolygonRep );
    elif girth = 12 then
        ty := NewType( GeometriesFamily, IsGeneralisedHexagon and IsGeneralisedPolygonRep );
    elif girth = 16 then
        ty := NewType( GeometriesFamily, IsGeneralisedOctagon and IsGeneralisedPolygonRep );
    else
        ty := NewType( GeometriesFamily, IsGeneralisedPolygon and IsGeneralisedPolygonRep );
        gp!.gonality := girth/2;
    fi;

    Objectify( ty, gp );
	if s <> -2 and t <> -2 then
		SetOrder(gp, [s, t]);
	fi;
    SetTypesOfElementsOfIncidenceStructure(gp, ["point","line"]);
    SetRankAttr(gp, 2);
    Setter( IncidenceGraphAttr )( gp, graph );
    Setter( HasGraphWithUnderlyingObjectsAsVertices )( gp, true);
    return gp;
end );

#############################################################################
#O  GeneralisedPolygonByElements( <pts>, <lns>, <inc>, <group>, <act> )
# <pts>: set of elements of some incidence structure, representing points of GP.
# <lns>: set of elements of some incidence structure, representing points of GP.
# <inc>: incidence function.
# <group>: group preserving <pts>, <lns> and <inc>
# <act>: action function for group on <pts> and <lns>.
# it is checked throug the graph that the incidence structure is a GP, by using
# the group, this is much more efficient. The user is responsible the <group>
# really preserves <pts> and <lns>
##
InstallMethod( GeneralisedPolygonByElements,
    "for two sets (points and lines), and an incidence function",
    [ IsSet, IsSet, IsFunction, IsGroup, IsFunction ],
    function( pts, lns, inc, group, act )
    local adj, graph, ty, girth, shadpoint, shadline, s, t, gp, vn,
	dist, listels, wrapped_incidence, spanoftwopoints, meetoftwolines,
	bicomp, pointvertices, linevertices, lp;

    adj := function(x,y)
    if x in pts and y in pts then
        return false;
    elif x in lns and y in lns then
        return false;
    else
        return inc(x,y);
    fi;
    end;

    graph := Graph(group, Concatenation(pts,lns), act, adj, true );
    girth := Girth(graph);

    if IsBipartite(graph) then
        if not girth = 2*Diameter(graph) then
            Error("<blocks> are not defining a generalised polygon");
        fi;
    else
        Error("elements are not defining a generalised polygon");
    fi;

	bicomp := Bicomponents(graph);
	pointvertices := First(bicomp,x->1 in x); #just take all points
	lp := LocalParameters(graph,pointvertices);
	t := lp[1][3]-1; #is the number of lines on a point, is -1 if there is no order, subtract -1 to have t
	linevertices := First(bicomp,x->(Size(pts)+1) in x); #just take all lines
	lp := LocalParameters(graph,linevertices);
	s := lp[1][3]-1; #is the number of points on a line, is -1 if there is no order, subtract -1 to have s

    vn := VertexNames(graph);

	# inc takes in fact underlying objects as arguments. So we must make a new function that takes
	# as arguments elements of this geometry and pipes the underlying objects to inc.

	wrapped_incidence := function(x,y)
		return inc(x!.obj,y!.obj);
	end;

    listels := function( geom, i )
		if i = 1 then
			return List(pts,x->Wrap(geom,i,x));
		else
			return List(lns,x->Wrap(geom,i,x));
		fi;
	end;

    shadpoint := function( pt )
        return List(vn{Adjacency(graph,Position(vn,pt!.obj))},x->Wrap(gp,2,x));
    end;

    shadline := function( line )
        return List(vn{Adjacency(graph,Position(vn,line!.obj))},x->Wrap(gp,1,x));
    end;

    dist := function( el1, el2 )
        return Distance(graph,Position(vn,el1!.obj),Position(vn,el2!.obj));
    end;

    spanoftwopoints := function(x,y) #x and y are elements
        local i,j,span,el;
        i := Position(vn,x!.obj);
        j := Position(vn,y!.obj);
        el := Intersection(DistanceSet(graph,[1],i), DistanceSet(graph,[1],j));
        if not Length(el) = 0 then
            span := vn{el};
            return Wrap(x!.geo,2,span[1]);
        else
            Info(InfoFinInG, 1, "<x> and <y> do not span a line of gp");
            return fail;
        fi;
    end;

    meetoftwolines := function(x,y) #x and y are elements
        local i,j,meet,el;
        i := Position(vn,x!.obj);
        j := Position(vn,y!.obj);
        el := Intersection(DistanceSet(graph,[1],i), DistanceSet(graph,[1],j));
        if not Length(el) = 0 then
            meet := vn{el};
            return Wrap(x!.geo,1,meet[1]);
        else
            Info(InfoFinInG, 1, "<x> and <y> do meet in a common point of gp");
            return fail;
        fi;
    end;

    gp := rec( pointsobj := pts, linesobj := lns, incidence := wrapped_incidence, listelements := listels,
				shadowofpoint := shadpoint, shadowofline := shadline, distance := dist, span := spanoftwopoints,
                meet := meetoftwolines, action := act );

	if t = -2 or s = -2 then
		ty := NewType( GeometriesFamily, IsWeakGeneralisedPolygon and IsGeneralisedPolygonRep );
		gp!.gonality := girth/2;
	elif girth = 6 then
        ty := NewType( GeometriesFamily, IsProjectivePlaneCategory and IsGeneralisedPolygonRep );
    elif girth = 8 then
        ty := NewType( GeometriesFamily, IsGeneralisedQuadrangle and IsGeneralisedPolygonRep );
    elif girth = 12 then
        ty := NewType( GeometriesFamily, IsGeneralisedHexagon and IsGeneralisedPolygonRep );
    elif girth = 16 then
        ty := NewType( GeometriesFamily, IsGeneralisedOctagon and IsGeneralisedPolygonRep );
    else
        ty := NewType( GeometriesFamily, IsGeneralisedPolygon and IsGeneralisedPolygonRep );
        gp!.gonality := girth/2;
    fi;

    Objectify( ty, gp );
	if s <> -2 and t <> -2 then
		SetOrder(gp, [s, t]);
	fi;
	SetTypesOfElementsOfIncidenceStructure(gp, ["point","line"]);
    SetRankAttr(gp, 2);
    Setter( IncidenceGraphAttr )( gp, graph );
    Setter( HasGraphWithUnderlyingObjectsAsVertices )( gp, true);
    return gp;
end );

#############################################################################
# View methods for GPs.
#############################################################################

InstallMethod( ViewObj,
	"for a projective plane in GP rep",
	[ IsProjectivePlaneCategory and IsGeneralisedPolygonRep],
	function( p )
        if HasOrder(p) then
            Print("<projective plane order ",Order(p)[1],">");
        else
            Print("<projective plane>");
        fi;
	end );

InstallMethod( ViewObj,
	"for a projective plane in GP rep",
	[ IsGeneralisedQuadrangle and IsGeneralisedPolygonRep],
	function( p )
        if HasOrder(p) then
            Print("<generalised quadrangle of order ",Order(p),">");
        else
            Print("<generalised quadrangle>");
        fi;
	end );

InstallMethod( ViewObj,
	"for a projective plane in GP rep",
	[ IsGeneralisedHexagon and IsGeneralisedPolygonRep],
	function( p )
        if HasOrder(p) then
            Print("<generalised hexagon of order ",Order(p),">");
        else
            Print("<generalised hexagon>");
        fi;
	end );

InstallMethod( ViewObj,
	"for a projective plane in GP rep",
	[ IsGeneralisedOctagon and IsGeneralisedPolygonRep],
	function( p )
        if HasOrder(p) then
            Print("<generalised octagon of order ",Order(p),">");
        else
            Print("<generalised octagon>");
        fi;
	end );

InstallMethod( ViewObj,
	"for a projective plane in GP rep",
	[ IsGeneralisedPolygon and IsGeneralisedPolygonRep],
	function( gp )
        if HasOrder(gp) then
            Print("<generalised polygon of gonality ",String(gp!.gonality)," and order ",Order(gp),">");
        else
            Print("<generalised polygon of gonality ",String(gp!.gonality),">");
        fi;
	end );

InstallMethod( ViewObj,
	"for a projective plane in GP rep",
	[ IsWeakGeneralisedPolygon and IsGeneralisedPolygonRep],
	function( gp )
		Print("<weak generalised polygon of gonality ",String(gp!.gonality),">");
	end );

#############################################################################
#
# Basic methods for elements (including construction and iterator).
#
#############################################################################

#############################################################################
#O  Order( <x> )
##
InstallMethod( Order,
	"for a weak generalised polygon",
	[ IsWeakGeneralisedPolygon ],
	function( gp )
		Error("<gp> is a weak generalised polygon and has no order");
	end );

#############################################################################
#O  UnderlyingObject( <x> )
##
InstallMethod( UnderlyingObject,
	"for an element of a LieGeometry",
	[ IsElementOfGeneralisedPolygon ],
	function( x )
		return x!.obj;
	end );

#############################################################################
#O  ObjectToElement( <geom>, <type>, <obj> )
# returns the subspace of <geom>, with representative <v> and subspace at infinity
# determined by <m> if and only if <obj> is the list [v,m].
##
InstallMethod( ObjectToElement,
	"for ageneralised polygon, an integer and an object",
	[ IsGeneralisedPolygon and IsGeneralisedPolygonRep, IsPosInt, IsObject],
	function(gp, t, obj)
		if t=1 then
			if obj in gp!.pointsobj then
				return Wrap(gp,t,obj);
			else
				Error("<obj> does not represent a point of <gp>");
			fi;
		elif t=2 then
			if obj in gp!.linesobj then
				return Wrap(gp,t,obj);
			else
				Error("<obj> does not represent a line of <gp>");
			fi;
		else
			Error("<gp> is a point-line geometry not containing elements of type ",t);
		fi;
	end );

#############################################################################
#O  ObjectToElement( <geom>, <obj> )
# returns the subspace of <geom>, with representative <v> and subspace at infinity
# determined by <m> if and only if <obj> is the list [v,m].
##
InstallMethod( ObjectToElement,
	"for ageneralised polygon and an object",
	[ IsGeneralisedPolygon and IsGeneralisedPolygonRep, IsObject],
	function(gp, obj)
		if obj in gp!.pointsobj then
			return Wrap(gp,1,obj);
		elif obj in gp!.linesobj then
			return Wrap(gp,2,obj);
		else
			Error("<obj> does not represent an element of <gp>");
		fi;
	end );

#############################################################################
#O  ElementsOfIncidenceStructure( <gp>, <j> )
# returns the elements of <gp> of type <j>
##
InstallMethod( ElementsOfIncidenceStructure,
	"for a generalised polygon and a positive integer",
	[IsGeneralisedPolygon and IsGeneralisedPolygonRep, IsPosInt],
	function( gp, j )
		local s, t, sz;
		if j in [1,2] then
			s := Order(gp)[j]; t := Order(gp)[3-j];
		else
			Error("Incorrect type value");
		fi;
		if IsProjectivePlaneCategory(gp) then
			sz := s^2 + s + 1;
		elif IsGeneralisedQuadrangle(gp) then
			sz := (1+s)*(1+s*t);
		elif IsGeneralisedHexagon(gp) then
			sz := (1+s)*(1+s*t+s^2*t^2);
		elif IsGeneralisedOctagon(gp) then
			sz := (1+s)*(1+s*t+s^2*t^2+s^3*t^3);
		else
            if j=1 then
                sz := Length(gp!.pointsobj);
            else
                sz := Length(gp!.linesobj);
            fi;
        fi;
		return Objectify( NewType( ElementsCollFamily, IsElementsOfGeneralisedPolygon and
                                IsElementsOfGeneralisedPolygonRep),
        rec( geometry := gp, type := j, size := sz )
						);
	end );

#############################################################################
#O  ElementsOfIncidenceStructure( <gp>, <j> )
# returns the elements of <gp> of type <j>
##
InstallMethod( ElementsOfIncidenceStructure,
	"for a generalised polygon and a positive integer",
	[IsWeakGeneralisedPolygon and IsGeneralisedPolygonRep, IsPosInt],
	function( gp, j )
		local s, t, sz;
		if not j in [1,2] then
			Error("Incorrect type value");
		fi;
        if j=1 then
			sz := Length(gp!.pointsobj);
		else
			sz := Length(gp!.linesobj);
		fi;
		return Objectify( NewType( ElementsCollFamily, IsElementsOfGeneralisedPolygon and
							IsElementsOfGeneralisedPolygonRep),
						rec( geometry := gp, type := j, size := sz ) );
	end );

#############################################################################
#O  Points( <gp>  )
# returns the points of <gp>.
##
InstallMethod( Points,
	"for a generalised polygon",
	[IsGeneralisedPolygon and IsGeneralisedPolygonRep],
	function( gp )
		return ElementsOfIncidenceStructure(gp, 1);
	end);

#############################################################################
#O  Lines( <gp>  )
# returns the lines of <gp>.
##
InstallMethod( Lines,
	"for a generalised polygon",
	[IsGeneralisedPolygon and IsGeneralisedPolygonRep],
	function( gp )
		return ElementsOfIncidenceStructure(gp, 2);
	end);

#############################################################################
# Display methods: Element collections
#############################################################################

InstallMethod( ViewObj,
	"for elements of a generalised polygon",
	[ IsElementsOfGeneralisedPolygon and IsElementsOfGeneralisedPolygonRep ],
	function( vs )
		local l;
		l := ["points","lines"];
		Print("<", l[vs!.type]," of ");
		ViewObj(vs!.geometry);
		Print(">");
	end );

InstallMethod( PrintObj,
	"for elements of a generalised polygon",
	[ IsElementsOfGeneralisedPolygon and IsElementsOfGeneralisedPolygonRep ],
	function( vs )
		Print("ElementsOfIncidenceStructure( ",vs!.geometry," , ",vs!.type,")");
	end );

#############################################################################
#O  Size( <gp>  )
# returns the size of a collection of elements of a <gp>
##
InstallMethod(Size,
	"for elements of a generalised polygon",
	[IsElementsOfGeneralisedPolygon],
	vs -> vs!.size );

#############################################################################
#O  Iterator( <vs>  )
# returns an iterator for the elements of a gp
##
InstallMethod( Iterator,
	"for elements of a generalised polygon",
	[ IsElementsOfGeneralisedPolygon and IsElementsOfGeneralisedPolygonRep],
	function( vs )
		local gp, j, vars;
		gp := vs!.geometry;
		j := vs!.type;
		if j in [1,2] then
			return IteratorList(gp!.listelements(gp,j)); #looks a bit strange, but correct.
		else
			Error("Element type does not exist");
		fi;
	end );

#############################################################################
#O  Iterator( <shadow>  )
# returns an iterator for a shadow of elements of a gp
##
InstallMethod( Iterator,
	"for shadow elements of a generalised polygon",
	[IsShadowElementsOfGeneralisedPolygon and IsShadowElementsOfGeneralisedPolygonRep ],
	function( vs )
        return IteratorList(vs!.func(vs!.element));
	end);

#############################################################################
#O  Random( <vs>  )
#	In general, the list of elements might be stored in the GP itself. Then
#	the standard method uses the Iterator. This standard method will be called
#	also if our particular GPs do not have a collienation/elation group, the
#	iterator will be used, which causes the computation of the collineation
#   group including a nice monomorphism. The next time Random is used, this
#	group is used.
##
InstallMethod( Random,
	"for a collection of elements of a generalised polygon",
	[ IsElementsOfGeneralisedPolygon and IsElementsOfGeneralisedPolygonRep ],
	function( vs )
		local geo, type, class, group, rep, act;
		geo := vs!.geometry;
		type := vs!.type;
		if IsClassicalGeneralisedHexagon(geo) and HasCollineationGroup(geo) then
			group := CollineationGroup(geo);
			act := CollineationAction(group);
			rep := RepresentativesOfElements(geo)[type];
			return act(rep,Random(group));
		elif HasElationGroup(geo) then
			group := ElationGroup(geo);
			act := CollineationAction(group);
			rep := RepresentativesOfElements(geo)[type];
			return act(Random(rep),Random(group));
		else
			TryNextMethod();
		fi;
	end );

#############################################################################
#O  IsIncident( <x>, <y>  )
# simply uses the incidence relation that is built in in the gp.
# caveat: check here that elements belong to the same geometry!
##
InstallMethod( IsIncident,
	"for elements of a generalised polygon",
    [IsElementOfGeneralisedPolygon, IsElementOfGeneralisedPolygon],
	function( x, y )
		local inc;
		if not x!.geo = y!.geo then
			Error("The elements <x> and <y> do not belong to the same geometry");
		else
			inc := x!.geo!.incidence;
			return inc(x, y);
		fi;
	end );

#############################################################################
#O  Span( <x>, <y>  )
# return the line spanned by <x> and <y>, if they span a line at all.
##
InstallMethod( Span,
    "for two elements of a generalised polygon",
    [ IsElementOfGeneralisedPolygon, IsElementOfGeneralisedPolygon ],
    function( x, y )
        local graph, vn, el, i, j, span;
        if not x!.type = 1 and y!.type = 1 then
            Error("<x> and <y> must be points of a generalised polygon");
        elif not x!.geo = y!.geo then
            Error("<x> and <y> must belong to the same generalised polygon");
        fi;
        return x!.geo!.span(x,y);
    end );

#############################################################################
#O  Meet( <x>, <y>  )
# return the line spanned by <x> and <y>, if they span a line at all.
##
InstallMethod( Meet,
    "for two elements of a generalised polygon",
    [ IsElementOfGeneralisedPolygon, IsElementOfGeneralisedPolygon ],
    function( x, y )
        local graph, vn, el, i, j, meet;
        if not x!.type = 2 and y!.type = 2 then
            Error("<x> and <y> must be lines of a generalised polygon");
        elif not x!.geo = y!.geo then
            Error("<x> and <y> must belong to the same generalised polygon");
        fi;
        return x!.geo!.meet(x,y);
    end );

#############################################################################
#O  Wrap( <geo>, <type>, <o>  )
# returns the element of <geo> represented by <o>.
# this method is generic, but of course not fool proof.
##
InstallMethod( Wrap,
	"for a generalised polygon and an object",
	[IsGeneralisedPolygon, IsPosInt, IsObject],
	function( geo, type, o )
		local w;
		w := rec( geo := geo, type := type, obj := o );
		Objectify( NewType( ElementsOfIncidenceStructureFamily,
			IsElementOfIncidenceStructureRep and IsElementOfGeneralisedPolygon ), w );
		return w;
  end );

# CHECKED 11/09/11 jdb
#############################################################################
#A  TypesOfElementsOfIncidenceStructure( <gp> )
# returns the names of the types of the elements of the projective space <ps>
# the is a helper operation.
##
InstallMethod( TypesOfElementsOfIncidenceStructurePlural,
	"for a generalised polygon in the general representation",
    [ IsGeneralisedPolygon and IsGeneralisedPolygonRep ],
		x -> ["points", "lines"] );

#############################################################################
#O ShadowOfElement(<gp>, <el>, <j> )
##
InstallMethod( ShadowOfElement,
	"for a generalised polygon, an element, and an integer",
	[IsGeneralisedPolygon and IsGeneralisedPolygonRep, IsElementOfGeneralisedPolygon, IsPosInt],
	function( gp, el, j )
		local shadow, func;
        if not AmbientGeometry(el) = gp then
            Error("ambient geometry of <el> is not <gp>");
        fi;
        if j = el!.type then
            func := x->[x];
        elif j = 1 then
            func := gp!.shadowofline;
        elif j = 2 then
            func := gp!.shadowofpoint;
        else
            Error("<gp> has no shadow elements of type", j );
        fi;

        shadow := rec( geometry := gp, type := j, element := el, func := func );
		return Objectify( NewType( ElementsCollFamily, IsElementsOfIncidenceStructure and
							IsShadowElementsOfGeneralisedPolygon and
							IsShadowElementsOfGeneralisedPolygonRep),
							shadow
						);
	end);

#############################################################################
# View methods for shadow objects.
#############################################################################

InstallMethod( ViewObj,
	"for shadow elements of a generalised polygon",
	[ IsShadowElementsOfGeneralisedPolygon and IsShadowElementsOfGeneralisedPolygonRep ],
	function( vs )
		Print("<shadow ",TypesOfElementsOfIncidenceStructurePlural(vs!.geometry)[vs!.type]," in ");
		ViewObj(vs!.geometry);
		Print(">");
	end );

#############################################################################
#O  Points( <el> )
# returns the points, i.e. elements of type <1> in <el>, relying on ShadowOfElement
# for particular <el>.
##
InstallMethod( Points,
    "for an element of a generalised polygon",
	[ IsElementOfGeneralisedPolygon ],
	function( var )
		return ShadowOfElement(var!.geo, var, 1);
	end );

#############################################################################
#O  Lines( <el> )
# returns the lines, i.e. elements of type <2> in <el>, relying on ShadowOfElement
# for particular <el>.
##
InstallMethod( Lines,
    "for an element of a generalised polygon",
	[ IsElementOfGeneralisedPolygon ],
	function( var )
		return ShadowOfElement(var!.geo, var, 2);
	end );

#############################################################################
#O  DistanceBetweenElements( <gp>, <el1>, <el2> )
# returns the distance in the incidence graph between two elements
# Important notice: the '5' before the function increases the priority of this
# method for a pair of points satisfying the filters. The only situation where
# this is relevant is to compute the distance between elements of the Classical
# hexagons. Elements of these are also IsSubspaceOfClassicalPolarSpace. For
# elements of classical GQs, the above method for IsSubspaceOfClassicalPolarSpace
# must be applied. For elements of classical GHs, the generic method here must be
# used. But elements of classical GHs satisfy both filters, so the rank of the filters
# is used to select the method. As the rank of IsSubspaceOfClassicalPolarSpace is
# larger than the rank of IsElementOfGeneralisedPolygon, the wrong method gets
# selected for elements of GHs. The difference in rank is currently 2, times 2 makes
# 4, so adding 5 here should do the job, and it does.
##
InstallMethod( DistanceBetweenElements,
    "for a gp in gpRep and two of its elements",
	[ IsElementOfGeneralisedPolygon, IsElementOfGeneralisedPolygon],
    5,
	function( p, q )
		local geo;
        geo := p!.geo;
        if not geo = q!.geo then
            Error("<p> and <q> are not elements of the same generalised polygon");
        fi;
        return geo!.distance(p,q);
	end );

#############################################################################
#O  IncidenceGraph( <gp> )
# We could install a generic method. But currently, our particular GPs (hexagons,
# elation GQs, and classical GQs) have a particular method for good reasons. All other GPs
# currently possible to construct, have their incidence graph computed upon construction.
# So we may restrict here to checking whether this attribute is bounded, and return it,
# or print an error message. Note that we deal here with a mutable attribute.
###
InstallMethod( IncidenceGraph,
    "for a generalised polygon (in all possible representations",
    [ IsGeneralisedPolygon ],
    function( gp )
    local points, lines, graph, sz, adj, elations, gg, coll;
    #if not "grape" in RecNames(GAPInfo.PackagesLoaded) then
    #   Error("You must load the GRAPE package\n");
    #fi;
    if IsBound(gp!.IncidenceGraphAttr) then
       return gp!.IncidenceGraphAttr;
    else
        Error("no method installed currently");
    fi;
    end );

#############################################################################
#O  IncidenceMatrixOfGeneralisedPolygon( <gp> )
#
InstallMethod( IncidenceMatrixOfGeneralisedPolygon,
	"for a generalised polygon",
	[ IsGeneralisedPolygon ],
	function( gp )
		local graph, mat, incmat, szpoints, szlines;
		graph := IncidenceGraph( gp );
		mat := CollapsedAdjacencyMat(Group(()), graph);

    ## The matrix above is the adjacency matrix of the
    ## bipartite incidence graph.

		szpoints := Size(Points(gp));
		szlines := Size(Lines(gp));

		incmat := mat{[1..szpoints]}{[szpoints+1..szpoints+szlines]};
		return incmat;
	end );

#############################################################################
#O  CollineationGroup( <gp> )
# This method is generic. Note that:
# - for classical GQs, we have completely different methods to compute their
#	collineation group, of course for good reasons;
# - for classical generalised hexagons, the same remark applies;
# - when a GP is constructed through generic methods, the underlying graph is
#	always computed, since this is the only way to check if the input is not rubbish.
#	But then the VertexNames of the constructed graph are the underlying objects.
#	For particular GQs, the underlying graph is not computed upon construction.
#   But computing a graph afterwards, is very naturally done with the elements themselves
#   as VertexNames. This has some technical consequences to compute the collineation
#   group and to define the CollineationAction of it.
#	To distinguish in this method, we introduced the property HasGraphWithUnderlyingObjectsAsVertices.
###
InstallMethod( CollineationGroup,
    "for a generalised polygon",
    [ IsGeneralisedPolygon and IsGeneralisedPolygonRep ],
    function( gp )
        local graph, aut, act, stab, coll, ptsn, points, pointsobj;
        graph := IncidenceGraph( gp );
        aut := AutomorphismGroup( graph );
        points := AsList(Points(gp));
        if HasGraphWithUnderlyingObjectsAsVertices(gp) then
            pointsobj := List(points,x->x!.obj);
            ptsn := Set(pointsobj,x->Position(VertexNames(graph),x));
        else
            ptsn := Set(points,x->Position(VertexNames(graph),x));
        fi;
        coll := Stabilizer(aut, ptsn, OnSets);
        if HasGraphWithUnderlyingObjectsAsVertices(gp) then
            act := function(el,g)
                local src,img;
                if el!.type = 1 then
                    src := Position(VertexNames(graph),el!.obj);
                    img := src^g;
                    return Wrap(gp,1,VertexNames(graph)[img]);
                elif el!.type = 2 then
                    src := Position(VertexNames(graph),el!.obj);
                    img := src^g;
                    return Wrap(gp,2,VertexNames(graph)[img]);
                fi;
            end;
        else
            act := function(el,g)
                local src,img;
				src := Position(VertexNames(graph),el); #change wrt generic function which would be el!.obj
				img := src^g;
				return VertexNames(graph)[img];
            end;
        fi;
        SetCollineationAction( coll, act );
		return coll;
    end );

#############################################################################
#O  BlockDesignOfGeneralisedPolygon( <gp> )
# Note about BlockDesign: this is a global (so read-only) function in the
# "design" package. We have created in FinInG a function BlockDesign (not
# through DeclareGlobalFunction, so it can be overwritten at user level and
# through loading other packages), so that the method for BlockDesignOfGeneralisedPolygon
# can be loaded without the "design" package loaded. We make sure that this method checks
# whether design is loaded, and produces an error when this is not the case.
#
InstallMethod( BlockDesignOfGeneralisedPolygon,
    "for a generalised polygon",
    [ IsGeneralisedPolygon and IsGeneralisedPolygonRep ],
    function( gp )
        local points, lines, des, blocks, l, b, elations, gg, orbs;
        if not IsPackageLoaded("design") then #this is a gap4r10 function.
        #if not "design" in RecNames(GAPInfo.PackagesLoaded) then
            Error("You must load the DESIGN package\n");
        fi;
        if IsBound(gp!.BlockDesignOfGeneralisedPolygonAttr) then
            return gp!.BlockDesignOfGeneralisedPolygonAttr;
        fi;
        points := AsList(Points(gp));;
        lines := AsList(Lines(gp));;
        if IsElationGQ(gp) and HasElationGroup( gp ) then
            elations := ElationGroup(gp);
            Info(InfoFinInG, 1, "Computing orbits on lines of gen. polygon...");
            orbs := List( Orbits(elations, lines, CollineationAction(elations)), Representative);
            orbs := List(orbs, l -> Filtered([1..Size(points)], i -> points[i] * l));
            gg := Action(elations, points, CollineationAction( elations ) );
            Info(InfoFinInG, 1, "Computing block design of generalised polygon...");
            des := BlockDesign(Size(points), orbs, gg );
        elif HasCollineationGroup(gp) then
            gg := CollineationGroup(gp);
            orbs := List( Orbits(gg, lines, CollineationAction(gg)), Representative);
            orbs := List(orbs, l -> Filtered([1..Size(points)], i -> points[i] * l));
            gg := Action(gg, points, CollineationAction( gg ) );
            des := BlockDesign(Size(points), orbs, gg );
        else
            blocks := [];
            for l in lines do
                b := Filtered([1..Size(points)], i -> points[i] * l);
                Add(blocks, b);
            od;
            des := BlockDesign(Size(points), Set(blocks));
        fi;
        Setter( BlockDesignOfGeneralisedPolygonAttr )( gp, des );
        return des;
  end );

#############################################################################
#
# Part II: particular models of GPs.
#
#############################################################################

#############################################################################
#
# Desarguesian projective planes. Methods needed:
#	- DistanceBetweenElements
#	- IncidenceGraph
#
#############################################################################

#############################################################################
#O  DistanceBetweenElements( <v>, <w> )
# It is possible to create points and lines of PG(2,q) in the category
# IsElementsOfGeneralisedPolygon (or even more specified). But this would increase
# the dependency of projectivespace.gi on gpolygons.gd, which we want to avoid.
###
InstallMethod( DistanceBetweenElements,
    "for subspaces of a projective space",
    [ IsSubspaceOfProjectiveSpace, IsSubspaceOfProjectiveSpace ],
    function( v, w )
        if not IsDesarguesianPlane(v!.geo) then
            Error( "Elements must have a generalised polygon as ambient geometry" );
        fi;
        if not v!.geo = w!.geo then
            Error( "Elements must belong to the same generalised polygon ");
        fi;
        if v = w then
            return 0;
        elif v!.type <> w!.type then
			if IsIncident(v,w) then
				return 1;
			else
				return 3;
			fi;
		else
			return 2;
		fi;
	end );

#############################################################################
#O  IncidenceGraph( <gp> )
# Note that computing the collineation group of a projective space is zero
# computation time. So useless to print the warning here if the group is not
# yet computed.
##
# Note that there is actually a method for IsProjectiveSpace, and IsDesarguesianPlane
# is a subcategory of IsProjectiveSpace. But the property HasGraphWithUnderlyingObjectsAsVertices
# is important for IsGeneralisedPolygon. We could check in the method for projective
# spaces whether the spaces is a plane, and set the property there, but we want
# to keep projectivespace.gi independent from gpolygons.gd.
###
InstallMethod( IncidenceGraph,
    "for a Desarguesian plane",
    [ IsDesarguesianPlane ],
    function( gp )
        local points, lines, graph, adj, group, coll, sz;
        #if not "grape" in RecNames(GAPInfo.PackagesLoaded) then
        #    Error("You must load the GRAPE package\n");
        #fi;
        if IsBound(gp!.IncidenceGraphAttr) then
            return gp!.IncidenceGraphAttr;
        fi;
        points := AsList(Points(gp));
        lines := AsList(Lines(gp));
        Setter( HasGraphWithUnderlyingObjectsAsVertices )( gp, false );
        Info(InfoFinInG, 1, "Computing incidence graph of generalised polygon...");
        adj := function(x,y)
            if x!.type <> y!.type then
                return IsIncident(x,y);
            else
                return false;
            fi;
        end;
        group := CollineationGroup(gp);
        graph := Graph(group,Concatenation(points,lines),OnProjSubspaces,adj,true);
        Setter( IncidenceGraphAttr )( gp, graph );
        return graph;
    end );

#############################################################################
#
# Classical GQs. Methods needed:
#	- DistanceBetweenElements
#	- IncidenceGraph
#
#############################################################################

#############################################################################
#O  DistanceBetweenElements( <v>, <w> )
# It is possible to create points and lines of PG(2,q) in the category
# IsElementsOfGeneralisedPolygon (or even more specified). But this would increase
# the dependency of projectivespace.gi on gpolygons.gd, which we want to avoid.
###
InstallMethod( DistanceBetweenElements,
    "for subspaces of a projective space",
    [ IsSubspaceOfClassicalPolarSpace, IsSubspaceOfClassicalPolarSpace ],
    function( v, w )
        if not IsClassicalGQ(v!.geo) then
            Error( "Elements must have a generalised polygon as ambient geometry" );
        fi;
        if not v!.geo = w!.geo then
            Error( "Elements must belong to the same generalised polygon ");
        fi;
        if v = w then
            return 0;
        elif v!.type <> w!.type then
			if IsIncident(v,w) then
				return 1;
			else
				return 3;
			fi;
		else
			if v!.type = 1 then
				if Span(v,w) in v!.geo then
					return 2;
				else
					return 4;
				fi;
			else
				if ProjectiveDimension(Meet(v,w)) = 0 then
					return 2;
				else
					return 4;
				fi;
			fi;
		fi;
	end );

#############################################################################
#O  IncidenceGraph( <gp> )
##
# Note that there is actually a method for IsProjectiveSpace, and IsDesarguesianPlane
# is a subcategory of IsProjectiveSpace. But the property HasGraphWithUnderlyingObjectsAsVertices
# is important for IsGeneralisedPolygon. We could check in the method for projective
# spaces whether the spaces is a plane, and set the property there, but we want
# to keep polarspace.gi independent from gpolygons.gd.
###
InstallMethod( IncidenceGraph,
    "for a generalised polygon (in all possible representations",
    [ IsClassicalGQ ],
    function( gp )
        local points, lines, graph, adj, group, coll, sz;
        #if not "grape" in RecNames(GAPInfo.PackagesLoaded) then
        #    Error("You must load the GRAPE package\n");
        #fi;
        if IsBound(gp!.IncidenceGraphAttr) then
            return gp!.IncidenceGraphAttr;
        fi;
        if not HasCollineationGroup(gp) then
            Error("No collineation group computed. Please compute collineation group before computing incidence graph\,n");
        else
            points := AsList(Points(gp));
            lines := AsList(Lines(gp));
            Setter( HasGraphWithUnderlyingObjectsAsVertices )( gp, false );
            Info(InfoFinInG, 1, "Computing incidence graph of generalised polygon...");
            adj := function(x,y)
                if x!.type <> y!.type then
                    return IsIncident(x,y);
                else
                    return false;
                fi;
            end;
            group := CollineationGroup(gp);
            graph := Graph(group,Concatenation(points,lines),OnProjSubspaces,adj,true);
            Setter( IncidenceGraphAttr )( gp, graph );
            return graph;
        fi;
  end );

#############################################################################
#
#  Classical Generalised Hexagons
#  This section implements H(q) and T(q,q^3). In FinInG, these geometries
#  are nicely constructed inside polar spaces, but are also constructed
#  formally as generalised polygons, which make typical operations available
#  Both are also Lie geometries, and are hard wired embedded inside the corresponding
#  polar space. See chapter 4 of the documentation.
#
#  For both models we (have to) install methods for Wrap etc. This makes other
#  operations, like OnProjSubspaces (action function) generic. As such, we can fully
#  exploit the fact that these geometries are also Lie geometries.
#
#############################################################################

#############################################################################
#O  Wrap( <geo>, <type>, <o>  )
# returns the element of <geo> represented by <o>
##
InstallMethod( Wrap,
	"for a generalised polygon and an object",
	[IsClassicalGeneralisedHexagon, IsPosInt, IsObject],
	function( geo, type, o )
		local w;
		w := rec( geo := geo, type := type, obj := o );
		Objectify( NewType( SoPSFamily, IsElementOfIncidenceStructureRep and IsElementOfGeneralisedPolygon
			and IsSubspaceOfClassicalPolarSpace ), w );
		return w;
  end );

#############################################################################
# The fixed, hard-coded triality :-)
#############################################################################

#############################################################################
#F  SplitCayleyPointToPlane5( <el> )
# returns a list of vectors spanning the plane of W(5,q): ---, which is the
# image of the point represented by <w> under the fixed triality
###
InstallGlobalFunction( SplitCayleyPointToPlane5,
    function(w, f)
        local z, hyps, q, y, spacevec, hyp, vec, int, n;
        q := Size(f);
        #w := Unpack(elvec);
		y := w{[1..3]};
        y{[5..7]} := w{[4..6]};
        y[4] := (y[1]*y[5]+y[2]*y[6]+y[3]*y[7])^(q/2);
        y[8] := -y[4];
        z := [];
		n := Zero(f);
        z[1] := [n,y[3],-y[2],y[5],y[8],n,n,n];
        z[2] := [-y[3],n,y[1],y[6],n,y[8],n,n];
        z[3] := [y[2],-y[1],n,y[7],n,n,y[8],n];
        z[4] := [n,n,n,-y[4],y[1],y[2],y[3],n];
        z[5] := [y[4],n,n,n,n,y[7],-y[6],y[1]];
        z[6] := [n,y[4],n,n,-y[7],n,y[5],y[2]];
        z[7] := [n,n,y[4],n,y[6],-y[5],n,y[3]];
        z[8] := [y[5],y[6],y[7],n,n,n,n,-y[8]];
        z := Filtered(z,x->not IsZero(x));
        hyp := [0,0,0,1,0,0,0,1]*Z(q)^0;
        Add(z,[0,0,0,1,0,0,0,1]*Z(q)^0);
        spacevec := NullspaceMat(TransposedMat(z));
		int := IdentityMat(8,f){[1..7]};
		int[4][8] := -One(f); #could have been One(f) too, since this is only used in even char...
		vec := SumIntersectionMat(spacevec, int)[2];
        return vec{[1..3]}{[1,2,3,5,6,7]};
    end );

#############################################################################
#F  SplitCayleyPointToPlane( <elvec>, <f> )
# returns a list of vectors spanning the plane of Q(6,q): ---, which is the
# image of the point represented by <elvec> under the fixed triality
##
InstallGlobalFunction( SplitCayleyPointToPlane,
	function(elvec, f)
		local z, hyps, y, spacevec, hyp, vec, int, n;
		y := ShallowCopy(elvec);
		y[8] := -y[4];
		z := [];
		n := Zero(f);
		z[1] := [n,y[3],-y[2],y[5],y[8],n,n,n];
		z[2] := [-y[3],n,y[1],y[6],n,y[8],n,n];
		z[3] := [y[2],-y[1],n,y[7],n,n,y[8],n];
		z[4] := [n,n,n,-y[4],y[1],y[2],y[3],n];
		z[5] := [y[4],n,n,n,n,y[7],-y[6],y[1]];
		z[6] := [n,y[4],n,n,-y[7],n,y[5],y[2]];
		z[7] := [n,n,y[4],n,y[6],-y[5],n,y[3]];
		z[8] := [y[5],y[6],y[7],n,n,n,n,-y[8]];
		z := Filtered(z,x->not IsZero(x));
		hyp := [0,0,0,1,0,0,0,1]*One(f);
		Add(z,[0,0,0,1,0,0,0,1]*One(f));
		spacevec := NullspaceMat(TransposedMat(z));
		int := IdentityMat(8,f){[1..7]};
		int[4,8] := -One(f);
		vec := SumIntersectionMat(spacevec, int)[2];
		return vec{[1..3]}{[1..7]};
	end );

#############################################################################
#F  ZeroPointToOnePointsSpaceByTriality( <elvec>, <frob>, <f> )
# returns a list of vectors spanning the solid of Q+(7,q): ---, which is the
# image of the point represented by <elvec> under the fixed triality
##
InstallGlobalFunction( ZeroPointToOnePointsSpaceByTriality,
	function(elvec,frob,f)
	# elvec represents a point of T(q,q^3)
		local z, hyps, y, spacevec, n;
		n := Zero(f);
		y := elvec^frob;
		z := [];
		z[1] := [n,y[3],-y[2],y[5],y[8],n,n,n];
		z[2] := [-y[3],n,y[1],y[6],n,y[8],n,n];
		z[3] := [y[2],-y[1],n,y[7],n,n,y[8],n];
		z[4] := [n,n,n,-y[4],y[1],y[2],y[3],n];
		z[5] := [y[4],n,n,n,n,y[7],-y[6],y[1]];
		z[6] := [n,y[4],n,n,-y[7],n,y[5],y[2]];
		z[7] := [n,n,y[4],n,y[6],-y[5],n,y[3]];
		z[8] := [y[5],y[6],y[7],n,n,n,n,-y[8]];
		z := Filtered(z,x->not IsZero(x));
		spacevec := NullspaceMat(TransposedMat(z));
		return spacevec;
	end );

#############################################################################
#F  TwistedTrialityHexagonPointToPlaneByTwoTimesTriality( <elvec>, <frob>, <f> )
# elvec represents a point of T(q^3,q). elvec^frob is a one point, elvec^frob^2
# is a two point. This function computes a basis for the intersection of the
# one and two point (which are actually generators of Q+(7,q)). There intersection
# will be a plane containing the q+1 lines through elvec.
##
InstallGlobalFunction( TwistedTrialityHexagonPointToPlaneByTwoTimesTriality,
	function(elvec,frob,f)
		local z, hyps, y, pg, spacevec1, spacevec2, n;
		n := Zero(f);
		#y := Unpack(elvec)^frob;
		y := elvec^frob;
		z := [];
		z[1] := [n,y[3],-y[2],y[5],y[8],n,n,n];
		z[2] := [-y[3],n,y[1],y[6],n,y[8],n,n];
		z[3] := [y[2],-y[1],n,y[7],n,n,y[8],n];
		z[4] := [n,n,n,-y[4],y[1],y[2],y[3],n];
		z[5] := [y[4],n,n,n,n,y[7],-y[6],y[1]];
		z[6] := [n,y[4],n,n,-y[7],n,y[5],y[2]];
		z[7] := [n,n,y[4],n,y[6],-y[5],n,y[3]];
		z[8] := [y[5],y[6],y[7],n,n,n,n,-y[8]];
		z := Filtered(z,x->not IsZero(x));
		spacevec1 := NullspaceMat(TransposedMat(z));
		z := y^frob;
		y := [];
		y[1] := [n,-z[3],z[2],n,z[4],n,n,z[5]];
		y[2] := [z[3],n,-z[1],n,n,z[4],n,z[6]];
		y[3] := [-z[2],z[1],n,n,n,n,z[4],z[7]];
		y[4] := [z[5],z[6],z[7],-z[4],n,n,n,n];
		y[5] := [z[8],n,n,z[1],n,-z[7],z[6],n];
		y[6] := [n,z[8],n,z[2],z[7],n,-z[5],n];
		y[7] := [n,n,z[8],z[3],-z[6],z[5],n,n];
		y[8] := [n,n,n,n,z[1],z[2],z[3],-z[8]];
		y := Filtered(y,x->not IsZero(x));
		spacevec2 := NullspaceMat(TransposedMat(y));
		return SumIntersectionMat(spacevec1, spacevec2)[2];
	end );

#############################################################################
# Constructor operations for the Classical Generalised Hexagons.
#############################################################################

#############################################################################
#O  SplitCayleyHexagon( <f> )
# returns the split cayley hexagon over <f>
##
InstallMethod( SplitCayleyHexagon,
	"for a finite field",
	[ IsField and IsFinite ],
	function( f )
    local geo, ty, repline, reppointvect, reppoint, replinevect, dist,
	    hvm, ps, hvmform, form, nonzerof, x, w, listels, shadpoint, shadline;
    if IsOddInt(Size(f)) then
       ## the corresponding sesquilinear form here for
       ## q odd is the matrix
       ## [[0,0,0,0,1,0,0],[0,0,0,0,0,1,0],
       ## [0,0,0,0,0,0,1],[0,0,0,-2,0,0,0],
       ## [1,0,0,0,0,0,0],[0,1,0,0,0,0,0],[0,0,1,0,0,0,0]];

	   ## this is Hendrik's form
		hvm := List([1..7], i -> [0,0,0,0,0,0,0]*One(f));
		hvm{[1..3]}{[5..7]} := IdentityMat(3, f);
		hvm{[5..7]}{[1..3]} := IdentityMat(3, f);
		hvm[4][4] := -2*One(f);
		hvmform := BilinearFormByMatrix(hvm, f);
		ps := PolarSpace(hvmform);
		# UnderlyingObject will return a cvec.
		reppointvect := UnderlyingObject(RepresentativesOfElements(ps)[1]);

       ## Hendrik's canonical line is <(1,0,0,0,0,0,0), (0,0,0,0,0,0,1)>
		replinevect := [[1,0,0,0,0,0,0], [0,0,0,0,0,0,1]] * One(f);
		TriangulizeMat(replinevect);
		#ConvertToMatrixRep(replinevect, f); #is useless now.
        shadpoint := function( pt )
            local planevec, flag, plane, f;
            f := BaseField( pt );
            planevec := SplitCayleyPointToPlane( Unpack(pt!.obj), f );
            plane := VectorSpaceToElement(PG(6,f),planevec);
            flag := FlagOfIncidenceStructure(PG(6,f),[pt,plane]);
            return List(ShadowOfFlag(PG(6,f),flag,2),x->Wrap(pt!.geo,2,Unwrap(x)));
        end;

        dist := function(el1,el2)
            local x,y;
            if el1=el2 then
                return 0;
            elif el1!.type = 1 and el2!.type = 1 then
                y := VectorSpaceToElement(PG(6,f), SplitCayleyPointToPlane(Unpack(el2!.obj),f)); #PG(5,f): avoids some unnecessary checks
                if el1 in y then
                    return 2;
                else
                    x := VectorSpaceToElement(PG(6,f), SplitCayleyPointToPlane(Unpack(el1!.obj),f));
                    if ProjectiveDimension(Meet(x,y)) = 0 then
                        return 4;
                    else
                        return 6;
                    fi;
                fi;
            elif el1!.type = 2 and el2!.type = 2 then
                if ProjectiveDimension(Meet(el1,el2)) = 0 then
                    return 2;
                fi;
                x := TangentSpace(ps,el1);
                if ProjectiveDimension(Meet(x,el2)) = 0 then
                    return 4;
                else
                    return 6;
                fi;
            elif el1!.type = 1 and el2!.type = 2 then
                if el1 in el2 then
                    return 1;
                fi;
                x := VectorSpaceToElement(PG(6,f), SplitCayleyPointToPlane(Unpack(el1!.obj),f));
                if ProjectiveDimension(Meet(x,el2)) = 0 then
                    return 3;
                else
                    return 5;
                fi;
            else
                return dist(el2,el1);
            fi;
        end;

    else
       ## Here we embed the hexagon in W(5,q)
       ## Hendrik's form
		hvm := List([1..6], i -> [0,0,0,0,0,0]*One(f));
		hvm{[1..3]}{[4..6]} := IdentityMat(3, f);
		hvm{[4..6]}{[1..3]} := IdentityMat(3, f);
		hvmform := BilinearFormByMatrix(hvm, f);
		ps := PolarSpace(hvmform);
		# UnderlyingObject will return a cvec.
		reppointvect := UnderlyingObject(RepresentativesOfElements(ps)[1]);

		## Hendrik's canonical line is <(1,0,0,0,0,0), (0,0,0,0,0,1)>
		replinevect := [[1,0,0,0,0,0], [0,0,0,0,0,1]] * One(f);
		TriangulizeMat(replinevect);
		#ConvertToMatrixRep(replinevect, f); #is useless now.
        shadpoint := function( pt )
            local planevec, flag, plane, f;
            f := BaseField( pt );
            planevec := SplitCayleyPointToPlane5( Unpack(pt!.obj), f );
            plane := VectorSpaceToElement(PG(5,f),planevec);
            flag := FlagOfIncidenceStructure(PG(5,f),[pt,plane]);
            return List(ShadowOfFlag(PG(5,f),flag,2),x->Wrap(pt!.geo,2,Unwrap(x)));
        end;

        dist := function(el1,el2)
            local x,y;
            if el1=el2 then
                return 0;
            elif el1!.type = 1 and el2!.type = 1 then
                y := VectorSpaceToElement(PG(5,f), SplitCayleyPointToPlane5(Unpack(el2!.obj),f)); #PG(5,f): avoids some unnecessary checks
                if el1 in y then
                    return 2;
                else
                    x := VectorSpaceToElement(PG(5,f), SplitCayleyPointToPlane5(Unpack(el1!.obj),f));
                    if ProjectiveDimension(Meet(x,y)) = 0 then
                        return 4;
                    else
                        return 6;
                    fi;
                fi;
            elif el1!.type = 2 and el2!.type = 2 then
                if ProjectiveDimension(Meet(el1,el2)) = 0 then
                    return 2;
                fi;
                x := TangentSpace(ps,el1);
                if ProjectiveDimension(Meet(x,el2)) = 0 then
                    return 4;
                else
                    return 6;
                fi;
            elif el1!.type = 1 and el2!.type = 2 then
                if el1 in el2 then
                    return 1;
                fi;
                x := VectorSpaceToElement(PG(5,f), SplitCayleyPointToPlane5(Unpack(el1!.obj),f));
                if ProjectiveDimension(Meet(x,el2)) = 0 then
                    return 3;
                else
                    return 5;
                fi;
            else
                return dist(el2,el1);
            fi;
        end;

    fi;
	#now comes the cmatrixification of the replinevect
	replinevect := NewMatrix(IsCMatRep,f,Length(reppointvect),replinevect);

	listels := function(gp,j)
		local coll,reps;
		coll := CollineationGroup(gp);
		reps := RepresentativesOfElements( gp );
		return Enumerate(Orb(coll, reps[j], OnProjSubspaces));
	end;

	shadline := function( l )
		return List(Points(ElementToElement(AmbientSpace(l),l)),x->Wrap(l!.geo,1,x!.obj));
	end;

	#in the next line, we set the data fields for the geometry. We have to take into account that H(q) will also be
	#a Lie geometry, so it needs more data fields than a GP. But we can derive this information from ps.
	geo := rec( pointsobj := [], linesobj := [], incidence:= \*, listelements := listels, basefield := BaseField(ps),
		dimension := Dimension(ps), vectorspace := UnderlyingVectorSpace(ps), polarspace := ps,
		shadowofpoint := shadpoint, shadowofline := shadline, distance := dist);
    ty := NewType( GeometriesFamily, IsClassicalGeneralisedHexagon and IsGeneralisedPolygonRep );
    Objectify( ty, geo );
    SetAmbientSpace(geo, AmbientSpace(ps));
    SetAmbientPolarSpace(geo,ps);
    SetOrder(geo, [Size(f), Size(f)]);
    SetTypesOfElementsOfIncidenceStructure(geo, ["point","line"]);
    SetRankAttr(geo, 2);

    #now we are ready to pack the representatives of the elements, which are also elements of a polar space.
    #recall that reppointvect and replinevect are triangulized.
	#we can not count here on VectorSpaceToElement (yet). Otherwise I could have left out the "replinevect := NewMatrix(Is..." part above.
    w := rec(geo := geo, type := 1, obj := reppointvect);
    reppoint := Objectify( NewType( SoPSFamily,  IsElementOfGeneralisedPolygon and IsElementOfIncidenceStructureRep
					            	and IsSubspaceOfClassicalPolarSpace ), w );
    w := rec(geo := geo, type := 2, obj := replinevect);
    repline := Objectify( NewType( SoPSFamily, IsElementOfGeneralisedPolygon and IsElementOfIncidenceStructureRep
					            	and IsSubspaceOfClassicalPolarSpace ), w );
    SetRepresentativesOfElements(geo, [reppoint, repline]);
    #SetName(geo,Concatenation("Split Cayley Hexagon of order ", String(Size(f))));
	SetName(geo,Concatenation("H(",String(Size(f)),")"));
	return geo;
  end );

#############################################################################
#O  SplitCayleyHexagon( <q> )
# shortcut to previous method.
##
InstallMethod( SplitCayleyHexagon,
	"input is a prime power",
	[ IsPosInt ],
	function( q )
		return SplitCayleyHexagon(GF(q));
	end );

# 24/3/2014. cmat changes.
#############################################################################
#O  SplitCayleyHexagon( <ps> )
# returns the split cayley hexagon over <f>
##
InstallMethod( SplitCayleyHexagon,
	"for a classical polar space",
	[ IsClassicalPolarSpace ],
	function( ps )
    local geo, ty, repline, reppointvect, reppoint, replinevect, f, naampje, eq, dist,
	    hvm, hvmform, form, nonzerof, x, w, listels, shadpoint, shadline, change, c1, c2;
		f := BaseField(ps);
	if IsParabolicQuadric(ps) and Dimension(ps) = 6 then
		hvm := List([1..7], i -> [0,0,0,0,0,0,0]*One(f));
		hvm{[1..3]}{[5..7]} := IdentityMat(3, f);
		hvm[4][4] := -One(f);
		hvmform := QuadraticFormByMatrix(hvm, f);
		hvm := PolarSpace(hvmform);
		c1 := BaseChangeToCanonical(hvmform);
		if not IsCanonicalPolarSpace(ps) then
			c2 := BaseChangeToCanonical(QuadraticForm(ps));
			change := c1^-1*c2;
		else
			change := c1^-1;
		fi;
		# UnderlyingObject will return a cvec. We must be a bit careful: reppointvect must be normed.
		reppointvect := UnderlyingObject(RepresentativesOfElements(hvm)[1]) * change; #to be changed
		reppointvect := reppointvect / First(reppointvect,x->not IsZero(x));
		replinevect := ([[1,0,0,0,0,0,0], [0,0,0,0,0,0,1]] * One(f)) * change;
		TriangulizeMat(replinevect);

        shadpoint := function( pt )
            local planevec, flag, plane, f;
            f := BaseField( pt );
            planevec := SplitCayleyPointToPlane( Unpack(pt!.obj) * change^-1, f ) * change;
            plane := VectorSpaceToElement(PG(6,f),planevec);
            flag := FlagOfIncidenceStructure(PG(6,f),[pt,plane]);
            return List(ShadowOfFlag(PG(6,f),flag,2),x->Wrap(pt!.geo,2,Unwrap(x)));
        end;
        dist := function(el1,el2)
            local x,y;
            if el1=el2 then
                return 0;
            elif el1!.type = 1 and el2!.type = 1 then
                y := VectorSpaceToElement(PG(6,f), SplitCayleyPointToPlane(Unpack(el2!.obj) * change^-1,f) * change); #PG(6,f): avoids some unnecessary checks
                if el1 in y then
                    return 2;
                else
                    x := VectorSpaceToElement(PG(6,f), SplitCayleyPointToPlane(Unpack(el1!.obj) * change^-1,f) * change);
                    if ProjectiveDimension(Meet(x,y)) = 0 then
                        return 4;
                    else
                        return 6;
                    fi;
                fi;
            elif el1!.type = 2 and el2!.type = 2 then
                if ProjectiveDimension(Meet(el1,el2)) = 0 then
                    return 2;
                fi;
                x := TangentSpace(ps,el1);
                if ProjectiveDimension(Meet(x,el2)) = 0 then
                    return 4;
                else
                    return 6;
                fi;
            elif el1!.type = 1 and el2!.type = 2 then
                if el1 in el2 then
                    return 1;
                fi;
                x := VectorSpaceToElement(PG(6,f), SplitCayleyPointToPlane(Unpack(el1!.obj) * change^-1,f) * change);
                if ProjectiveDimension(Meet(x,el2)) = 0 then
                    return 3;
                else
                    return 5;
                fi;
            else
                return dist(el2,el1);
            fi;
        end;
    elif IsSymplecticSpace(ps) and Dimension(ps) = 5 then
       ## Here we embed the hexagon in W(5,q)
       ## Hendrik's form
		hvm := List([1..6], i -> [0,0,0,0,0,0]*One(f));
		hvm{[1..3]}{[4..6]} := IdentityMat(3, f);
		hvm{[4..6]}{[1..3]} := IdentityMat(3, f);
		hvmform := BilinearFormByMatrix(hvm, f);
		hvm := PolarSpace(hvmform);
		c1 := BaseChangeToCanonical(hvmform);
		if not IsCanonicalPolarSpace(ps) then
			c2 := BaseChangeToCanonical(SesquilinearForm(ps));
			change := c1^-1*c2;
		else
			change := c1^-1;
		fi;
		# UnderlyingObject will return a cvec. We must be a bit carefull: reppointvect must be normed.
		reppointvect := UnderlyingObject(RepresentativesOfElements(hvm)[1]) * change;
		reppointvect := reppointvect / First(reppointvect,x->not IsZero(x));

		## Hendrik's canonical line is <(1,0,0,0,0,0), (0,0,0,0,0,1)>
		replinevect := ([[1,0,0,0,0,0], [0,0,0,0,0,1]] * One(f)) * change;
		TriangulizeMat(replinevect);
		#ConvertToMatrixRep(replinevect, f); #is useless now.
        shadpoint := function( pt )
            local planevec, flag, plane, f;
            f := BaseField( pt );
            planevec := SplitCayleyPointToPlane5( Unpack(pt!.obj) * change^-1, f ) * change;
            plane := VectorSpaceToElement(PG(5,f),planevec);
            flag := FlagOfIncidenceStructure(PG(5,f),[pt,plane]);
            return List(ShadowOfFlag(PG(5,f),flag,2),x->Wrap(pt!.geo,2,Unwrap(x)));
        end;
        dist := function(el1,el2)
            local x,y;
            if el1=el2 then
                return 0;
            elif el1!.type = 1 and el2!.type = 1 then
                y := VectorSpaceToElement(PG(5,f), SplitCayleyPointToPlane5(Unpack(el2!.obj) * change^-1,f) * change); #PG(5,f): avoids some unnecessary checks
                if el1 in y then
                    return 2;
                else
                    x := VectorSpaceToElement(PG(5,f), SplitCayleyPointToPlane5(Unpack(el1!.obj) * change^-1,f) * change);
                    if ProjectiveDimension(Meet(x,y)) = 0 then
                        return 4;
                    else
                        return 6;
                    fi;
                fi;
            elif el1!.type = 2 and el2!.type = 2 then
                if ProjectiveDimension(Meet(el1,el2)) = 0 then
                    return 2;
                fi;
                x := TangentSpace(ps,el1);
                if ProjectiveDimension(Meet(x,el2)) = 0 then
                    return 4;
                else
                    return 6;
                fi;
            elif el1!.type = 1 and el2!.type = 2 then
                if el1 in el2 then
                    return 1;
                fi;
                x := VectorSpaceToElement(PG(5,f), SplitCayleyPointToPlane5(Unpack(el1!.obj) * change^-1,f) * change);
                if ProjectiveDimension(Meet(x,el2)) = 0 then
                    return 3;
                else
                    return 5;
                fi;
            else
                return dist(el2,el1);
            fi;
        end;
    else
        Error("No embedding of split Cayley hexagon possible in <ps>");
    fi;
	#now comes the cmatrixification of the reppointvect and replinevect
	reppointvect :=  NewMatrix(IsCMatRep,f,Length(reppointvect),[reppointvect])[1];
	replinevect := NewMatrix(IsCMatRep,f,Length(reppointvect),replinevect);

	listels := function(gp,j)
		local coll,reps;
		coll := CollineationGroup(gp);
		reps := RepresentativesOfElements( gp );
		return Enumerate(Orb(coll, reps[j], OnProjSubspaces));
	end;

	shadline := function( l )
		return List(Points(ElementToElement(AmbientSpace(l),l)),x->Wrap(l!.geo,1,x!.obj));
	end;

	#in the next line, we set the data fields for the geometry. We have to take into account that H(q) will also be
	#a Lie geometry, so it needs more data fields than a GP. But we can derive this information from ps.
	geo := rec( pointsobj := [], linesobj := [], incidence:= \*, listelements := listels, basefield := BaseField(ps),
		dimension := Dimension(ps), vectorspace := UnderlyingVectorSpace(ps), polarspace := ps,
		shadowofpoint := shadpoint, shadowofline := shadline, distance := dist, basechange := change);
    ty := NewType( GeometriesFamily, IsClassicalGeneralisedHexagon and IsGeneralisedPolygonRep );
    Objectify( ty, geo );
    SetAmbientSpace(geo, AmbientSpace(ps));
    SetAmbientPolarSpace(geo,ps);
    SetOrder(geo, [Size(f), Size(f)]);
    SetTypesOfElementsOfIncidenceStructure(geo, ["point","line"]);
    SetRankAttr(geo, 2);

    #now we are ready to pack the representatives of the elements, which are also elements of a polar space.
    #recall that reppointvect and replinevect are triangulized.
	#we can not count here on VectorSpaceToElement (yet). Otherwise I could have left out the "replinevect := NewMatrix(Is..." part above.
    w := rec(geo := geo, type := 1, obj := reppointvect);
    reppoint := Objectify( NewType( SoPSFamily,  IsElementOfGeneralisedPolygon and IsElementOfIncidenceStructureRep
					            	and IsSubspaceOfClassicalPolarSpace ), w );
    w := rec(geo := geo, type := 2, obj := replinevect);
    repline := Objectify( NewType( SoPSFamily, IsElementOfGeneralisedPolygon and IsElementOfIncidenceStructureRep
					            	and IsSubspaceOfClassicalPolarSpace ), w );
    SetRepresentativesOfElements(geo, [reppoint, repline]);
    #it looks a bit exaggerated to do such an effort for the name, but polar spaces do have a Name attribute set...
	if not IsCanonicalPolarSpace(ps) then
		eq := Concatenation(": ",String(EquationForPolarSpace(ps)));
	else
		eq := "";
	fi;
	if ps!.dimension = 5 then
		naampje := Concatenation("W(5, ",String(Size(f)),")",eq);
	else
		naampje := Concatenation("Q(6, ",String(Size(f)),")",eq);
	fi;
	SetName(geo,Concatenation("H(",String(Size(f)),")"," in ",naampje));
	return geo;
  end );

# 24/3/2014. cmat changes. Same principle as SplitCayleyHexagon
#############################################################################
#O  TwistedTrialityHexagon( <f> )
# returns the twisted triality hexagon over <f>
##
InstallMethod( TwistedTrialityHexagon,
	"input is a finite field",
    [ IsField and IsFinite ],
	function( f )
    local geo, ty, points, lines, repline, hvm, ps, orblen, hvmc, c, listels, dist,
          hvmform, form, q, reppoint, reppointvect, replinevect, w, shadline,
          shadpoint, frob;
       ## Field must be GF(q^3);
	frob := FrobeniusAutomorphism(f);
    q := RootInt(Size(f), 3);
	if not q^3 = Size(f) then
       Error("Field order must be a cube of a prime power");
    fi;

       ## Hendrik's form
    hvm := List([1..8], i -> [0,0,0,0,0,0,0,0]*One(f));
    hvm{[1..4]}{[5..8]} := IdentityMat(4, f);
    hvmform := QuadraticFormByMatrix(hvm, f);
    ps := PolarSpace( hvmform );

	## Hendrik's canonical point is <(1,0,0,0,0,0,0,0)>
    reppointvect := ([1,0,0,0,0,0,0,0] * One(f));
    MultVector(reppointvect,Inverse( reppointvect[PositionNonZero(reppointvect)] ));
	#ConvertToVectorRep(reppointvect, f); #useless now.

	## Hendrik's canonical line is <(1,0,0,0,0,0,0,0), (0,0,0,0,0,0,1,0)>
    replinevect := ([[1,0,0,0,0,0,0,0], [0,0,0,0,0,0,1,0]] * One(f));
	#ConvertToMatrixRep(replinevect, f); #useless now.

	listels := function(gp,j)
		local coll,reps;
		coll := CollineationGroup(gp);
		reps := RepresentativesOfElements( gp );
		return Enumerate(Orb(coll, reps[j], OnProjSubspaces));
	end;

	shadline := function( l )
		return List(Points(ElementToElement(AmbientSpace(l),l)),x->Wrap(l!.geo,1,x!.obj));
	end;

    shadpoint := function( pt )
        local planevec, flag, plane, f, frob;
        f := BaseField( pt );
        frob := FrobeniusAutomorphism(f);
        planevec := TwistedTrialityHexagonPointToPlaneByTwoTimesTriality( Unpack(pt!.obj), frob, f );
        plane := VectorSpaceToElement(PG(7,f),planevec);
        flag := FlagOfIncidenceStructure(PG(7,f),[pt,plane]);
        # we have q^3+1 lines and have to select q+1 from them. Following can probably done more efficiently,
        # but I need more mathematics then.
        return List(Filtered(ShadowOfFlag(PG(7,f),flag,2),x->x in pt!.geo), y-> Wrap(pt!.geo,2,Unwrap(y)));
    end;

    dist := function(el1,el2)
		local x,y;
        if el1 = el2 then
			return 0;
		elif el1!.type = 1 and el2!.type = 1 then
			y := VectorSpaceToElement(PG(7,f),
				TwistedTrialityHexagonPointToPlaneByTwoTimesTriality(Unpack(el2!.obj),frob,f)); #PG(7,f): avoids some unnecessary checks
			if el1 in y then
				return 2;
			else
				x := VectorSpaceToElement(PG(7,f),
					TwistedTrialityHexagonPointToPlaneByTwoTimesTriality(Unpack(el1!.obj),frob,f));
				if ProjectiveDimension(Meet(x,y)) = 0 then
					return 4;
				else
					return 6;
				fi;
			fi;
		elif el1!.type = 2 and el2!.type = 2 then
			if ProjectiveDimension(Meet(el1,el2)) = 0 then
				return 2;
			fi;
			x := TangentSpace(ps,el1);
			if ProjectiveDimension(Meet(x,el2)) = 0 then
				return 4;
			else
				return 6;
			fi;
		elif el1!.type = 1 and el2!.type = 2 then
			if el1 in el2 then
				return 1;
			fi;
			x := VectorSpaceToElement(PG(7,f),
				TwistedTrialityHexagonPointToPlaneByTwoTimesTriality(Unpack(el1!.obj),frob,f));
			if ProjectiveDimension(Meet(x,el2)) = 0 then
				return 3;
			else
				return 5;
			fi;
		else
			return dist(el2,el1);
		fi;
	end;

    geo := rec( pointsobj := [], linesobj := [], incidence:= \*, listelements := listels, shadowofpoint := shadpoint,
        shadowofline := shadline, basefield := BaseField(ps), dimension := Dimension(ps),
        vectorspace := UnderlyingVectorSpace(ps), polarspace := ps, distance := dist );
    ty := NewType( GeometriesFamily, IsClassicalGeneralisedHexagon and IsGeneralisedPolygonRep );
    Objectify( ty, geo );
    SetAmbientSpace(geo, AmbientSpace(ps));
    SetAmbientPolarSpace(geo,ps);

	#now we are ready to pack the representatives of the elements, which are also elements of a polar space.
	#recall that reppointvect and replinevect are triangulized.
	#now come the cvec/cmat ing of reppointvect and repplinevect.

	reppointvect := CVec(reppointvect,f);
   	replinevect := NewMatrix(IsCMatRep,f,Length(reppointvect),replinevect);

	w := rec(geo := geo, type := 1, obj := reppointvect);
    reppoint := Objectify( NewType( SoPSFamily, IsElementOfIncidenceStructure and IsElementOfIncidenceStructureRep and
							IsElementOfGeneralisedPolygon and IsSubspaceOfClassicalPolarSpace ), w );
    w := rec(geo := geo, type := 2, obj := replinevect);
    repline := Objectify( NewType( SoPSFamily, IsElementOfIncidenceStructure and IsElementOfIncidenceStructureRep and
	 						IsElementOfGeneralisedPolygon and IsSubspaceOfClassicalPolarSpace ), w );

    SetOrder(geo, [q^3, q]);
    SetTypesOfElementsOfIncidenceStructure(geo, ["point","line"]);
    SetRankAttr(geo, 2);
    SetRepresentativesOfElements(geo, [reppoint, repline]);
    #SetName(geo,Concatenation("Twisted Triality Hexagon of order ", String([q^3, q])));
	SetName(geo,Concatenation("T(",String(q^3),", ",String(q),")"));
   	return geo;
  end );

#############################################################################
#O  TwistedTrialityHexagon( <q> )
# shortcut to previous method.
##
InstallMethod( TwistedTrialityHexagon,
	"input is a prime power",
	[ IsPosInt ],
	function( q )
		return TwistedTrialityHexagon(GF(q));
	end );

# 24/3/2014. cmat changes. Same principle as SplitCayleyHexagon
#############################################################################
#O  TwistedTrialityHexagon( <ps> )
# returns the twisted triality hexagon over <f>
##
InstallMethod( TwistedTrialityHexagon,
	"for a classical polar space",
	[ IsClassicalPolarSpace ],
	function( ps )
    local geo, ty, points, lines, repline, hvm, orblen, hvmc, c, listels, eq, naampje,
          hvmform, form, q, reppoint, reppointvect, replinevect, w, shadline, f,
          shadpoint, c1, c2, change, dist, frob;
       ## Field must be GF(q^3);
    if not (IsHyperbolicQuadric(ps) and ps!.dimension = 7) then
        Error("No embedding of twisted triality hexagon possible in <ps>");
    fi;
    f := BaseField(ps);
	frob := FrobeniusAutomorphism(f);
    q := RootInt(Size(f), 3);
	if not q^3 = Size(f) then
       Error("Field order must be a cube of a prime power");
    fi;

       ## Hendrik's form
    hvm := List([1..8], i -> [0,0,0,0,0,0,0,0]*One(f));
    hvm{[1..4]}{[5..8]} := IdentityMat(4, f);
    hvmform := QuadraticFormByMatrix(hvm, f);
    hvm := PolarSpace(hvmform);
	c1 := BaseChangeToCanonical(hvmform);
	if not IsCanonicalPolarSpace(ps) then
		c2 := BaseChangeToCanonical(QuadraticForm(ps));
		change := c1^-1*c2;
	else
		change := c1^-1;
	fi;

	## Hendrik's canonical point is <(1,0,0,0,0,0,0,0)>
    reppointvect := ([1,0,0,0,0,0,0,0] * One(f)) * change;
    MultVector(reppointvect,Inverse( reppointvect[PositionNonZero(reppointvect)] ));
	#ConvertToVectorRep(reppointvect, f); #useless now.

	## Hendrik's canonical line is <(1,0,0,0,0,0,0,0), (0,0,0,0,0,0,1,0)>
    replinevect := ([[1,0,0,0,0,0,0,0], [0,0,0,0,0,0,1,0]] * One(f)) * change;
	TriangulizeMat(replinevect);

	#ConvertToMatrixRep(replinevect, f); #useless now.

	listels := function(gp,j)
		local coll,reps;
		coll := CollineationGroup(gp);
		reps := RepresentativesOfElements( gp );
		return Enumerate(Orb(coll, reps[j], OnProjSubspaces));
	end;

	shadline := function( l )
		return List(Points(ElementToElement(AmbientSpace(l),l)),x->Wrap(l!.geo,1,x!.obj));
	end;

    shadpoint := function( pt )
        local planevec, flag, plane, f, frob;
        f := BaseField( pt );
        frob := FrobeniusAutomorphism(f);
        planevec := TwistedTrialityHexagonPointToPlaneByTwoTimesTriality( Unpack(pt!.obj) * change^-1, frob, f ) * change;
        plane := VectorSpaceToElement(PG(7,f),planevec);
        flag := FlagOfIncidenceStructure(PG(7,f),[pt,plane]);
        # we have q^3+1 lines and have to select q+1 from them. Following can probably done more efficient,
        # but I need more mathematics then.
        return List(Filtered(ShadowOfFlag(PG(7,f),flag,2),x->x in pt!.geo), y-> Wrap(pt!.geo,2,Unwrap(y)));
    end;

	dist := function(el1,el2)
		local x,y;
        if el1 = el2 then
			return 0;
		elif el1!.type = 1 and el2!.type = 1 then
			y := VectorSpaceToElement(PG(7,f), TwistedTrialityHexagonPointToPlaneByTwoTimesTriality(
				Unpack(el2!.obj) * change^-1,frob,f) * change); #PG(7,f): avoids some unnecessary checks
			if el1 in y then
				return 2;
			else
				x := VectorSpaceToElement(PG(7,f), TwistedTrialityHexagonPointToPlaneByTwoTimesTriality(
					Unpack(el1!.obj) * change^-1,frob,f) * change);
				if ProjectiveDimension(Meet(x,y)) = 0 then
					return 4;
				else
					return 6;
				fi;
			fi;
		elif el1!.type = 2 and el2!.type = 2 then
			if ProjectiveDimension(Meet(el1,el2)) = 0 then
				return 2;
			fi;
			x := TangentSpace(ps,el1);
			if ProjectiveDimension(Meet(x,el2)) = 0 then
				return 4;
			else
				return 6;
			fi;
		elif el1!.type = 1 and el2!.type = 2 then
			if el1 in el2 then
				return 1;
			fi;
			x := VectorSpaceToElement(PG(7,f), TwistedTrialityHexagonPointToPlaneByTwoTimesTriality(
				Unpack(el1!.obj) * change^-1,frob,f) * change);
			if ProjectiveDimension(Meet(x,el2)) = 0 then
				return 3;
			else
				return 5;
			fi;
		else
			return dist(el2,el1);
		fi;
	end;

    geo := rec( pointsobj := [], linesobj := [], incidence:= \*, listelements := listels, shadowofpoint := shadpoint,
        shadowofline := shadline, basefield := BaseField(ps), dimension := Dimension(ps), basechange := change,
        vectorspace := UnderlyingVectorSpace(ps), polarspace := ps, distance := dist );
    ty := NewType( GeometriesFamily, IsClassicalGeneralisedHexagon and IsGeneralisedPolygonRep ); #change by jdb 7/12/11
    Objectify( ty, geo );
    SetAmbientSpace(geo, AmbientSpace(ps));
    SetAmbientPolarSpace(geo,ps);

	#now we are ready to pack the representatives of the elements, which are also elements of a polar space.
	#recall that reppointvect and replinevect are triangulized.
	#now come the cvec/cmat ing of reppointvect and repplinevect.

	#reppointvect := CVec(reppointvect,f);
	reppointvect :=  NewMatrix(IsCMatRep,f,Length(reppointvect),[reppointvect])[1];
    replinevect := NewMatrix(IsCMatRep,f,Length(reppointvect),replinevect);

	w := rec(geo := geo, type := 1, obj := reppointvect);
    reppoint := Objectify( NewType( SoPSFamily, IsElementOfIncidenceStructure and IsElementOfIncidenceStructureRep and
							IsElementOfGeneralisedPolygon and IsSubspaceOfClassicalPolarSpace ), w );
    w := rec(geo := geo, type := 2, obj := replinevect);
    repline := Objectify( NewType( SoPSFamily, IsElementOfIncidenceStructure and IsElementOfIncidenceStructureRep and
	 						IsElementOfGeneralisedPolygon and IsSubspaceOfClassicalPolarSpace ), w );

    SetOrder(geo, [q^3, q]);
    SetTypesOfElementsOfIncidenceStructure(geo, ["point","line"]);
    SetRepresentativesOfElements(geo, [reppoint, repline]);
    SetRankAttr(geo, 2);
    #SetName(geo,Concatenation("Twisted Triality Hexagon of order ", String([q^3, q])));
	#SetName(geo,Concatenation("T(",String(q^3),", ",String(q),")"));
   	if not IsCanonicalPolarSpace(ps) then
		eq := Concatenation(": ",String(EquationForPolarSpace(ps)));
	else
		eq := "";
	fi;
	naampje := Concatenation("Q+(7, ",String(Size(f)),")",eq);
	SetName(geo,Concatenation("T(",String(q^3),", ",String(q),")", " in ",naampje));
    return geo;
  end );

#############################################################################
#O  Display( <egq> )
#  for a classical generalised hexagon
##
InstallMethod( Display,
	"for a classical generalised hexagon",
	[ IsGeneralisedHexagon and IsLieGeometry ],
	function( gp )
		if IsHyperbolicQuadric(AmbientPolarSpace(gp)) then
            Print("Twisted triality hexagon of order ",String(Order(gp)),"\n",
                "with ambient polar space\n", AmbientPolarSpace(gp));
        else
            Print("Split Cayley hexagon of order ",String(Order(gp)),"\n",
                "with ambient polar space\n", AmbientPolarSpace(gp));
        fi;
    end );

##################################################################################
# Groups: we follow the same approach as for polar spaces. There is an operation
# that returns the groups as fining groups, which is used when needed by e.g.
# CollineationGroup( <gh> )
##################################################################################

#############################################################################
#O  G2fining( <d>, <f> )
## returns the Chevalley group G_2(q) (also known as Dickson's group.
# <f> is a finite field. For <d> equals 6, the returned group is a will be a
# subgroup of PGL(7,<f>). When <f> has characteristic 2, <d> equals 5 is allowed,
# then the returned group will be a subgroup of PGL(6,<f>).
##
InstallMethod( G2fining,
	"for a dimension and a field",
	[ IsPosInt, IsField and IsFinite ],
	function(d, f)
	local m, mp, ml, g, gens, nonzerof, newgens;
	if d=6 then
		 m:=[[ 0, 0, 0, 0, 0, 1, 0],
             [ 0, 0, 0, 0, 0, 0, 1],
             [ 0, 0, 0, 0, 1, 0, 0],
             [ 0, 0, 0, -1, 0, 0, 0],               ## **
             [ 0, 1, 0, 0, 0, 0, 0],
             [ 0, 0, 1, 0, 0, 0, 0],
             [ 1, 0, 0, 0, 0, 0, 0]]*One(f);
         mp := d ->
			[[1,  0,  0,  0,  0,  d,  0],
             [0,  1,  0,  0, -d,  0,  0],
             [0,  0,  1,  0,  0,  0,  0],
             [0,  0,  -2*d,  1,  0,  0,  0],         ## **
             [0,  0,  0,  0,  1,  0,  0],
             [0,  0,  0,  0,  0,  1,  0],
             [0,  0,d^2,  -d,  0,  0,  1]]*One(f);   ## **
         ml := d ->
			[[1, -d,  0,  0,  0,  0,  0],
             [0,  1,  0,  0,  0,  0,  0],
             [0,  0,  1,  0,  0,  0,  0],
             [0,  0,  0,  1,  0,  0,  0],
             [0,  0,  0,  0,  1,  0,  0],
             [0,  0,  0,  0,  d,  1,  0],
             [0,  0,  0,  0,  0,  0,  1]]*One(f);
		nonzerof := AsList(f){[2..Size(f)]};
		gens := Union([m], List(nonzerof, mp), List(nonzerof, ml));
        #for x in gens do
        #   ConvertToMatrixRep(x, f);
        #od;
	elif d=5 then
		if not IsEvenInt(Characteristic(f)) then
			Error("embedding of G_2(q) in PGL(6,q) is only possible for even q");
		fi;
		m:=[[ 0, 0, 0, 0, 1, 0],
		   [ 0, 0, 0, 0, 0, 1],
           [ 0, 0, 0, 1, 0, 0],
           [ 0, 1, 0, 0, 0, 0],
           [ 0, 0, 1, 0, 0, 0],
           [ 1, 0, 0, 0, 0, 0]]*One(f);
		mp := d ->
			[[1,  0,  0,  0,  d,  0],
             [0,  1,  0,  d,  0,  0],
             [0,  0,  1,  0,  0,  0],
             [0,  0,  0,  1,  0,  0],
             [0,  0,  0,  0,  1,  0],
             [0,  0,d^2,  0,  0,  1]]*One(f);
		ml := d ->
			[[1,  d,  0,  0,  0,  0],
             [0,  1,  0,  0,  0,  0],
             [0,  0,  1,  0,  0,  0],
             [0,  0,  0,  1,  0,  0],
             [0,  0,  0,  d,  1,  0],
             [0,  0,  0,  0,  0,  1]]*One(f);
		nonzerof := AsList(f){[2..Size(f)]};
		gens := Union([m], List(nonzerof,mp), List(nonzerof,ml));
	else
		Error("<d> should be 5 or 6");
	fi;
    newgens := List(gens, x -> CollineationOfProjectiveSpace(x, f));
	g := GroupWithGenerators( newgens );
	#SetCollineationAction(coll, OnProjSubspaces);
	SetName(g, Concatenation("G_2(",String(Size(f)),")") );
    return g;
  end );

#############################################################################
#O  3D4fining( <d>, <f> )
##
InstallMethod( 3D4fining,
	"for a finite field",
	[ IsField and IsFinite ],
	function(f)
		local q, pps, frob, sigma, m, mp, ml, nonzerof, nonzeroq, gens, g, newgens;
		q := RootInt(Size(f), 3);
		if not q^3 = Size(f) then
			Error("Field order must be a cube of a prime power");
		fi;
		pps := Characteristic(f);
		frob := FrobeniusAutomorphism(f);
		sigma := frob^LogInt(q,pps);
		# The generators of 3D4(q) were taken from Hendrik
    	# Van Maldeghem's book: "Generalized Polygons".

       m:=[[ 0, 0, 0, 0, 0, 1, 0, 0],
           [ 0, 0, 0, 0, 0, 0, 1, 0],
           [ 0, 0, 0, 0, 1, 0, 0, 0],
           [ 0, 0, 0, 0, 0, 0, 0, 1],
           [ 0, 1, 0, 0, 0, 0, 0, 0],
           [ 0, 0, 1, 0, 0, 0, 0, 0],
           [ 1, 0, 0, 0, 0, 0, 0, 0],
           [ 0, 0, 0, 1, 0, 0, 0, 0]]*One(f);
       #ConvertToMatrixRep(m, f);      #jdb 19/09/2018: I think this ConverToMatrixRep is not needed anymore.
       mp:=d->[[1,  0,  0,  0,  0,  d,  0,  0],
               [0,  1,  0,  0, -d,  0,  0,  0],
               [0,  0,  1,  0,  0,  0,  0,  0],
               [0,  0,  -d^sigma,  1,  0,  0,  0,  0],
               [0,  0,  0,  0,  1,  0,  0,  0],
               [0,  0,  0,  0,  0,  1,  0,  0],
               [0,0,d^sigma*d^(sigma^2),-d^(sigma^2),0,0,1,d^sigma],
               [0,0,d^(sigma^2),0,0,0,0,1]]*One(f);
       ml:=d->[[1, -d,  0,  0,  0,  0,  0,  0],
               [0,  1,  0,  0,  0,  0,  0,  0],
               [0,  0,  1,  0,  0,  0,  0,  0],
               [0,  0,  0,  1,  0,  0,  0,  0],
               [0,  0,  0,  0,  1,  0,  0,  0],
               [0,  0,  0,  0,  d,  1,  0,  0],
               [0,  0,  0,  0,  0,  0,  1,  0],
               [0,  0,  0,  0,  0,  0,  0,  1]]*One(f);

		nonzerof := AsList(f){[2..Size(f)]};
		nonzeroq := AsList(GF(q)){[2..q]};
		gens := Union([m], List(nonzerof, mp), List(nonzeroq, ml));
		newgens := List(gens, x -> CollineationOfProjectiveSpace(x,f));
		g := GroupWithGenerators(newgens);
		SetName(g, Concatenation("3D_4(",String(Size(f)),")") );
		return g;
	end );

#############################################################################
#A  CollineationGroup( <hexagon> )
# computes the collineation group of a (classical) generalised hexagon
##
InstallMethod( CollineationGroup,
	"for a generalised hexagon",
	[ IsClassicalGeneralisedHexagon],
	function( hexagon )
		local group, coll, f, gens, newgens, change, d, q, rep, domain, orblen, hom, frob, t,
		sigma, m, mp, ml, nonzerof, nonzeroq,  x;
		f := hexagon!.basefield;
		q := Size(f);
		d := hexagon!.dimension;
		if Size(Set(Order(hexagon))) > 1 then
			f := hexagon!.basefield;
       ## field must be GF(q^3);
			group := 3D4fining(f);
            if IsBound(hexagon!.basechange) then
				change := hexagon!.basechange;
				gens := List(GeneratorsOfGroup(group),x->Unpack(x!.mat));
				newgens := List(gens,x->CollineationOfProjectiveSpace(change^-1 * x * change,f));
            else
				gens := GeneratorsOfGroup(group);
				newgens := ShallowCopy(gens);
            fi;
			coll := GroupWithGenerators(newgens);
			Info(InfoFinInG, 1, "Computing nice monomorphism...");
       		t := RootInt(q, 3);
			orblen := (t+1)*(t^8+t^4+1);
			rep := RepresentativesOfElements(hexagon)[2];
			domain := Orb(coll, rep, OnProjSubspaces,
                    rec(orbsizelimit := orblen, hashlen := 2*orblen, storenumbers := true));
			Enumerate(domain);
			Info(InfoFinInG, 1, "Found permutation domain...");
			hom := OrbActionHomomorphism(coll, domain);
			SetIsBijective(hom, true);
			SetNiceObject(coll, Image(hom) );
			SetNiceMonomorphism(coll, hom );
			SetCollineationAction(coll, OnProjSubspaces);
			if not IsBound(hexagon!.basechange) then
				SetName(coll, Concatenation("3D_4(",String(Size(f)),")") );
            fi;
			return coll;
		else
			Info(InfoFinInG, 1, "for Split Cayley Hexagon");
			group := G2fining(d,f);
			if IsBound(hexagon!.basechange) then
				change := hexagon!.basechange;
				gens := List(GeneratorsOfGroup(group),x->Unpack(x!.mat));
				newgens := List(gens,x->CollineationOfProjectiveSpace(change^-1 * x * change,f));
			    if not IsPrimeInt(q) then
					frob := FrobeniusAutomorphism(f);
					Add(newgens, CollineationOfProjectiveSpace( change^-1 * IdentityMat(d+1,f) * change^(frob^-1), frob, f ));
				fi;
			else
				gens := GeneratorsOfGroup(group);
				newgens := ShallowCopy(gens);
				if not IsPrimeInt(q) then
					frob := FrobeniusAutomorphism(f);
					Add(newgens, CollineationOfProjectiveSpace( IdentityMat(d+1,f), frob, f ));
				fi;
			fi;
			coll := GroupWithGenerators(newgens);
			Info(InfoFinInG, 1, "Computing nice monomorphism...");
			orblen := (q+1)*(q^4+q^2+1);
			rep := RepresentativesOfElements(hexagon)[1];
			domain := Orb(coll, rep, OnProjSubspaces,
                  rec(orbsizelimit := orblen, hashlen := 2*orblen, storenumbers := true));
			Enumerate(domain);
			Info(InfoFinInG, 1, "Found permutation domain...");
			hom := OrbActionHomomorphism(coll, domain);
			SetIsBijective(hom, true);
			SetNiceObject(coll, Image(hom) );
			SetNiceMonomorphism(coll, hom );
			SetCollineationAction(coll, OnProjSubspaces);
			if not IsBound(hexagon!.basechange) then
				if IsPrime(Size(f)) then
					SetName(coll, Concatenation("G_2(",String(Size(f)),")") );
				else
					SetName(coll, Concatenation("G_2(",String(Size(f)),").", String(Order(frob))) );
				fi;
			fi;
			return coll;
		fi;
	end );

#############################################################################
# Incidence graph for classical generalised hexagons.
#############################################################################

#############################################################################
#O  IncidenceGraph( <gp> )
###
InstallMethod( IncidenceGraph,
    "for an ElationGQ in generic representation",
    [ IsClassicalGeneralisedHexagon and IsGeneralisedPolygonRep ],
    function( gp )
    local points, lines, graph, adj, group, coll, act,sz;
    #if not "grape" in RecNames(GAPInfo.PackagesLoaded) then
    #   Error("You must load the GRAPE package\n");
    #fi;
    if IsBound(gp!.IncidenceGraphAttr) then
       return gp!.IncidenceGraphAttr;
    fi;
    if not HasCollineationGroup(gp) then
        Error("No collineation group computed. Please compute collineation group before computing incidence graph\,n");
    else
        points := AsList(Points(gp));
        lines := AsList(Lines(gp));
        Setter( HasGraphWithUnderlyingObjectsAsVertices )( gp, false );

        Info(InfoFinInG, 1, "Computing incidence graph of generalised polygon...");

        adj := function(x,y)
            if x!.type <> y!.type then
                return IsIncident(x,y);
            else
                return false;
            fi;
        end;
		group := CollineationGroup(gp);
		act := CollineationAction(group);
		graph := Graph(group,Concatenation(points,lines),act,adj,true);
        Setter( IncidenceGraphAttr )( gp, graph );
        return graph;
    fi;
  end );

#############################################################################
# Dealing with elements: constructor operations and membership test.
#############################################################################

# Added 02/08/2014 jdb
#############################################################################
#O  VectorSpaceToElement( <geom>, <v> ) returns the elements in <geom> determined
# by the vectorspace <v>.
##
InstallMethod( VectorSpaceToElement,
	"for a polar space and a cvec",
	[ IsClassicalGeneralisedHexagon, IsCVecRep],
	function( geom, v )
		return VectorSpaceToElement(geom,Unpack(v));
	end );

#############################################################################
#O  VectorSpaceToElement( <geom>, <v> ) returns the element in <geom> determined
# by the rowvector <v>. <geom> is a generalised hexagon, so an ambient polar space
# ps is available. A point of geom is necessary a point of ps, but for T(q^3,q) we need
# to check whether the point of Q+(7,q) is absolute. This method is base on VectorSpaceToElement
# for polar spaces and a row vector.
##
InstallMethod( VectorSpaceToElement,
    "for a generalised hexagon and a row vector",
    [ IsClassicalGeneralisedHexagon, IsRowVector ],
    function(geom,vec)
		local x,y, ps, el, onespace, f, frob, change;
		## dimension should be correct
		if Length(vec) <> geom!.dimension + 1 then
			Error("Dimensions are incompatible");
		fi;
		x := ShallowCopy(vec);
		if IsZero(x) then
			return EmptySubspace(geom);
		fi;
		MultVector(x,Inverse( x[PositionNonZero(x)] ));
		y := NewMatrix(IsCMatRep,geom!.basefield,Length(x),[x])[1];
		ps := AmbientPolarSpace(geom);
		if geom!.dimension = 5 then
			return Wrap(geom, 1, y);
		elif geom!.dimension = 6 then
			if not IsSingularVector(QuadraticForm(ps),x) then
				Error("<v> does not generate an element of <geom>");
			else
				return Wrap(geom, 1, y);
			fi;
		else
			if not IsSingularVector(QuadraticForm(ps),x) then
				Error("<v> does not generate an element of <geom>");
			fi;
			f := geom!.basefield;
       		if IsBound(geom!.basechange) then
                change := geom!.basechange;
            else
                change := IdentityMat(geom!.dimension+1,f);
            fi;
			el := VectorSpaceToElement(ps,y);
			frob := FrobeniusAutomorphism(f);
			onespace := VectorSpaceToElement(AmbientSpace(ps), ZeroPointToOnePointsSpaceByTriality(x * change^-1,frob,f) * change);
			if el in onespace then
				return Wrap(geom,1,y);
			else
				Error("<v> generates an element of the ambient polar space but not of <geom>");
			fi;
		fi;
end );

#############################################################################
#O  VectorSpaceToElement( <geom>, <v> ) returns the elements in <geom> determined
# by the rowvector <v>. Several checks are built in.
##
InstallMethod( VectorSpaceToElement,
	"for a polar space and an 8-bit vector",
	[ IsClassicalGeneralisedHexagon, Is8BitVectorRep ],
    function(geom,vec)
		local x,y, ps, el, onespace, f, frob, change;
		## dimension should be correct
		if Length(vec) <> geom!.dimension + 1 then
			Error("Dimensions are incompatible");
		fi;
		x := ShallowCopy(vec);
		if IsZero(x) then
			return EmptySubspace(geom);
		fi;
		MultVector(x,Inverse( x[PositionNonZero(x)] ));
		y := NewMatrix(IsCMatRep,geom!.basefield,Length(x),[x])[1];
		ps := AmbientPolarSpace(geom);
		if geom!.dimension = 5 then
			return Wrap(geom, 1, y);
		elif geom!.dimension = 6 then
			if not IsSingularVector(QuadraticForm(ps),x) then
				Error("<v> does not generate an element of <geom>");
			else
				return Wrap(geom, 1, y);
			fi;
		else
			if not IsSingularVector(QuadraticForm(ps),x) then
				Error("<v> does not generate an element of <geom>");
			fi;
			el := VectorSpaceToElement(ps,y);
			f := geom!.basefield;
			frob := FrobeniusAutomorphism(f);
			onespace := VectorSpaceToElement(AmbientSpace(ps), ZeroPointToOnePointsSpaceByTriality(x * change^-1,frob,f) * change);
			if el in onespace then
				return Wrap(geom,1,y);
			else
				Error("<v> generates an element of the ambient polar space but not of <geom>");
			fi;
		fi;
end );

# changed 19/01/16 (jdb): by a change of IsPlistRep, this method gets also
# called when using a row vector, causing a problem with TriangulizeMat.
# a solution was to add IsMatrix.
#############################################################################
#O  VectorSpaceToElement( <geom>, <v> ) returns the elements in <geom> determined
# by the vectorspace <v>. Code based on VectorSpaceToElement for polar spaces
# and IsPlistRep.
##
InstallMethod( VectorSpaceToElement,
	"for a polar space and a Plist",
	[ IsClassicalGeneralisedHexagon, IsPlistRep and IsMatrix],
	function( geom, v )
		local  x, n, i, y, ps, f, onespace1, onespace2, p1, p2, change, frob;
		## when v is empty...
		if IsEmpty(v) then
			Error("<v> does not represent any element");
		fi;
		x := MutableCopyMat(v);
		TriangulizeMat(x);
		## dimension should be correct
		if Length(v[1]) <> geom!.dimension + 1 then
			Error("Dimensions are incompatible");
		fi;
		## Remove zero rows. It is possible the the user
		## has inputted a matrix which does not have full rank
        n := Length(x);
		i := 0;
		while i < n and ForAll(x[n-i], IsZero) do
			i := i+1;
		od;
		if i = n then
			return EmptySubspace(geom);
		fi;
		x := x{[1..n-i]};
        if Length(x) = 1 then
            return VectorSpaceToElement(geom, x[1]);
        fi;
		ps := AmbientPolarSpace(geom);
		y := NewMatrix(IsCMatRep,geom!.basefield,Length(x[1]),x);
		f := geom!.basefield;
		if IsBound(geom!.basechange) then
			change := geom!.basechange;
		else
			change := IdentityMat(geom!.dimension+1,f);
		fi;
		if geom!.dimension = 5 then
			if not IsTotallyIsotropicSubspace(SesquilinearForm(ps),x) then
				Error("<x> does not generate an element of the ambient polar space of <geom>");
			fi;
			p1 := Wrap(AmbientSpace(ps),1,y[1]);
			onespace1 := VectorSpaceToElement(AmbientSpace(ps),SplitCayleyPointToPlane5(x[2] * change^-1,f) * change);
			if p1 in onespace1 then
				return Wrap(geom,2,y);
			else
				Error("<x> does not generate an element of <geom>");
			fi;
		elif geom!.dimension = 6 then
			if not IsTotallySingularSubspace(QuadraticForm(ps),x) then
				Error("<x> does not generate an element of the ambient polar space of <geom>");
			fi;
			p1 := Wrap(AmbientSpace(ps),1,y[1]);
			onespace1 := VectorSpaceToElement(AmbientSpace(ps),SplitCayleyPointToPlane(x[2] * change^-1,f) * change);
			if p1 in onespace1 then
				return Wrap(geom,2,y);
			else
				Error("<x> does not generate an element of <geom>");
			fi;
		else
			frob := FrobeniusAutomorphism(f);
			if not IsTotallySingularSubspace(QuadraticForm(ps),x) then
				Error("<x> does not generate an element of the ambient polar space of <geom>");
			fi;
			p1 := Wrap(AmbientSpace(ps),1,y[1]);
			onespace1 := VectorSpaceToElement(AmbientSpace(ps),
					ZeroPointToOnePointsSpaceByTriality(x[1] * change^-1,frob,f) * change);
			p2 := Wrap(AmbientSpace(ps),1,y[2]);
			onespace2 := VectorSpaceToElement(AmbientSpace(ps),
					ZeroPointToOnePointsSpaceByTriality(x[2] * change^-1,frob,f) * change);
			if p1 in onespace1 and p2 in onespace2 and p2 in onespace1 then
				return Wrap(geom,2,y);
			else
				Error("<x> does not generate an element of <geom>");
			fi;
		fi;
	end );

#############################################################################
#O  VectorSpaceToElement( <geom>, <v> )
# if mat is in IsGF2MatrixRep, then Unpack(mat) is in IsPlistRep.
##
InstallMethod( VectorSpaceToElement,
	"for a polar space and a compressed GF(2)-matrix",
	[ IsClassicalGeneralisedHexagon, IsGF2MatrixRep],
	function(geom, mat)
		return VectorSpaceToElement(geom,Unpack(mat));
	end );

#############################################################################
#O  VectorSpaceToElement( <geom>, <v> )
# if mat is in Is8BitMatrixRep, then Unpack(mat) is in IsPlistRep.
##
InstallMethod( VectorSpaceToElement,
	"for a polar space compressed basis of a vector subspace",
	[ IsClassicalGeneralisedHexagon, Is8BitMatrixRep],
	function(geom, mat)
		return VectorSpaceToElement(geom,Unpack(mat));
	end );

#############################################################################
#O  VectorSpaceToElement( <geom>, <v> ) returns the elements in <geom> determined
# by the vectorspace <v>.
##
InstallMethod( VectorSpaceToElement,
	"for a polar space and a cmat",
	[ IsClassicalGeneralisedHexagon, IsCMatRep],
	function( geom, v )
		return VectorSpaceToElement(geom,Unpack(v));
	end );

#############################################################################
#O  \in( <w>, <gp> ) true if the element <w> is contained in <gp>
# We can base this method on VectorSpaceToElement, some checks can be avoided since we
# start from an element already.
InstallMethod( \in,
	"for an element of an incidencestructure a classical generalised hexagon",
	[ IsElementOfIncidenceStructure, IsClassicalGeneralisedHexagon ],
	function( el, gp )
        local vec, onespace, f, frob, p1, p2, onespace1, onespace2, change;
        vec := Unpack(UnderlyingObject(el));
        if not AmbientSpace(el) = AmbientSpace(gp) then
            return false;
        fi;
        if el!.type = 1 then #dealing with a point
            if gp!.dimension = 5 then
                return true;
            elif gp!.dimension = 6 then
                if not IsSingularVector(QuadraticForm(AmbientPolarSpace(gp)),vec) then
                    return false;
                else
                    return true;
                fi;
            else
                if not IsSingularVector(QuadraticForm(AmbientPolarSpace(gp)),vec) then
                    return false;
                fi;
                f := gp!.basefield;
                if IsBound(gp!.basechange) then
                    change := gp!.basechange;
                else
                    change := IdentityMat(gp!.dimension+1,f);
                fi;
                frob := FrobeniusAutomorphism(f);
                #onespace := VectorSpaceToElement(AmbientSpace(gp), ZeroPointToOnePointsSpaceByTriality(vec,frob,f));
                onespace := VectorSpaceToElement(AmbientSpace(gp), ZeroPointToOnePointsSpaceByTriality(vec * change^-1,frob,f) * change);
                if el in onespace then
                    return true;
                else
                    return false;
                fi;
            fi;
        elif el!.type = 2 then #dealing with a line
            f := gp!.basefield;
            if IsBound(gp!.basechange) then
                change := gp!.basechange;
            else
                change := IdentityMat(gp!.dimension+1,f);
            fi;
            if gp!.dimension = 5 then
                if not IsTotallyIsotropicSubspace(SesquilinearForm(AmbientPolarSpace(gp)),vec) then
                    return false;
                fi;
                p1 := VectorSpaceToElement(AmbientSpace(gp),vec[1]);
                onespace1 := VectorSpaceToElement(AmbientSpace(gp),SplitCayleyPointToPlane5(vec[2] * change^-1,f) * change);
                if el in onespace1 then
                    return true;
                else
                    return false;
                fi;
            elif gp!.dimension = 6 then
                if not IsTotallySingularSubspace(QuadraticForm(AmbientPolarSpace(gp)),vec) then
                    return false;
                fi;
                p1 := VectorSpaceToElement(AmbientSpace(gp),vec[1]);
                onespace1 := VectorSpaceToElement(AmbientSpace(gp),SplitCayleyPointToPlane(vec[2] * change^-1,f) * change);
                if p1 in onespace1 then
                    return true;
                else
                    return false;
                fi;
            else
                frob := FrobeniusAutomorphism(f);
                if not IsTotallySingularSubspace(QuadraticForm(AmbientPolarSpace(gp)),vec) then
                    return false;
                fi;
                p1 := VectorSpaceToElement(AmbientSpace(gp),vec[1]);
                onespace1 := VectorSpaceToElement(AmbientSpace(gp),
					ZeroPointToOnePointsSpaceByTriality(vec[1] * change^-1,frob,f) * change);
                p2 := VectorSpaceToElement(AmbientSpace(gp),vec[2]);
                onespace2 := VectorSpaceToElement(AmbientSpace(gp),
					ZeroPointToOnePointsSpaceByTriality(vec[2] * change^-1,frob,f) * change);
                if p1 in onespace1 and p2 in onespace2 and p2 in onespace1 then
                    return true;
                else
                    return false;
                fi;
            fi;
        else
            return false;
        fi;
    end );

#############################################################################
#O  ObjectToElement( <geom>, <v> ) returns the elements in <geom> determined
# by the object <obj>. This is of course just a shortcut to VectorSpaceToElement.
##
InstallMethod( ObjectToElement,
	"for a polar space and a cmat",
	[ IsClassicalGeneralisedHexagon, IsObject],
	function( geom, obj )
		return VectorSpaceToElement(geom,obj);
	end );

#############################################################################
#
# Elation generalised quadrangles.
#
#############################################################################

#############################################################################
#
#  Kantor Families and associated EGQ's
#
#  The setup is a bit different than the standard one.
#  IsElationGQ is a subcategory of IsGeneralisedQuadrangle. IsElationGQByKantorFamily
#  is a subcategory of IsElationGQ. For historical (?) reasons, the elements of a
#  IsElationGQByKantorFamily-GP are in a category called IsElementOfKantorFamily,
#  which is a subcategory of IsElementOfGeneralisedPolygon. Contrary to all other
#  instances of elements of an incidence geometry, an IsElementOfKantorFamily contains
#  also its class (and of course its embient geometry, type and underlying object).
#  This makes typical operations like Wrap different. The same applies to UnderlyingObject
#  and ObjectToElement for IsElationGQByKantorFamily
#
#   components in the EGQByKantor (different from the standard ones for GPs);
#	pointsobj, linesobj = []
#	group: contains the elation group.
#	S, Sstar: contains the two collections of subgroups. Note that g,S,Sstar determine the EGQ.
#
#############################################################################

#############################################################################
# Very particular display methods.
#############################################################################

InstallMethod( ViewObj,
	"for an element of a Kantor family",
	[ IsElementOfKantorFamily ],
	function( v )
		if v!.type = 1 then
			Print("<a point of class ", v!.class," of ",v!.geo,">");
		else
			Print("<a line of class ", v!.class," of ",v!.geo,">");
		fi;
	end );

InstallMethod( PrintObj,
	"for an element of a Kantor family",
	[ IsElementOfKantorFamily ],
	function( v )
		if v!.type = 1 then
			Print("<a point of class ", v!.class," of ",v!.geo,">\n");
		else
			Print("<a line of class ", v!.class," of ",v!.geo,">\n");
		fi;
		Print(v!.obj);
	end );

#############################################################################
#O  Wrap( <geo>, <type>, <class>, <o>  )
# returns the element of <geo> represented by <o>
##
InstallMethod( Wrap,
	"for an EGQ (Kantor family), and an object",
	[IsElationGQByKantorFamily, IsPosInt, IsPosInt, IsObject],
	function( geo, type, class, o )
		local w;
		w := rec( geo := geo, type := type, class := class, obj := o );
		Objectify( NewType( ElementsOfIncidenceStructureFamily,   #ElementsFamily,
			IsElementOfKantorFamilyRep and IsElementOfKantorFamily ), w );
		return w;
	end );

#############################################################################
#O  \=( <a>, <b>  )
# for thwo elements of a Kantor family.
##
InstallMethod( \=,
	"for two elements of a Kantor family",
	[IsElementOfKantorFamily, IsElementOfKantorFamily],
	function( a, b )
		return a!.obj = b!.obj;
	end );

#############################################################################
#O  \<( <a>, <b>  )
# for thwo elements of a Kantor family.
##
InstallMethod( \<,
	"for two elements of a Kantor family",
	[IsElementOfKantorFamily, IsElementOfKantorFamily],
	function( a, b )
		if a!.type <> b!.type then return a!.type < b!.type;
		else
			if a!.class <> b!.class then return a!.class < b!.class;
			else return a!.obj < b!.obj;
			fi;
		fi;
	end );

#############################################################################
#O  \<( <a>, <b>  )
# for a group and two lists.
##
InstallMethod( IsKantorFamily,
	"for a group and two lists of subgroups",
	[IsGroup, IsList, IsList],
	function( g, f, fstar)
		local flag, a, b, c, ab, astar, tplus1;
		tplus1 := Size(f);
		flag := true;
		if not ForAll([1..Size(fstar)], x -> IsSubgroup(fstar[x], f[x])) then
			Error( "second and third arguments are incompatile");
		fi;
		if not ForAll(fstar, x -> IsSubgroup(g, x)) then
			Error( "elements of second argument are not subgroups of first argument" );
		fi;

		Info(InfoFinInG, 1, "Checking tangency condition...");

		## K2 tangency condition
		for a in [1..tplus1-1] do
			for astar in [a+1..tplus1] do
				flag := IsTrivial(Intersection(fstar[astar], f[a]));
				if not flag then
					Print("Failed tangency condition\n");
					Print(a,"  ",astar,"\n");
					return flag;
				fi;
			od;
		od;

		Info(InfoFinInG, 1, "Checking triple condition...");

		## K1 triple condition
		for a in [1..tplus1-2] do
			for b in [a+1..tplus1-1] do
				for c in [b+1..tplus1] do
					ab := DoubleCoset(f[a], One(g), f[b]);
					flag := IsTrivial(Intersection(ab, f[c]));
					if not flag then
						Print("Failed triple condition\n");
						Print(a,"  ",b,"  ",c,"\n");
						return flag;
					fi;
				od;
			od;
		od;
		return flag;
	end );

#############################################################################
#F  OnKantorFamily( <v>, <el> ): action function for elation groups on elements.
##
InstallGlobalFunction( OnKantorFamily,
  function( v, el )
    local geo, type, class, new;
    geo := v!.geo;
    type := v!.type;
    class := v!.class;
    if (type = 1 and class = 3) or (type = 2 and class = 2) then
       return v;
    elif (type = 1 and class = 2) or (type = 2 and class = 1) then
       new := [v!.obj[1], CanonicalRightCosetElement(v!.obj[1],v!.obj[2]*el)];
    else
       new := OnRight(v!.obj, el);
    fi;
    return Wrap(geo, type, class, new);
  end );

#############################################################################
#O  EGQByKantorFamily(<g>, <f>, <fstar>)
# for a group and two lists.
# Note: for this model, the underlying objects are IsElementOfKantorFamily.
##
InstallMethod( EGQByKantorFamily,
	"for a group, a list and a list",
	[IsGroup, IsList, IsList],
	function( g, f, fstar)
    local basepoint, inc, listels, shadpoint, shadline,
          x, y, geo, ty, points, lines, pointreps, linereps, dist, spanpts, meetlns;
    ## Some checks first.
    ## We do not check if it's a Kantor family though (this can be rather slow)

    if not ForAll([1..Size(fstar)], x -> IsSubgroup(fstar[x], f[x])) then
       Error( "second and third arguments are incompatible");
       return;
    fi;
    if not ForAll(fstar, x -> IsSubgroup(g, x)) then
       Error( "elements of second argument are not subgroups of first argument" );
       return;
    fi;

    inc := function(x, y)
        if x!.type = y!.type then
            return x = y;
        elif x!.type = 1 and y!.type = 2 then
            if x!.class = 1 and y!.class = 1 then
                return x!.obj*y!.obj[2]^-1 in y!.obj[1];
            elif x!.class = 2 and y!.class = 1 then
                return IsSubset(x!.obj[1], RightCoset(y!.obj[1], y!.obj[2]*x!.obj[2]^-1));
            elif x!.class = 2 and y!.class = 2 then
                return x!.obj[1] = y!.obj;
            elif x!.class = 3 and y!.class = 2 then
                return true;
            else return false;
            fi;
        else
            return inc(y, x);
        fi;
    end;

    geo := rec( pointsobj := [], linesobj := [], incidence := inc, group := g, S := f, Sstar := fstar );
    ty := NewType( GeometriesFamily, IsElationGQByKantorFamily and IsGeneralisedPolygonRep );
    Objectify( ty, geo );

	#obvious information on this gp:
    basepoint := Wrap(geo, 1, 3, 0);
    SetBasePointOfEGQ( geo, basepoint );
    SetOrder(geo, [Index(g,fstar[1]), Size(f)-1]);
    SetCollineationAction(g, OnKantorFamily);
    SetElationGroup(geo, g);
    SetTypesOfElementsOfIncidenceStructure(geo, ["point","line"]);
    pointreps := Concatenation( [Wrap(geo,1,1,One(g))], Set(fstar, b -> Wrap(geo,1,2,[b, One(b)])), [basepoint]);
    linereps := Concatenation(Set(f, a -> Wrap(geo,2,1,[a, One(g)])), Set(fstar, x -> Wrap(geo, 2, 2, x)));
    SetRepresentativesOfElements(geo, [pointreps,linereps]);
    SetName(geo,Concatenation("<EGQ of order ",String([Index(g,fstar[1]), Size(f)-1])," and basepoint 0>"));

    listels := function(gp,i)
        local pts1,pts2,pts3,ls1,ls2;
        if i=1 then
            pts1 := List(gp!.group, x -> Wrap(geo,1,1,x));
            pts2 := Set(gp!.Sstar, b -> Set(RightCosets(g,b),x->[b,CanonicalRightCosetElement(b, Representative(x))]));
            pts2 := Concatenation(pts2);
            pts2 := List(pts2, x -> Wrap(geo,1,2,x));
            pts3 := BasePointOfEGQ(gp);
            return Concatenation(pts1,pts2,[pts3]);
        elif i=2 then
            ls1 := Set(gp!.S, a -> Set(RightCosets(g,a), x -> [a,CanonicalRightCosetElement(a, Representative(x))]));
            ls1 := Concatenation(ls1);
            ls1 := List(ls1, x -> Wrap(geo,2,1,x)); ## this is a strictly sorted list
            ls2 := Set(gp!.Sstar, x -> Wrap(geo, 2, 2, x));
            return Concatenation(ls1, ls2);
        fi;
    end;

	shadpoint := function(pt)
		local cosets, objs, Ss, S, g, lns;
		if pt!.class = 1 then
			cosets := List(f,x->RightCoset(x,pt!.obj));
			objs := List(cosets,x->[ActingDomain(x),CanonicalRightCosetElement(ActingDomain(x),Representative(x))]);
			return List(objs,x->Wrap(pt!.geo,2,1,x));
		elif pt!.class = 2 then
			Ss := pt!.obj[1];
			g := pt!.obj[2];
			S := f[Position(fstar,Ss)];
			cosets := RightCosets(Ss,S);
			objs := List(cosets,x->[ActingDomain(x),CanonicalRightCosetElement(ActingDomain(x),Representative(x))]);
			cosets := List(objs,x->RightCoset(x[1],x[2]*g));
			objs := List(cosets,x->[ActingDomain(x),CanonicalRightCosetElement(ActingDomain(x),Representative(x))]);
			lns := List(objs,x->Wrap(pt!.geo,2,1,x));
			Add(lns,Wrap(pt!.geo,2,2,Ss));
			return lns;
		else
			return List(fstar,x->Wrap(pt!.geo,2,2,x));
		fi;
	end;

	shadline := function(line)
		local coset,cosets,Ss,pts,objs;
		if line!.class = 1 then
			coset := RightCoset(line!.obj[1],line!.obj[2]);
			pts := List(List(coset),x->Wrap(line!.geo,1,1,x));
			Ss := fstar[Position(f,line!.obj[1])];
			coset := RightCoset(Ss,line!.obj[2]);
			Add(pts,Wrap(line!.geo,1,2,
						[ActingDomain(coset),CanonicalRightCosetElement(ActingDomain(coset),Representative(coset))]));
			return pts;
		elif line!.class = 2 then
			cosets := RightCosets(g,line!.obj);
			objs := List(cosets,x->[ActingDomain(x),CanonicalRightCosetElement(ActingDomain(x),Representative(x))]);
			pts := List(objs,x->Wrap(line!.geo,1,2,x));
			Add(pts,BasePointOfEGQ(line!.geo));
			return pts;
		fi;
	end;

    spanpts := function(p,q)
        local class, obj, geo, list, S, coset, r;
        if not p!.geo = q!.geo then
            Error("<p> and <q> should belong to the same ambient geometry");
        fi;
        if p = q then
            return fail;
        fi;
        if p!.class = 1 and q!.class = 1 then
            S := First(f,x->p!.obj*q!.obj^-1 in x);
            if S = fail then
                return fail;
            else
                coset := RightCoset(S,p!.obj);
                r := CanonicalRightCosetElement(S,Representative(coset));
                return Wrap(p!.geo,2,1,[S,r]);
            fi;
        elif p!.class = 1 and q!.class = 2 then
            if p!.obj in RightCoset(q!.obj[1],q!.obj[2]) then
                S := First(f,x->IsSubgroup(q!.obj[1],x));
                coset := RightCoset(S,p!.obj);
                r := CanonicalRightCosetElement(S,Representative(coset));
                return Wrap(p!.geo,2,1,[S,r]);
            else
                return fail;
            fi;
        elif p!.class = 1 and q!.class = 3 then
            return fail;
        elif p!.class = 2 and q!.class = 2 then
            if p!.obj[1] = q!.obj[1] then
                return Wrap(p!.geo,2,2,p!.obj[1]);
            else
                return fail;
            fi;
        elif p!.class = 2 and q!.class = 3 then
            return Wrap(p!.geo,2,2,p!.obj[1]);
        else
            return spanpts(q,p);
        fi;
    end;

    meetlns := function(l,m)
        local class, obj, geo, Sl, Sm, S, coset, r, g, h;
        if not l!.geo = m!.geo then
            Error("<l> and <m> should belong to the same ambient geometry");
        fi;
        if l = m then
            return fail;
        fi;
        if l!.class = 1 and m!.class = 1 then
            Sl := l!.obj[1];
            Sm := m!.obj[1];
            if Sl = Sm then
                S := First(fstar, x->IsSubgroup(x,Sl));
                coset := RightCoset(S,l!.obj[2]);
                r := CanonicalRightCosetElement(S,Representative(coset));
                return Wrap(l!.geo,1,2,[S,r]);
            else
                g := l!.obj[2];
                h := m!.obj[2];
                r := Intersection(RightCoset(Sl,g),RightCoset(Sm,h));
                if Length(r) = 0 then
                    return fail;
                else
                    return Wrap(l!.geo,1,1,r[1]);
                fi;
            fi;
        elif l!.class = 1 and m!.class = 2 then
            if IsSubgroup(m!.obj,l!.obj[1]) then
                coset := RightCoset(m!.obj,l!.obj[2]);
                r := CanonicalRightCosetElement(m!.obj,Representative(coset));
                return Wrap(l!.geo,1,2,[m!.obj,r]);
            else
                return fail;
            fi;
        elif l!.class = 2 and m!.class = 2 then
            return Wrap(l!.geo,1,3,0);
        else
            return meetlns(m,l);
        fi;
    end;

    dist := function(x,y)
        if x!.type <> y!.type then
            if inc(x,y) then
                return 1;
            else
                return 3;
            fi;
        elif x!.type = 1 then
            if spanpts(x,y) = fail then
                return 4;
            else
                return 2;
            fi;
        else
            if meetlns(x,y) = fail then
                return 4;
            else
                return 2;
            fi;
        fi;
    end;

	geo!.listelements := listels;
	geo!.shadowofpoint := shadpoint;
	geo!.shadowofline := shadline;
    geo!.span := spanpts;
    geo!.meet := meetlns;
    geo!.distance := dist;
	return geo;
  end );


#############################################################################
#O  Display( <egq> )
#  for an elation GQ by Kantor family
##
InstallMethod( Display,
	"for an elation GQ by Kantor family",
	[ IsElationGQByKantorFamily ],
	function( gp )
        Print("Elation generalised quadrangle of order ",String(Order(gp)),"\n",
                "with elation group\n");
        View(gp!.group);
    end );

#############################################################################
#O  UnderlyingObject( <el>)
# for IsElementOfKantorFamily.
##
InstallMethod( UnderlyingObject,
    "for an element of a Kantor family",
    [ IsElementOfKantorFamily ],
    function( el )
        if el!.type = 1 then
            if el!.class = 2 then
                return RightCoset(el!.obj[1],el!.obj[2]);
            else
                return el!.obj;
            fi;
        else
            if el!.class = 1 then
                return RightCoset(el!.obj[1],el!.obj[2]);
            else
                return el!.obj;
            fi;
        fi;
    end );

#############################################################################
#O  ObjectToElement( <egq>, <t>, <obj>)
#  for an EGQByKantorFamily, a type and a right coset
##
InstallMethod( ObjectToElement,
    "for an EGQByKantorFamily, a type and a right coset",
    [ IsElationGQByKantorFamily, IsPosInt, IsRightCoset ],
    function( geo, t, obj )
        local S,r;
        S := ActingDomain(obj);
        r := CanonicalRightCosetElement(S,Representative(obj));
        if t = 1 then
            if not S in geo!.Sstar then
                Error(" <obj> does not represent a point of <geo>");
            else
                return Wrap(geo,t,2,[S,r]);
            fi;
        elif t = 2 then
            if not S in geo!.S then
                Error(" <obj> does not represent a line of <geo>");
            else
                return Wrap(geo,t,1,[S,r]);
            fi;
        else
            Error("<geo> has no elements other than points and lines");
        fi;
    end );

#############################################################################
#O  ObjectToElement( <egq>, <t>, <obj>)
#  for an EGQByKantorFamily, a type and a right coset
##
InstallMethod( ObjectToElement,
    "for an EGQByKantorFamily, a type and a right coset",
    [ IsElationGQByKantorFamily, IsRightCoset ],
    function( geo, obj )
        local S,r;
        S := ActingDomain(obj);
        r := CanonicalRightCosetElement(S,Representative(obj));
        if S in geo!.Sstar then
            return Wrap(geo,1,2,[S,r]);
        elif S in geo!.S then
            return Wrap(geo,2,1,[S,r]);
        else
            Error("<obj> does not represent any element of <geo>");
        fi;
    end );

#############################################################################
#O  ObjectToElement( <egq>, <t>, <obj>)
#  for an EGQByKantorFamily, a type and a group element
##
InstallMethod( ObjectToElement,
    "for an EGQByKantorFamily, a type and a group element",
    [ IsElationGQByKantorFamily, IsPosInt, IsMultiplicativeElementWithInverse ],
    function( geo, t, obj )
        if not t = 1 or not obj in geo!.group then
            Error("<obj> does not represent a point");
        else
            return Wrap(geo,1,1,obj);
        fi;
    end );

#############################################################################
#O  ObjectToElement( <egq>, <obj>)
#  for an EGQByKantorFamily and a group element
##
InstallMethod( ObjectToElement,
    "for an EGQByKantorFamily and a group element",
    [ IsElationGQByKantorFamily, IsMultiplicativeElementWithInverse ],
    function( geo, obj )
        if not obj in geo!.group then
            Error("<obj> does not represent a point");
        else
            return Wrap(geo,1,1,obj);
        fi;
    end );

#############################################################################
#O  ObjectToElement( <egq>, <t>, <obj>)
#  for an EGQByKantorFamily, a type and a group element
##
InstallMethod( ObjectToElement,
    "for an EGQByKantorFamily, a type and a group",
    [ IsElationGQByKantorFamily, IsPosInt, IsMagmaWithInverses ],
    function( geo, t, obj )
        if not t = 2 or not obj in geo!.Sstar then
            Error("<obj> does not represent a line");
        else
            return Wrap(geo,2,2,obj);
        fi;
    end );

#############################################################################
#O  ObjectToElement( <egq>, <obj>)
#  for an EGQByKantorFamily and a group element
##
InstallMethod( ObjectToElement,
    "for an EGQByKantorFamily and a group",
    [ IsElationGQByKantorFamily, IsMagmaWithInverses ],
    function( geo, obj )
        if not obj in geo!.Sstar then
            Error("<obj> does not represent a line");
        else
            return Wrap(geo,2,2,obj);
        fi;
    end );

#############################################################################
#
#   q-Clans and EGQ's made from them
#
#############################################################################

#############################################################################
#O  IsAnisotropic( <m>, <f> )
# simply checks if a matrix is anisotropic.
##
InstallMethod( IsAnisotropic,
	"for a matrix and a finite field",
	[IsFFECollColl,  IsField and IsFinite],
	function( m, f )
		local pairs, o;
		o := Zero(f);
		pairs := Difference(AsList(f^2),[[o,o]]);
		return ForAll(pairs, x -> x * m * x <> o);
	end );

#############################################################################
#O  IsqClan( <list>, <f> )
# simply checks if all differences of elements in a set of 2 x 2 matrices is are
# anisotropic.
##
InstallMethod( IsqClan,
	"input are 2x2 matrices",
    [ IsFFECollCollColl,  IsField and IsFinite],
	function( clan, f )
		return ForAll(Combinations(clan,2), x -> IsAnisotropic(x[1]-x[2], f));
	end );

#############################################################################
#O  qClan( <clan>, <f> )
# returns a qClan object from a suitable list of matrices.
##
InstallMethod( qClan,
	"for a list of 2x2 matrices",
	[ IsFFECollCollColl, IsField ],
	function( m, f )
		local qclan;
		## test to see if it is a qClan
		if not ForAll(Combinations(m, 2), x -> IsAnisotropic(x[1]-x[2], f)) then
			Error("These matrices do not form a q-clan");
		fi;
		qclan := rec( matrices := m, basefield := f );
		Objectify( NewType( qClanFamily, IsqClanObj and IsqClanRep ), qclan );
		return qclan;
	end );

InstallMethod( ViewObj,
	"for a qClan",
	[ IsqClanObj and IsqClanRep ],
	function( x )
		Print("<q-clan over ",x!.basefield,">");
	end );

InstallMethod( PrintObj,
	"for a qClan",
	[ IsqClanObj and IsqClanRep ],
	function( x )
		Print("qClan( ", x!.matrices, ", ", x!.basefield , ")");
	end );

#############################################################################
#O  AsList( <clan> )
# returns the matrices defining a qClan
##
InstallOtherMethod( AsList,
	"for a qClan",
	[IsqClanObj and IsqClanRep],
	function( qclan )
		return qclan!.matrices;
	end );

#############################################################################
#O  AsList( <clan> )
# returns the matrices defining a qClan
##
InstallOtherMethod( AsSet,
	"for a qClan",
	[IsqClanObj and IsqClanRep],
	function( qclan )
		return Set(qclan!.matrices);
	end );

#############################################################################
#O  BaseField( <clan> )
# returns the BaseField of the qClan
##
InstallMethod( BaseField,
	"for a qClan",
	[IsqClanObj and IsqClanRep],
	function( qclan )
		return qclan!.basefield;
	end );

#############################################################################
#O  BaseField( <clan> )
# checks if the qClan is linear.
##
InstallMethod( IsLinearqClan,
	"for a qClan",
	[ IsqClanObj ],
	function( qclan )
		local blt;
		blt := BLTSetByqClan( qclan );
		return ProjectiveDimension(Span(blt)) = 2;
	end );

#############################################################################
#O  LinearqClan( <clan> )
# returns a linear qClan.
##
InstallMethod( LinearqClan,
	"for a prime power",
	[ IsPosInt ],
	function(q)
		local f, g, clan, n;
		if not IsPrimePowerInt(q) then
			Error("Argument must be a prime power");
		fi;
		n := First(GF(q), t -> not IsZero(t) and LogFFE(t, Z(q)^2) = fail);
		if n = fail then
			Error("Couldn't find nonsquare");
		fi;
		f := t -> 0 * Z(q)^0;
		g := t -> -n * t;
		clan := List(GF(q), t -> [[t, f(t)], [f(t), g(t)]]);
		clan := qClan(clan, GF(q));
		SetIsLinearqClan(clan, true);
		return clan;
	end );

#############################################################################
#O  FisherThasWalkerKantorBettenqClan( <q> )
# returns the Fisher Thas Walker Kantor Betten qClan
##
InstallMethod( FisherThasWalkerKantorBettenqClan,
	"for a prime power",
	[ IsPosInt ],
	function(q)
		local f, g, clan;
		if not IsPrimePowerInt(q) then
			Error("Argument must be a prime power");
		fi;
		if q mod 3 <> 2 then
			Error("q must be congruent to 2 mod (3)");
		fi;
		f := t -> 3/2 * t^2;
		g := t -> 3 * t^3;
		clan := List(GF(q), t -> [[t, f(t)], [f(t), g(t)]]);
		return qClan(clan, GF(q));
	end );

#############################################################################
#O  KantorMonomialqClan( <q> )
# returns the Kantor Monomial qClan
##
InstallMethod( KantorMonomialqClan,
	"for a prime power",
	[ IsPosInt ],
	function(q)
		local f, g, clan;
		if not IsPrimePowerInt(q) then
			Error("Argument must be a prime power");
		fi;
		if not q mod 5 in [2, 3] then
			Error("q must be congruent to 2 mod (3)");
		fi;
		f := t -> 5/2 * t^3;
		g := t -> 5 * t^5;
		clan := List(GF(q), t -> [[t, f(t)], [f(t), g(t)]]);
	return qClan(clan, GF(q));
end );

#############################################################################
#O  KantorKnuthqClan( <q> )
# returns the Kantor Knuth qClan
##
InstallMethod( KantorKnuthqClan,
	"for a prime power",
	[ IsPosInt ],
	function(q)
		local f, g, clan, n, sigma;
		if not IsPrimePowerInt(q) then
			Error("Argument must be a prime power");
		fi;
		if IsPrime(q) then
			Error("q is a prime");
		fi;
		sigma := FrobeniusAutomorphism(GF(q));
		n := First(GF(q), t -> not IsZero(t) and LogFFE(t, Z(q)^2) = fail);
		if n = fail then
			Error("Couldn't find nonsquare");
		fi;
		f := t -> 0 * Z(q)^0;
		g := t -> -n * t^sigma;
		clan := List(GF(q), t -> [[t, f(t)], [f(t), g(t)]]);
		return qClan(clan, GF(q));
	end );

#############################################################################
#O  FisherqClan( <q> )
# returns the Fisher qClan
##
InstallMethod( FisherqClan,
	"for a prime power",
	[ IsPosInt ],
	function(q)
		local f, g, clan, n, zeta, omega, squares, nonsquares, i, z, a, t, j;
		if not IsPrimePowerInt(q) or IsEvenInt(q) then
			Error("Argument must be an odd prime power");
    fi;
	squares := ShallowCopy(AsList(Group(Z(q)^2)));; Add(squares, 0*Z(q));
	nonsquares := Difference(AsList(GF(q)),squares);;
	n := First(nonsquares, t -> t-1 in squares);

	zeta := Z(q^2);
	omega := zeta^(q+1);
	i := zeta^((q+1)/2);
	z := zeta^(q-1);
	a := (z+z^q)/2;
	clan := [];
	for t in GF(q) do
	    if t^2-2/(1+a) in squares then
	       Add(clan, [[t, 0],[0,-omega*t]] * Z(q)^0);
	    fi;
	od;

	for j in [0..(q-1)/2] do
	    Add(clan, [[-(z^(2*j+1)+z^(-2*j))/(z+1), i*(z^(2*j+1)-z^(-2*j))/(z+1)],
	       [i*(z^(2*j+1)-z^(-2*j))/(z+1), -omega*(z^(2*j+1)+z^(-2*j))/(z+1)]] * Z(q)^0 );
	od;

  return qClan(clan, GF(q));
end );

#############################################################################
#O  KantorFamilyByqClan( <clan> )
# returns the Kantor familyt corresponding with the q-Clan <clan>
##
InstallMethod( KantorFamilyByqClan,
	"for a q-Clan",
    [ IsqClanObj and IsqClanRep ],
	function( clan )
    local g, q, f, i, omega, mat, at, ainf, ainfstar, clanmats,
          ainfgens, centregens, as, astars, k;
    f := clan!.basefield;
    clanmats := clan!.matrices;
    q := Size(f);
    i := One(f);
    omega := PrimitiveElement(f);
    mat := function(a1,a2,c,b1,b2)
             return i * [[1, a1, a2,  c], [0,  1,  0, b1],
                         [0,  0,  1, b2], [0,  0,  0,  1]];
           end;
    centregens := [mat(0,0,1,0,0), mat(0,0,omega,0,0)];
    ainfgens := [mat(0,0,0,1,0),mat(0,0,0,0,1),mat(0,0,0,omega,0),mat(0,0,0,0,omega)];
    ainf := Group(ainfgens);
    ainfstar := Group(Union(ainfgens,centregens));

    at := function( m )
       ## returns generators for Kantor family element defined by q-Clan element m
       local a1, a2, k, gens, bas, zero;
       gens := [];
       bas := AsList(Basis(f));
       zero := Zero(f);
       for a1 in bas do
           k := [a1,zero] * (m+TransposedMat(m));
           Add(gens, mat(a1,zero,[a1,zero]*m*[a1,zero],k[1],k[2]) );
       od;

       for a2 in bas do
           k := [zero,a2] * (m+TransposedMat(m));
           Add(gens, mat(zero,a2,[zero,a2]*m*[zero,a2],k[1],k[2]) );
       od;
       return gens;
    end;

    g := Group(Union( ainfgens, centregens, at(clanmats[1]) ));
    as := List(clanmats, m -> Group( at(m) ));
    Add(as, ainf);
    astars := List(clanmats, m -> Group(Union(at(m), centregens)));
    Add(astars, ainfstar);
    return [g, as, astars];
  end );

#############################################################################
#O  EGQByqClan( <clan> )
# returns the EGQ constructed from the q-Clan, using the corresponding Kantor family.
##
InstallMethod( EGQByqClan,
	"for a q-Clan",
	[ IsqClanObj and IsqClanRep ],
	function( clan )
		local kantor;
		kantor := KantorFamilyByqClan( clan );

		Info(InfoFinInG, 1, "Computed Kantor family. Now computing EGQ...");
			return EGQByKantorFamily(kantor[1], kantor[2], kantor[3]);
  end );

#############################################################################
#O  IncidenceGraph( <gp> )
###
InstallMethod( IncidenceGraph,
    "for an ElationGQ in generic representation",
    [ IsElationGQ and IsGeneralisedPolygonRep ],
    function( gp )
    local points, lines, graph, adj, elationgroup, coll, act,sz;
    #if not "grape" in RecNames(GAPInfo.PackagesLoaded) then
    #   Error("You must load the GRAPE package\n");
    #fi;
    if IsBound(gp!.IncidenceGraphAttr) then
       return gp!.IncidenceGraphAttr;
    fi;
    points := AsList(Points(gp));
    lines := AsList(Lines(gp));
    Setter( HasGraphWithUnderlyingObjectsAsVertices )( gp, false);

    Info(InfoFinInG, 1, "Computing incidence graph of generalised polygon...");

	adj := function(x,y)
		if x!.type <> y!.type then
			return IsIncident(x,y);
		else
			return false;
		fi;
	end;
    sz := Size(points);

	if HasCollineationSubgroup(gp) then
		Info(InfoFinInG, 1, "Using subgroup of the collineation group...");
		coll := CollineationSubgroup(gp);
		act := CollineationAction(coll);
		graph := Graph(coll,Concatenation(points,lines),act,adj,true);
	elif HasElationGroup(gp) then
		Info(InfoFinInG, 1, "Using elation of the collineation group...");
		elationgroup := ElationGroup(gp);
		act := CollineationAction(elationgroup);
		graph := Graph(elationgroup,Concatenation(points,lines),act,adj,true);
	else
	   graph := Graph( Group(()), [1..sz+Size(lines)], OnPoints, adj );
	fi;

    Setter( IncidenceGraphAttr )( gp, graph );
    return graph;
  end );

#############################################################################
#
#	BLT sets and q-Clans.
#
#############################################################################

#############################################################################
#O  BLTSetByqClan( <clan> )
# returns the BLT set corresponding with the q-Clan <clan>
##
InstallMethod( BLTSetByqClan,
	"for a q-Clan",
	[ IsqClanObj and IsqClanRep ],
	function( clan )
    ##
    ## The q-clan must consist only of symmetric matrices
    ##
        local q, i, f,  blt, ps, x, pring, form, poly;
    f := clan!.basefield;
    q := Size(f);
    i := One(f);
    blt := List(clan!.matrices, t -> [i, t[2,2], -t[1,2], t[1,1],  t[1,2]^2 -t[1,1]*t[2,2]]);
    Add(blt, [0,0,0,0,1]*i);  ## last point is distinguished point.
    for x in blt do
		ConvertToVectorRepNC(x,f);
	od;

    ## This BLT-set is in Q(4,q) defined by Gram matrix
    pring := PolynomialRing(GF(q),5);
    poly := pring.1*pring.5+pring.2*pring.4-pring.3^2;
    form := QuadraticFormByPolynomial(poly, pring);
    ps := PolarSpace( form );
    return List(blt, x -> VectorSpaceToElement(ps, x));
end );

#############################################################################
#O  EGQByBLTSet( <blt>, <p>, <solid> )
# helper operation to be called from EGBByBLTSet( <bltset> ), where the latter
# is a BLT set of points in Q(4,q).
# Note: points and lines are always elements of W(5,q). Since we will use shadows
# it is better to wrap them really as elements of W(5,q) rather then leaving them
# an element of PG(5,q). E.g. Planes(x) makes a huge difference when x is a W(5,q)
# line than when x is a PG(5,q) line, and I am to lazy to type Planes(w5q,x) :-)
# extra fields in geo record:
# - planes: contains the planes spanned by the basepoint and the blt lines in W(5,q).
# - basepoint: base point p
# - basepointperp: image of p under the polarity associated with W(5,q).
##
InstallMethod( EGQByBLTSet,
	"constructs an EGQ from a BLT-set of lines of W(3,q) via the Knarr construction",
	[IsList, IsSubspaceOfProjectiveSpace, IsSubspaceOfProjectiveSpace],
	function( blt, p, solid)
	## The point p is a point of W(5,q).
	## "solid" is a 3-space contained in P^perp
	## blt is a BLT-set of lines of W(3,q)

	local w3q, f, q, w5q, perp, pperp, x,em, blt2, pis, pointreps, linereps, reps, lines2,
         geo, points, lines, ty, listels, inc, shadpoint, shadline, vec, r, spanpts,
		 dist, meetlns;

	w3q := blt[1]!.geo;
	f := w3q!.basefield;
	q := Size(f);
	w5q := SymplecticSpace(5, f);
	perp := PolarMap(w5q);
	pperp := perp(p);
	em := NaturalEmbeddingBySubspace(w3q, w5q, solid);
	blt2 := List(blt,t->t^em);
	pis := List(blt2, l -> Span(p, l));
	# next one is dirty, see note above.
	pis := List(pis,x->Wrap(w5q,3,UnderlyingObject(x)));
	## check everything is kosher
	if not solid in pperp then
		Error("Solid is not contained in perp of point");
	fi;
	if p in solid then
		Error("Chosen point is contained in the chosen solid");
	fi;
	vec := ComplementSpace(f^6,Unpack(UnderlyingObject(pperp)));
	r := VectorSpaceToElement(w5q,BasisVectors(Basis(vec)));
	pointreps := Concatenation([r],blt2,[p]);

	linereps := List(pis,x->Span(r,Meet(x,perp(r))));
	linereps := List(linereps,x->Wrap(w5q,3,UnderlyingObject(x)));
	lines2 := List(blt2,x->Span(p,x));
	lines2 := List(lines2,x->Wrap(w5q,3,UnderlyingObject(x)));
	linereps := Concatenation(linereps,lines2);

	listels := function(gp, i)
		local pts,lines,x,points2;
		if i=1 then
			Info(InfoFinInG, 1, "Computing points(1) of Knarr construction...");
			pts := Filtered(Points(gp!.polarspace),x->not x in gp!.basepointperp);
			Add(pts,gp!.basepointobj);
			Info(InfoFinInG, 1, "Computing points(2) of Knarr construction...");
			if IsBound(gp!.points2) then
				Info(InfoFinInG, 1, "...which were computed already");
				Append(pts,gp!.points2);
			else
				points2 := [];
				for x in gp!.planes do
					Append(points2,Filtered(Lines(x),t->not gp!.basepointobj in t));
				od;
				points2 := List(points2,x->Wrap(w5q,2,UnderlyingObject(x)));
				gp!.points2 := ShallowCopy(points2);
				Append(pts,points2);
			fi;
			return List(pts,x->Wrap(gp,1,x));
		elif i=2 then
			Info(InfoFinInG, 1, "Lines(1) of Knarr construction are known...");
			lines := ShallowCopy(gp!.planes);
			if IsBound(gp!.points2) then
				points2 := gp!.points2;
				Info(InfoFinInG, 1, "Taking points(2) of Knarr construction which were computed already...");
			else
				Info(InfoFinInG, 1, "Computing points(2) of Knarr construction (and saving them)...");
				points2 := [];
				for x in gp!.planes do
					Append(points2,Filtered(Lines(x),t->not gp!.basepointobj in t));
				od;
				points2 := List(points2,x->Wrap(w5q,2,UnderlyingObject(x)));
				gp!.points2 := ShallowCopy(points2);
			fi;
			Info(InfoFinInG, 1, "Computing lines(2) of Knarr construction... please wait");
			for x in points2 do
				Append(lines,Filtered(Planes(x), t -> not t in gp!.basepointperp));
			od;
			return List(lines,x->Wrap(gp,2,x));
		fi;
	end;

	inc := function(x,y)
		return IsIncident(x!.obj,y!.obj); #underlying objects are elements of a projective space
	end;

	shadpoint := function(pt)
		local obj,planes,objs,geo,w5q;
		obj := pt!.obj;
		geo := pt!.geo;
		w5q := geo!.polarspace;
		if ProjectiveDimension(obj) = 1 then
			return List(Planes(w5q,obj),x->Wrap(geo,2,x));
		elif pt=BasePointOfEGQ(geo) then
			return List(geo!.planes,x->Wrap(geo,2,x));
		else
			objs := List(geo!.planes,x->Meet(PolarMap(w5q)(obj),x));
			objs := List(objs,x->Span(obj,x));
			objs := List(objs,x->Wrap(w5q,3,UnderlyingObject(x)));
			return List(objs,x->Wrap(geo,2,x));
		fi;
	end;

	shadline := function(line)
		local obj,geo,p,pts,meet,objs;
		obj := line!.obj;
		geo := line!.geo;
		p := BasePointOfEGQ(geo);
		if p!.obj in obj then
			pts := List(Filtered(Lines(obj), t -> not p!.obj in t),x->Wrap(geo,1,x));
			Add(pts,p);
			return pts;
		else
			meet := Meet(obj,PolarMap(w5q)(p!.obj));
			pts := List(Points(meet));
			objs := Difference(List(Points(obj)),pts);
			pts := List(objs,x->Wrap(geo,1,x));
			Add(pts,Wrap(geo,1,meet));
			return pts;
		fi;
	end;

	spanpts := function(p,q)
		local gp,obj,l,perp,lperp,m;
		gp := p!.geo;
		if p = q then
			return fail;
		elif p = BasePointOfEGQ(gp) then
			return spanpts(q,p);
		elif ProjectiveDimension(p!.obj) = 0 then
			if q = BasePointOfEGQ(gp) then
				return fail;
			elif ProjectiveDimension(q!.obj) = 1 then
				obj := Span(p!.obj,q!.obj);
				if obj in gp!.polarspace then
					obj := Wrap(gp!.polarspace,3,UnderlyingObject(obj));
					return Wrap(gp,2,obj);
				else
					return fail;
				fi;
			elif ProjectiveDimension(q!.obj) = 0 then
				l := Span(p!.obj,q!.obj);
				perp := PolarMap(gp!.polarspace);
				lperp := perp(l);
				m := List(gp!.planes,x->Meet(x,lperp));
				m := First(m,x->ProjectiveDimension(x)=1);
				if m=fail then
					return fail;
				else
					l := Span(m,p!.obj);
					l := Wrap(gp!.polarspace,3,UnderlyingObject(l));
					return Wrap(gp,2,l);
				fi;
			fi;
		elif ProjectiveDimension(p!.obj) = 1 then
			if q = BasePointOfEGQ(gp) then
				obj := Span(p!.obj,q!.obj);
				obj := Wrap(gp!.polarspace,3,UnderlyingObject(obj));
				return Wrap(gp,2,obj);
			elif ProjectiveDimension(q!.obj) = 1 then
				obj := Span(p!.obj,q!.obj);
				if ProjectiveDimension(obj) = 2 then
					obj := Wrap(gp!.polarspace,3,UnderlyingObject(obj));
					return Wrap(gp,2,obj);
				else
					return fail;
				fi;
			fi;
		else
			return spanpts(q,p);
		fi;
	end;

	meetlns := function(l,m)
		local gp,obj;
		gp := l!.geo;
		if l = m then
			return fail;
		fi;
		if not gp!.basepointobj in l!.obj then
			obj := Meet(l!.obj,m!.obj);
			if not gp!.basepointobj in m!.obj then
				if ProjectiveDimension(obj) = 0 and not obj in gp!.basepointperp then
					return Wrap(gp,1,obj);
				else
					return fail;
				fi;
			else
				if ProjectiveDimension(obj) = 1 then
					return Wrap(gp,1,obj);
				else
					return fail;
				fi;
			fi;
		else
			if gp!.basepointobj in m!.obj then
				return BasePointOfEGQ(gp);
			else
				return meetlns(m,l);
			fi;
		fi;
	end;

    dist := function(x,y)
        if x!.type <> y!.type then
            if inc(x,y) then
                return 1;
            else
                return 3;
            fi;
        elif x!.type = 1 then
            if spanpts(x,y) = fail then
                return 4;
            else
                return 2;
            fi;
        else
            if meetlns(x,y) = fail then
                return 4;
            else
                return 2;
            fi;
        fi;
    end;

	geo := rec( pointsobj := [], linesobj := [], incidence := inc, planes := pis, polarspace := w5q,
				basepointobj := p, basepointperp := pperp, listelements := listels, shadowofpoint := shadpoint,
				shadowofline := shadline, span := spanpts, meet := meetlns, distance := dist );
	ty := NewType( GeometriesFamily, IsElationGQByBLTSet and IsGeneralisedPolygonRep);
	Objectify( ty, geo );
	pointreps := List(pointreps,x->Wrap(geo,1,x));
	linereps := List(linereps,x->Wrap(geo,2,x));
	SetRepresentativesOfElements(geo,[pointreps,linereps]);
	SetBasePointOfEGQ( geo, Wrap(geo, 1, p) );
	SetOrder(geo, [q^2, q]);
	SetTypesOfElementsOfIncidenceStructure(geo, ["point","line"]);
    SetName(geo,Concatenation("<EGQ of order ",String([q^2,q])," and basepoint in W(5, ",String(q)," ) >"));
	return geo;
end );

#############################################################################
#O  EGQByBLTSet( <blt> )
# User method to construct the EGQ including the elation group.
##
InstallMethod( EGQByBLTSet,
	"constructs an EGQ from a BLT-set of points of Q(4,q) via the Knarr construction",
	[ IsList ],
	function( blt )
	local q4q, f, w3q, duality, w5q, p, pg5, solid,
         q4qcanonical, iso, bltdual, geo, bas, listels,
         mat, elations, a, gens, zero, action, b;
	q4q := AmbientGeometry( blt[1] );
	f := q4q!.basefield;
	w3q := SymplecticSpace(3, f);
	duality := NaturalDuality( w3q );
	w5q := SymplecticSpace(5, f);
	p := VectorSpaceToElement(w5q, [1,0,0,0,0,0] * One(f));
	pg5 := AmbientSpace( w5q );
	solid := VectorSpaceToElement(pg5, [[1,0,0,0,0,1],[0,0,1,0,0,0],
                                       [0,0,0,1,0,0],[0,0,0,0,1,0]]*One(f));
	q4qcanonical := Range(duality)!.geometry;
	iso := IsomorphismPolarSpaces(q4q, q4qcanonical);
	bltdual := PreImagesSet(duality, ImagesSet(iso, blt));

	Info(InfoFinInG, 1, "Now embedding dual BLT-set into W(5,q)...");

	geo := EGQByBLTSet( bltdual, p, solid);

	## Now we construct the elation group. See Maska Law's Thesis for details
	## (we have a different form though, so we need to apply a base change).

	Info(InfoFinInG, 1, "Computing elation group...");

	mat := function(a,b,c,d,e)
            local m;
            m := IdentityMat(6, f);
            m[6]{[1..5]} := [e,d,c,-b,-a];
            m[2,1] := a; m[3,1] := b; m[4,1] := c; m[5,1] := d;
            return m;
          end;
	bas := AsList(Basis(f));
	gens := [];
	zero := Zero(f);
	for a in bas do
		Add(gens, mat(a,zero,zero,zero,zero) );
		Add(gens, mat(zero,a,zero,zero,zero) );
		Add(gens, mat(zero,zero,a,zero,zero) );
		Add(gens, mat(zero,zero,zero,a,zero) );
		Add(gens, mat(zero,zero,zero,zero,a) );
	od;
	## base change
	b := [ [ 1, 0, 0, 0, 0, 0 ], [ 0, 0, 0, 0, 0, 1 ],
		 [ 0, 1, 0, 0, 0, 0 ], [ 0, 0, 0, 0, 1, 0 ],
         [ 0, 0, 1, 0, 0, 0 ], [ 0, 0, 0, 1, 0, 0 ] ];
	gens := List(gens, t-> b*t*b^-1);
	gens := List(gens, t -> CollineationOfProjectiveSpace(t,f));
	elations := Group( gens );
    #SetOrder
	SetElationGroup( geo, elations );

	action := function(el, x)
		return Wrap( geo, el!.type, OnProjSubspaces(el!.obj, x));
	end;
	SetCollineationAction( elations, action );

	listels := function(gp,i)
		local reps,group,act;
		Info(InfoFinInG, 1, "Using elation group to enumerate elements");
		group := ElationGroup(gp);
		act := CollineationAction(group);
		reps := RepresentativesOfElements(gp)[i];
		return Union(List(reps,x->List(Enumerate(Orb(group,x,act)))));
	end;
	geo!.listelements := listels;
	return geo;
	end );

#############################################################################
#O  Display( <egq> )
#  for an EGQByBLTSet
##
InstallMethod( Display,
	"for an elation GQ by BLT set",
	[ IsElationGQByBLTSet ],
	function( gp )
        Print("Elation generalised quadrangle of order ",String(Order(gp)),"\n",
                "with defining planes\n");
        View(gp!.planes);
        Print("\nand base point with underlyig object\n");
        View(gp!.basepointobj);
    end );

#############################################################################
#O  DefiningPlanesOfEGQByBLTSet( <egq> )
#  for an EGQByBLTSet
##
InstallMethod( DefiningPlanesOfEGQByBLTSet,
    "for an EGQByBLTSet",
    [ IsElationGQByBLTSet ],
    function( egq )
		return egq!.planes;
	end );

#############################################################################
#O  ObjectToElement( <egq>, <type>, <el> )
#
##
InstallMethod( ObjectToElement,
	"for an egq byt blt, an integer and a subspace of a polar space",
	[ IsElationGQByBLTSet, IsPosInt, IsSubspaceOfClassicalPolarSpace],
	function(egq, t, el)
		local p,planes;
		if not el in egq!.polarspace then
			Error("<el> does not represent an element of <egq>");
		fi;
		planes := egq!.planes;
		if t=1 then
			p := egq!.basepointobj;
			if ProjectiveDimension(el) = 0 then
				if el = p then
					return BasePointOfEGQ(egq);
				elif not IsCollinear(egq!.polarspace,p,el) then
					return Wrap(egq,1,el);
				else
					Error("<el> does not represent a point of <egq>");
				fi;
			elif ProjectiveDimension(el) = 1 then
				if Span(p,el) in planes then
					return Wrap(egq,1,el);
				fi;
			else
				Error("<el> does not represent a point of <egq>");
			fi;
		elif t=2 then
			if el in planes then
				return Wrap(egq,2,el);
			elif Number(planes,x->ProjectiveDimension(Meet(x,el))=1)=1 then
				return Wrap(egq,2,el);
			else
				Error("<el> does not represent a line of <egq>");
			fi;
		else
			Error("<egq> is a point-line geometry not containing elements of type ",t);
		fi;
	end );

#############################################################################
#O  ObjectToElement( <egq>, <el> )
#
##
InstallMethod( ObjectToElement,
	"for an egq byt blt, and a subspace of a polar space",
	[ IsElationGQByBLTSet, IsSubspaceOfClassicalPolarSpace],
	function(egq, el)
		local p,planes;
		if not el in egq!.polarspace then
			Error("<el> does not represent an element of <egq>");
		fi;
		planes := egq!.planes;
		p := egq!.basepointobj;
		if ProjectiveDimension(el) = 2 then
			if el in planes then
				return Wrap(egq,2,el);
			elif Number(planes,x->ProjectiveDimension(Meet(x,el))=1)=1 then
				return Wrap(egq,2,el);
			else
				Error("<el> does not represent an element of <egq>");
			fi;
		elif ProjectiveDimension(el) = 1 then
			if Span(p,el) in planes then
					return Wrap(egq,1,el);
			else
				Error("3<el> does not represent an element of <egq>");
			fi;
		elif ProjectiveDimension(el) = 0 then
			if el = p then
				return BasePointOfEGQ(egq);
			elif not IsCollinear(egq!.polarspace,p,el) then
				return Wrap(egq,1,el);
			else
				Error("<el> does not represent an element of <egq>");
			fi;
		else
			Error("<el> does not represent an element of <egq>");
		fi;
	end );

#############################################################################
#O  CollineationSubgroup( <egq> )
# The setwise stabiliser of the BLT lines is a sub group of the Collineation
# group of the egq-by-blt. This subgroup is of course not the complete
# collineation group, but might be useful, and can be computed much quicker
# than the full collineation group.
##
InstallMethod( CollineationSubgroup,
	"for a generalised polygon, an integer and an object",
	[ IsElationGQByBLTSet ],
	function(egq)
		local ps, stabp, coll;
		ps := egq!.polarspace;
		stabp := FiningStabiliser(CollineationGroup(ps),egq!.basepointobj);
		coll := FiningSetwiseStabiliser(stabp,egq!.planes);
		SetCollineationAction(coll,CollineationAction(ElationGroup(egq)));
		return coll;
	end );


#############################################################################
#O  FlockGQByqClan( <clan> )
# returns the BLT set corresponding with the q-Clan <clan>
# not documented yet.
##
InstallMethod( FlockGQByqClan, [ IsqClanObj ],
 function( qclan )
  local f, q, mat, form, w5, p, blt, x, perp, pperp, pg5, a, bas, gens, zero, elations, action,
        projpoints, gqpoints, gqlines, gqpoints2, gqlines2, res, geo, ty, points, lines, clan,
		pgammasp, stabp, stabblt, hom, omega, imgs, bltvecs;
  f := qclan!.basefield;
  clan := qclan!.matrices;
  q := Size(f);
  if not IsOddInt(q) then
       Error("Invalid input"); return;
  fi;

  mat := [[0,0,0,0,0,1],[0,0,0,0,1,0],[0,0,0,1,0,0],
          [0,0,-1,0,0,0],[0,-1,0,0,0,0],[-1,0,0,0,0,0]] * Z(q)^0;
  form := BilinearFormByMatrix(mat, GF(q));
  w5 := PolarSpace( form );
  p := VectorSpaceToElement(w5, [1,0,0,0,0,0] * Z(q)^0);

  blt := [ VectorSpaceToElement(w5, [[1,0,0,0,0,0], [0,0,0,1,0,0],[0,0,0,0,1,0]]*One(f)) ];

	bltvecs := List(clan,x->[[1,0,0,0,0,0], [0,1,0,x[1][2],x[1][1],0], [0,0,1,x[2][2],x[1][2],0]] * One(f));
	#for x in bltvecs do
	#	ConvertToMatrixRepNC(x,f); #jdb 19/09/2018: I think this is not needed anymore.
	#od;

  #for x in clan do
  #    Add(blt, VectorSpaceToElement(w5, [[1,0,0,0,0,0], [0,1,0,x[1][2],x[1][1],0], [0,0,1,x[2][2],x[1][2],0]] * One(f)));
  #od;

	for x in bltvecs do
		Add(blt,VectorSpaceToElement(w5,x));
	od;
    Info(InfoFinInG, 1, "Making flock GQ...");

  perp := PolarMap(w5);;
  pperp := perp(p);
  pg5 := AmbientSpace( w5 );;
  projpoints := Points(pg5);;

	Info(InfoFinInG, 1, "...points outside of perp of P...");

  gqpoints := Filtered(projpoints, x -> not x in pperp);;
  Add(gqpoints, p);

  gqlines := ShallowCopy(blt);;
  gqpoints2 := [];;

    Info(InfoFinInG, 1, "...lines contained in some BLT-set element...");

  for x in gqlines do
      res := ShadowOfElement(pg5, x, 2);
      res := Filtered(res, t -> not p in t);
      Append(gqpoints2, res);
  od;
  gqpoints2 := Set(gqpoints2);;
  gqlines2 := [];;

    Info(InfoFinInG, 1, "...planes meeting some BLT-set element in a line...");
  for x in gqpoints2 do
      res := ShadowOfFlag(pg5, [x,perp(x)], 3);
      res := Filtered(res, t -> not p in t);
      Append(gqlines2, res);
  od;

    Info(InfoFinInG, 1, "...sorting the points and lines...");

  points := SortedList( Concatenation(gqpoints, gqpoints2) );;
  lines := SortedList( Concatenation(gqlines, gqlines2) );;

  geo := rec( points := points, lines := lines, incidence := IsIncident);;

  ty := NewType( GeometriesFamily, IsElationGQ and IsGeneralisedPolygonRep);
  Objectify( ty, geo );
  SetBasePointOfEGQ( geo, Wrap(geo, 1, p) );
  SetAmbientSpace(geo, w5);
  SetOrder(geo, [q^2, q]);
  SetTypesOfElementsOfIncidenceStructure(geo, ["point","line"]);

  Info(InfoFinInG, 1, "Computing collineation group in PGammaSp(6,q)...");
  pgammasp := CollineationGroup( w5 );
  stabp := SetwiseStabilizer(pgammasp, OnProjSubspaces, [p])!.setstab;

  Info(InfoFinInG, 1, "..computed stabiliser of P");

  ## compute the stabiliser of the BLT-set differently...

  hom := ActionHomomorphism(stabp, AsList(Planes(p)), OnProjSubspaces, "surjective");
  omega := HomeEnumerator(UnderlyingExternalSet(hom));;
  imgs := Filtered([1..Size(omega)], x -> omega[x] in blt);;
  stabblt := Stabilizer(Image(hom), imgs, OnSets);
  gens := GeneratorsOfGroup(stabblt);
  gens := List(gens, x -> PreImagesRepresentative(hom, x));
  stabblt := GroupWithGenerators(gens);

  Info(InfoFinInG, 1, "..computed stabiliser of BLT set");

  SetCollineationGroup( geo, stabblt );
  action := function(el, x)
               return Wrap( geo, el!.type, OnProjSubspaces(el!.obj, x));
             end;
  SetCollineationAction( stabblt, action );

  ## Now we construct the elation group. See Maska Law's Thesis for details

  Info(InfoFinInG, 1, "Computing elation group...");
  mat := function(a,b,c,d,e)
            local m;
            m := IdentityMat(6, f);
            m[6]{[1..5]} := [e,d,c,-b,-a];
            m[2,1] := a; m[3,1] := b; m[4,1] := c; m[5,1] := d;
            return m;
         end;
  bas := AsList(Basis(f));
  gens := [];
  zero := Zero(f);
  for a in bas do
       Add(gens, mat(a,zero,zero,zero,zero) );
       Add(gens, mat(zero,a,zero,zero,zero) );
       Add(gens, mat(zero,zero,a,zero,zero) );
       Add(gens, mat(zero,zero,zero,a,zero) );
       Add(gens, mat(zero,zero,zero,zero,a) );
  od;
  gens := List(gens, t -> CollineationOfProjectiveSpace(t,f));
  elations := SubgroupNC( stabblt, gens );
  SetElationGroup( geo, elations );
  SetCollineationAction( elations, action );

  return geo;
end );
