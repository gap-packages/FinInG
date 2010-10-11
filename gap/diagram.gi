#############################################################################
##
##  diagram.gi              FinInG package
##                                                              John Bamberg
##                                                              Anton Betten
##                                                              Jan De Beule
##                                                             Philippe Cara
##                                                            Michel Lavrauw
##                                                                 Maska Law
##                                                           Max Neunhoeffer
##                                                            Michael Pauley
##                                                             Sven Reichard
##
##  Copyright 2008 University of Western Australia, Perth
##                 Lehrstuhl D fuer Mathematik, RWTH Aachen
##                 Ghent University
##                 Colorado State University
##                 Vrije Universiteit Brussel
##
##  Implementation stuff for diagram geometries
##
#############################################################################

########################################
#
# Things To Do:
#
# - testing
# - Lots of attributes (in general, but at least for coset geometries), such as
#     - "IsFlagTransitiveGeometry"
#     - "IsResiduallyConnected"
#     - "IsFirmGeometry"
#     - "IsThinGeometry"
#     - "IsThickGeometry"
#     - "IncidenceGraph"
# - documentation
# - Priorities
#   1. Residues
#   2. Recognition of rank 2 geometries
#   3. Diagram and graph
#   4. Documentation
#
########################################



#############################################################################
##
##  Diagrams and the drawing of them
##
#############################################################################


InstallMethod( \=, [ IsVertexOfDiagram and IsVertexOfDiagramRep, 
                     IsVertexOfDiagram and IsVertexOfDiagramRep ],
  function( u, v )
    return u!.type = v!.type;
  end );

InstallMethod( \=, [ IsEdgeOfDiagram and IsEdgeOfDiagramRep, 
                     IsEdgeOfDiagram and IsEdgeOfDiagramRep ],
  function( u, v )
    return u!.edge = v!.edge;
  end );

InstallMethod( DiagramOfGeometry, [IsCosetGeometry],
  function( cg )
    local rank2residues, vertices, types, x, v, edges, parameters, e, diagram, parabolics;
    rank2residues := Rank2Residues( cg );
    Perform(rank2residues, MakeRank2Residue);

    parabolics := cg!.parabolics;
	vertices := [];
    types := TypesOfElementsOfIncidenceStructure( cg );

  ## Wrapping up... 
    for x in types do
      v := rec( type := x );
      Objectify( NewType( VertexOfDiagramFamily, IsVertexOfDiagram and
                      IsVertexOfDiagramRep ), v);
      Add( vertices, v );
    od;

    edges := [];

  ## Wrapping up...
    for x in rank2residues do
        parameters := Rank2Parameters( x!.residuegeometry );
        if not parameters[1] = [2,2,2] then
           e := rec( edge := x!.edge );
           Objectify( NewType( EdgeOfDiagramFamily, 
                        IsEdgeOfDiagram and IsEdgeOfDiagramRep ), e);
           SetParametersEdge(e, parameters[1]);
           if not HasOrderVertex( vertices[x!.edge[1]] ) then
              SetOrderVertex( vertices[x!.edge[1]], parameters[2][1] );
              SetNrElementsVertex( vertices[x!.edge[1]], IndexNC(cg!.group, parabolics[x!.edge[1]]) );
              SetStabiliserVertex( vertices[x!.edge[1]], parabolics[x!.edge[1]] );
           fi;
           if not HasOrderVertex( vertices[x!.edge[2]] ) then
              SetOrderVertex( vertices[x!.edge[2]], parameters[3][1] );
              SetNrElementsVertex( vertices[x!.edge[2]], IndexNC(cg!.group, parabolics[x!.edge[2]]) );
              SetStabiliserVertex( vertices[x!.edge[2]], parabolics[x!.edge[2]] );
           fi;

           # SetResidueLabelForEdge( e, "P");
           Add( edges, e );
        fi;
    od;
	diagram := rec( vertices := vertices, edges := edges );;
    Objectify( NewType( DiagramFamily, IsDiagram and IsDiagramRep ), diagram);
    SetGeometryOfDiagram( diagram, cg );
    return diagram;
  end );

InstallMethod( DrawDiagram, [ IsDiagram, IsString ],
  function( diagram, filename )
    local vertices, edges, longstring, v, e;
    vertices := diagram!.vertices;
    edges := diagram!.edges;

    longstring := "digraph DIAGRAM{\n rankdir = LR;\n";
    for v in vertices do
      longstring:=Concatenation(longstring, "subgraph cluster", String(v!.type), " {\n");
      longstring:=Concatenation(longstring, "color=transparent\n");    
      longstring:=Concatenation(longstring, "node [shape=circle, width=0.1]; ", String(v!.type), " [label=\"\"];\n");    
      longstring:=Concatenation(longstring, "labelloc=b\n");    
      longstring:=Concatenation(longstring, "label=\"", String(OrderVertex(v)), "\\n", String(NrElementsVertex(v)),
                   "\";\n}\n");   
    od;
    for e in edges do
      longstring:=Concatenation(longstring, String(e!.edge[1]), " -> ", String(e!.edge[2]), 
       " [label = \"", String(ParametersEdge(e)[2]), " ", String(ParametersEdge(e)[1]), " ", 
       String(ParametersEdge(e)[3]), "\", arrowhead = none ];\n");
    od;    
    longstring:=Concatenation(longstring, "}\n");    
    PrintTo( Concatenation(filename, ".dot") , longstring );
    Exec( Concatenation("dot -Tps ", filename, ".dot -o ", filename, ".ps") );
    return;
  end );


InstallGlobalFunction( Drawing_Diagram,
  function( verts, edges, way )
    local mat, v, e, v1, v2, c1, c2, posvertices,
          breadth, height, edges2, label, unit;

    ## The unit we use for the character size of an edge
    unit := 3;

    ## First find dimensions of diagram

    breadth := Maximum( List(way, t->t[2]) );
    height := Maximum( List(way, t->t[1]) );
    edges2 := List(edges, t -> [Position(verts, t!.edge[1]), Position(verts, t!.edge[2]), t] );;

    # We now work out where to put the lines,
    # and what type of lines they are  
    # 1: ---
    # 2: |
    # 3: /
    # 4: \

    mat := NullMat( (unit + 1)*(height - 1) + 1, (unit + 1) * (breadth - 1) + 1, Integers);

    ## put vertices in (-1)
    posvertices := List( way, v -> 
           [ (unit + 1) * (v[1] - 1) + 1, (unit + 1) *(v[2] - 1) + 1] );

    for v in posvertices do
      mat[v[1]][v[2]] := -1;
    od;

    ## put lines in

    for e in edges2 do
        v1 := posvertices[ e[1] ];
        v2 := posvertices[ e[2] ];

        ## start at middle of edge

        c1 := (v1[1]+v2[1])/2;
        c2 := (v1[2]+v2[2])/2;
        if HasResidueLabelForEdge( e[3] ) then
           label := ResidueLabelForEdge( e[3] );
           if label = "GQ" then
              if v1[1] = v2[1] then 
                 mat[c1]{[c2-1,c2,c2+1]} := [ "=", "=", "="];
              elif v1[2] = v2[2] then
                 mat{[c1-1,c1,c1+1]}[c2] := ["||", "||", "||"];
              elif (v1[1] - v2[1]) * (v1[2]-v2[2]) < 0  then
                 mat[c1+1][c2-1] := "//";
                 mat[c1][c2] := "//";
                 mat[c1-1][c2+1] := "//";
              else
                 mat[c1-1][c2+1] := "\\\\";
                 mat[c1][c2] := "\\\\";
                 mat[c1+1][c2-1] := "\\\\";
              fi;

           elif label = "P" or label = "-" then
              if v1[1] = v2[1] then 
                 mat[c1]{[c2-1,c2,c2+1]} := [ "-", "-", "-"];
              elif v1[2] = v2[2] then
                 mat{[c1-1,c1,c1+1]}[c2] := ["|", "|", "|"];
              elif (v1[1] - v2[1]) * (v1[2]-v2[2]) < 0  then
                 mat[c1+1][c2-1] := "/";
                 mat[c1][c2] := "/";
                 mat[c1-1][c2+1] := "/";
              else
                 mat[c1-1][c2-1] := "\\";
                 mat[c1][c2] := "\\";
                 mat[c1+1][c2+1] := "\\";
              fi;

           else  
              if v1[1] = v2[1] then 
                mat[c1]{[c2-1,c2,c2+1]} := ["-", label, "-"];
              elif v1[2] = v2[2] then
                mat{[c1-1,c1,c1+1]}[c2] := ["|", label, "|"];
              elif (v1[1] - v2[1]) * (v1[2]-v2[2]) < 0  then
                mat[c1+1][c2-1] := "/";
                mat[c1][c2] := label;
                mat[c1-1][c2+1] := "/";
              else
                mat[c1-1][c2+1] := "\\";
                mat[c1][c2] := label;
                mat[c1+1][c2-1] := "\\";
              fi;
           fi;
        else
           if v1[1] = v2[1] then 
              mat[c1]{[c2-1,c2,c2+1]} := [1,1,1]; 
           elif v1[2] = v2[2] then
              mat[c1][c2] := 2;
           elif (v1[1] - v2[1]) * (v1[2]-v2[2]) < 0  then
              mat[c1][c2] := 3;
           else
              mat[c1][c2] := 4;
           fi;
        fi;
    od;
    return mat;
  end ); 


InstallMethod( Display, [ IsDiagram and IsDiagramRep ],
  function( diag )
    local way, verts, edges, breadth, height, i, j, mat;
    way := diag!.drawing;
    verts := diag!.vertices;
    edges := diag!.edges;
    mat := Drawing_Diagram( verts, edges, way ); 

    for i in [1..Size(mat)] do
      for j in [1..Size(mat[1])] do 
          if mat[i][j] = 0 then Print(" ");
            # if IsOddInt(j) then Print( " " );
            # else Print( "  " );
            # fi;
          elif mat[i][j] = -1 then
             Print( "o" );
          elif mat[i][j] = 1 then
             Print( "-" );
          elif mat[i][j] = 2 then
             Print( "|" );
          elif mat[i][j] = 3 then
             Print( " /" );
          elif mat[i][j] = 4 then
             Print( " \\" );
          else 
             Print( mat[i][j] );
          fi;
      od;
      Print("\n");
  od;
  return; 
  end );


InstallMethod( ViewObj, [ IsVertexOfDiagram and IsVertexOfDiagramRep ],
  function( v )
    Print("Vertex(",v!.type,")");
  end );

InstallMethod( PrintObj, [ IsVertexOfDiagram and IsVertexOfDiagramRep ],
  function( v )
    Print("Vertex(",v!.type,")");
  end );

InstallMethod( ViewObj, [ IsEdgeOfDiagram and IsEdgeOfDiagramRep ],
  function( e )
    Print("Edge(",e!.edge,")");
  end );

InstallMethod( PrintObj, [ IsEdgeOfDiagram and IsEdgeOfDiagramRep ],
  function( e )
    Print("Edge(",e!.edge,")");
  end );

InstallMethod( ViewObj, [ IsRank2Residue and IsRank2ResidueRep ],
  function( e )
    Print("Rank2Residue(",e!.edge,") of ", e!.geo);
  end );

InstallMethod( PrintObj, [ IsRank2Residue and IsRank2ResidueRep  ],
  function( e )
    Print("Rank2Residue(",e!.edge,") of ", e!.geo);
  end );



#############################################################################
##
##  Coset geometries
##
#############################################################################

InstallMethod( CosetGeometry, [ IsGroup, IsHomogeneousList ],
  function( g, l )

    ##  We assume the types of the geometry index the set l, the ordering
    ##  of l is important.

    local geo, ty;
    geo := rec( group := g, parabolics := l );
    ty := NewType( GeometriesFamily, IsCosetGeometry and IsCosetGeometryRep );
    ObjectifyWithAttributes( geo, ty,
           AmbientSpace, geo,
           TypesOfElementsOfIncidenceStructure, [1..Size(l)],
           RepresentativesOfElements, l );
    return geo;
  end );

InstallMethod( Rank2Residues, [ IsIncidenceGeometry ],
  function( geo )
    
    local types, residues, res, x, ty;
    types := TypesOfElementsOfIncidenceStructure( geo );
    residues := []; 
    for x in Combinations( types, 2 ) do
        res := rec( edge := x, geo := geo );
        ty := NewType( Rank2ResidueFamily, IsRank2Residue and IsRank2ResidueRep );
        Objectify( ty, res );
        Add(residues, res);
    od;
     
    Setter( Rank2ResiduesAttr )( geo, residues );
    return residues;
  end );

InstallMethod( MakeRank2Residue, [ IsRank2Residue ],
  function( res )

    local geo, flag;
      geo:=res!.geo;
      if IsCosetGeometry(geo) then
        flag:=StandardFlagOfCosetGeometry(geo){Difference(TypesOfElementsOfIncidenceStructure(geo),res!.edge)};
        res!.residuegeometry:=CanonicalResidueOfFlag(geo,flag);
      else
        Error("Don't know how to compute residue!\n");
      fi;
end );

InstallGlobalFunction( OnCosetGeometryElement,
  function( c, t )
    return Wrap(c!.geo, c!.type, OnRight(c!.obj, t));
  end ); 

InstallMethod( ElementsOfIncidenceStructure, [IsCosetGeometry, IsPosInt],
  function( cg, j )
    local vars;
    vars := rec( geometry := cg, type := j );
    Objectify(
      NewType( ElementsCollFamily, IsElementsOfIncidenceStructure and
                                IsAllElementsOfCosetGeometry and
                                IsAllElementsOfCosetGeometryRep),
        vars );
    return vars;
  end );

InstallMethod(Size, [IsAllElementsOfCosetGeometry],
  function( vs )
    local cg;
    cg := vs!.geometry;
    return Index(cg!.group, cg!.parabolics[vs!.type]);
  end );

InstallMethod( Wrap, "for a coset geometry and an object (coset)",
  [IsCosetGeometry, IsPosInt, IsObject],
  function( geo, type, o )
    local w;
    w := rec( geo := geo, type := type, obj := o );
    Objectify( NewType( ElementsOfIncidenceStructureFamily,
      IsElementOfCosetGeometryRep and IsElementOfCosetGeometry ), w );
    return w;
  end );

InstallMethod(Iterator, "for elements of a coset geometry",
        [IsAllElementsOfCosetGeometry],
  function( vs )
    local cg, j, g, h, iter, newiter;
    cg := vs!.geometry;
    j := vs!.type;
    g := cg!.group;
    h := cg!.parabolics[j];
    iter := Iterator(RightCosets(g,h));    
    ## Wrap 'em up
    newiter := IteratorByFunctions( rec(
            NextIterator := function( i )
              local x;
              x := NextIterator(i!.S);
              return Wrap(cg, j, x);
            end,
            IsDoneIterator := function(i)
              return IsDoneIterator(i!.S);
            end,
            ShallowCopy := function(i)
              return rec( S := ShallowCopy(i!.S) );
            end,
            S := iter ));
    return newiter;
  end );

InstallMethod( IsIncident, "for varieties of a coset geometry", 
              [IsElementOfCosetGeometry, IsElementOfCosetGeometry],
  function( x, y )
    local vx, vy, tx, ty, g, h, k;
    vx := x!.obj; vy := y!.obj;
    tx := x!.type; ty := y!.type;
    if tx = ty then 
       return false;
    fi;
    ## Let Ha and Kb be two right cosets, and let g=ab^-1.
    ## Then Ha and Kb intersect if and only if g is an element of the double
    ## coset HK.
    g := Representative(vx) * Representative(vy)^-1;
    h := ActingDomain(vx);
    k := ActingDomain(vy);
    return g in DoubleCoset(h, One(h), k);
  end );

InstallMethod( ParabolicSubgroups, "for coset geometries",
               [ IsCosetGeometry ],  cg -> cg!.parabolics );

InstallMethod( AmbientGroup, "for coset geometries",
               [ IsCosetGeometry ],  cg -> cg!.group );

InstallMethod( BorelSubgroup, "for coset geometries",
               [ IsCosetGeometry ],  cg -> Intersection(cg!.parabolics) );

InstallMethod( IsFlagTransitiveGeometry, "for coset geometries",
               [ IsCosetGeometry ],
  function( cg )
    ## From Buekenhout's chapter in the "Handbook of Incidence Geometry"
    ## a coset geometry with parabolics {G1} is flag-transitive if
    ## (G1 G2) \cap (G1 G3) \cap ... \cap (G1 Gn) = G1( G2 \cap...\cap Gn),
    ## (G2 G3) \cap ...\cap (G2 Gn) = G2( G3 \cap ... \cap Gn),
    ## ..., (G(n-2) G(n-1)) \cap (G(n-2) G(n)) = G(n-2)( G(n-1)\cap Gn )
    ## For each case, we have that the right-hand side is contained in 
    ## the left-hand side, so it suffices to compute sizes of the entities
    ## in each equation.

    local g, parabolics, gi, gj, orb, trans, rank, 
          left, int, newint, act, right, i, j;
  
    g := cg!.group;
    parabolics := cg!.parabolics;
    rank := Size(parabolics);

    ## Note that |G1( G2 \cap...\cap Gn)| = |( G2 \cap...\cap Gn) : (G1\cap G2 \cap...\cap Gn)|.
    ## So to efficiently compute the right hand sides, we do the "shortest one" first
    ## and iterate. To compute the left hand sides is much more difficult.
    ## For the first case, we need to compute the intersections of the orbits
    ## of G2, G3, ..., Gn on the trivial coset G1 in the action of G on
    ## the right cosets of G1.
   
    ## Start from the end, where the number of intersections are shortest
    ## and go upwards.

    int := Intersection( parabolics[rank - 1], parabolics[rank] );

    for i in [2..rank-1] do
        # compute right-hand side
        newint := Intersection( int, parabolics[rank - i] );
        right := Size(int) / Size(newint);
       
        # compute left-hand side
        gi := parabolics[rank-i];
        act := FactorCosetAction(g, gi);
        orb := [1..Index(g,gi)];
        for j in [rank-i+1..rank] do
            gj := Image(act, parabolics[j]);
            orb := Intersection(orb, Orbit(gj, 1)); 
        od;
        left := Size(orb);
        if left > right then 
           return false;
        fi;
        int := newint;
    od;

    return true;
  end );

InstallMethod( IsFirmGeometry, "for coset geometries",
               [ IsCosetGeometry ],
  function( cg )
    local parabolics, pis, without_i, int, borel, i;
    parabolics := ParabolicSubgroups( cg );
    pis := [];
    for i in [1..Size(parabolics)] do
        without_i := Difference( [1..Size(parabolics)], [ i ] );
        int := Intersection( parabolics{without_i} );
        Add(pis, int);
    od;

    if HasIsFlagTransitiveGeometry( cg ) and IsFlagTransitiveGeometry( cg ) then
       ## In this case, the coset geometry is firm
       ## if and only if the borel subgroup is not
       ## contained in any Pi = \cap{Hj: j<>i}.

       borel := BorelSubgroup( cg );
       return not ForAny(pis, t -> IsSubset(t, borel));
    else
       ## In this case, the coset geometry is firm
       ## if and only if for every i, Pi is not contained in Hi
       ## (where the Hi's are the parabolics).
       
       return not ForAny([1..Size(parabolics)], i -> 
              IsSubset(parabolics[i], pis[i]) );
    fi;
  end );

InstallMethod( IsConnected, "for coset geometries",
               [ IsCosetGeometry ],
  function( cg )
    ## A coset geometry is connected if and only if the
    ## parabolics generate the group.

    local parabolics, g, gens;
    g := cg!.group;
    parabolics := ParabolicSubgroups( cg );
    gens := Union( List(parabolics, GeneratorsOfGroup) );
    return Group( gens ) = g;
  end );


InstallMethod( IsResiduallyConnected, "for coset geometries",
               [ IsCosetGeometry ],
  function( cg )
    ## A coset geometry is residually connected if and only
    ## if for every subset J of the types I with |I-J|>1, we have
    ## \cap{Hj: j in J} = < \cap{Hi: i in J-{j}} : j in I-J >

    # to be completed
    return true;
  end );

InstallMethod( StandardFlagOfCosetGeometry, "for coset geometries",
	       [ IsCosetGeometry ],
  function(cg)
  local parabolics, flag;

  parabolics:=cg!.parabolics;
  flag:=List([1..Size(parabolics)], i -> Wrap(cg,i,parabolics[i]));
  return flag;
end );

InstallMethod( FlagToStandardFlag, "for coset geometries",
                [ IsCosetGeometry, IsHomogeneousList ],
  function( cg, flag )
  
    ## This operation returns an element g which maps 
    ## "flag" to the standard flag consisting of trivial
    ## cosets of parabolic subgroups.
    ## Suppose [G(1) a(1), ..., G(n) a(n)] is a flag.
    ## Let x(1) = a(1)^-1 and then define x(i) inductively
    ## by the condition
    ##  x(i)^-1 in G(1) \cap G(2)\cap ... \cap G(i-1)\cap G(i)a(i)x(1)x(2)...x(i-1)
    ## Then g = x(1)x(2)...x(n) maps our flag to [G(1),...,G(n)].
    
    local g, x, reps, pabs, i, int, ginv;
    
    # initialise
    reps := List(flag, t -> Representative(t!.obj) );
    pabs := ParabolicSubgroups( cg ){ List(flag, t->t!.type) };
    g := reps[1]^-1;
    int := pabs[1];
    for i in [2..Size(flag)] do
        if i > 2 then 
           int := Intersection(int, pabs[i-1]); 
        fi;
        
        ginv := g^-1;
        repeat
            x := PseudoRandom(int);
        until x * ginv in flag[i]!.obj;
        
        g := g * x^-1;
    od;    
    return g;
  end );


InstallMethod( CanonicalResidueOfFlag, "for coset geometries",
               [ IsCosetGeometry, IsHomogeneousList ],

  function( cg, flag )

    ## return coset geometry which is isomorphic to residue of the given flag
    ## have non-empty intersection with all of the elements
    ## of flag

    local typesflag, types, parabolics, i, resg, respabs;

    if IsFlagTransitiveGeometry( cg ) then
       typesflag := Set(flag, t->t!.type);
       if IsEmpty( typesflag ) then
         return cg;
       fi;
       types := Difference( TypesOfElementsOfIncidenceStructure(cg), typesflag);
       if IsEmpty( types ) then
          return CosetGeometry( Group(()), [] );
       fi;
 
       parabolics := ParabolicSubgroups( cg ){types};;
           
       resg := Intersection( ParabolicSubgroups( cg ){typesflag} );
       respabs := [];
       for i in parabolics do
          Add( respabs, Intersection( resg, i ) );
       od;
     else
       Error("not implemented for not flag-transitive geometries");
     fi;
     
     return CosetGeometry( resg, respabs ); 
  end );


InstallMethod( ResidueOfFlag, "for coset geometries",
               [ IsCosetGeometry, IsHomogeneousList ],

  function( cg, flag )

    ## return all right cosets of parabolics which
    ## have non-empty intersection with all of the elements
    ## of flag

    local typesflag, types, r, parabolics, i, resg, respabs;

    if IsFlagTransitiveGeometry( cg ) then
       typesflag := Set(flag, t->t!.type);
       if IsEmpty( typesflag ) then
         return cg;
       fi;
       types := Difference( TypesOfElementsOfIncidenceStructure(cg), typesflag);
       if IsEmpty( types ) then
          return CosetGeometry( Group(()), [] );
       fi;
 
       parabolics := ParabolicSubgroups( cg ){types};;
           
       resg := Intersection( ParabolicSubgroups( cg ){typesflag} );
       respabs := [];
       for i in parabolics do
          Add( respabs, Intersection( resg, i ) );
       od;
       
       ## up to this point, we have the residue of the standard flag, 
       ## and so we need to map back to our original flag
       
       ## The mathematics here needs checking/testing!
       
       r := FlagToStandardFlag( cg, flag );  
       respabs := List(respabs, t -> t^r);
       resg := resg^r;
     else
       Error("not implemented for not flag-transitive geometries");
     fi;
     
     return CosetGeometry( resg, respabs ); 
  end );




InstallMethod( IncidenceGraph, [ IsCosetGeometry ],
  function( geo )
  
  ## This operation returns the multiparitite incidence graph
  ## associated to "geo", and it also sets a mutable attribute
  ## IncidenceGraphAttr which can be called hence.  
  
    local g, vars, gamma, allvars, reps;

    if not "grape" in RecNames(GAPInfo.PackagesLoaded) then
       Error("You must load the Grape package\n");
    fi;

    g := geo!.group;

    allvars := List( [1..Size(TypesOfElementsOfIncidenceStructure(geo))],
       i -> ElementsOfIncidenceStructure(geo, i) );
#    reps:=RepresentativesOfElements(geo);

    vars := Concatenation( List(allvars, AsList) );
    gamma := Graph( g, vars, OnCosetGeometryElement, 
                function(x,y) return x in y; end, true);
    
    Setter( IncidenceGraphAttr )( geo, gamma );
    return gamma;
  end );

#############################################################################
# View methods:
#############################################################################

InstallMethod( ViewObj, [ IsDiagram and IsDiagramRep ],
  function( diag )
    Print("< Diagram >");
  end );

InstallMethod( ViewObj, [ IsDiagram and IsDiagramRep and HasGeometryOfDiagram],
  function( diag )
    local geo;
    geo := GeometryOfDiagram( diag );
    Print("< Diagram of ", geo ," >");
  end );


InstallMethod( ViewObj, [ IsCosetGeometry and IsCosetGeometryRep ],
  function( geo )
    Print("CosetGeometry( ", geo!.group, " )");
  end );

InstallMethod( PrintObj, [ IsCosetGeometry and IsCosetGeometryRep ],
  function( geo )
    Print("CosetGeometry( ", geo!.group, " , ", geo!.parabolics , " )");
  end );

InstallMethod( ViewObj, [ IsAllElementsOfCosetGeometry and
  IsAllElementsOfCosetGeometryRep ],
  function( vs )
    Print("<varieties of type ", vs!.type," of ");
    ViewObj(vs!.geometry);
    Print(">");
  end );

InstallMethod( PrintObj, [ IsAllElementsOfCosetGeometry and
  IsAllElementsOfCosetGeometryRep ],
  function( vs )
    Print("ElementsOfIncidenceStructure( ",vs!.geometry," , ",vs!.type,")");
  end );

InstallMethod( ViewObj, [ IsElementOfCosetGeometry ],
  function( v )
    Print("<variety of type ", v!.type," of ");
    ViewObj(v!.geo);
    Print(">");
  end );

InstallMethod( PrintObj, [ IsElementOfCosetGeometry ],
  function( v )
    Print(v!.obj);
  end );

#############################################################################
# Methods for diagrams of geometries.
#############################################################################

InstallMethod( DiagramOfGeometry, [ IsProjectiveSpace ],
  function( geo )
    local vertices, types, x, v, edges, newedges, e, way, diagram, q;

    vertices := [];
    types := TypesOfElementsOfIncidenceStructure( geo );
    q := Size( geo!.basefield );

  ## Wrapping up... 
    for x in types do
      v := rec( type := x );
      Objectify( NewType( VertexOfDiagramFamily, IsVertexOfDiagram and
                      IsVertexOfDiagramRep ), v);
      SetOrderVertex(v, q);
      Add( vertices, v );
    od;

    edges := List([1..Size(types)-1], i -> vertices{[i,i+1]} );
    newedges := [];

  ## Wrapping up...
    for x in edges do
      e := rec( edge := x );
      Objectify( NewType( EdgeOfDiagramFamily, 
                      IsEdgeOfDiagram and IsEdgeOfDiagramRep ), e);
      SetResidueLabelForEdge( e, "P");
      Add( newedges, e );
    od;

  ## simple path diagram

    way := List([1..Size(types)], i -> [1,i] );
    diagram := rec( vertices := vertices, edges := newedges, drawing := way );;
    Objectify( NewType( DiagramFamily, IsDiagram and IsDiagramRep ), diagram);
    SetGeometryOfDiagram( diagram, geo );
    return diagram;
  end );

InstallMethod( Rk2GeoDiameter, "for a coset geometry", [IsCosetGeometry,
IsPosInt],
  function( cg, type ) # type in {1,2}
    local parabs, d, g, gpchain;
    parabs:=ParabolicSubgroups(cg);
    g:=AmbientGroup(cg);
    d:=0;
    gpchain:=parabs[type];
    while gpchain <> g do
      d:=d+1;
      gpchain:=Set(List(Cartesian(gpchain,parabs[((type + d) mod 2) + 1]), Product));
    od;
    return d;
   end );

InstallMethod( Rank2Parameters, "for a coset geometry of rank 2", [IsCosetGeometry], 
  function(geo)

# Computes all parameters of the rank 2 geometry geo
# Returns a list of length 3 with
# - [g, dp, dl] as first entry. That is a list with (half) the girth,
# the point- and the line diameter of the geometry.
# - [sp, np] as second entry. That is the point order and the number of points
# - [sl, nl] as third entry. That is the line order and number of lines.
#
# For the moment this only works for a coset geometry geo because IncidenceGraph is only inplemented for that kind of geometries and we also compute the number of elements as indices of parabolic subgroups.
#

#  ***** Check that the input is kosher ******

  local incgr, reps, g, dp, dl, sp, sl, np, nl, locinfo;

  incgr:=IncidenceGraph(geo);
  reps:=incgr!.representatives; #we assume the GRAPE graph has an element of each type in its 'representative' field. This is indeed the case when the geometry is flag-transitive.
  locinfo:=LocalInfo(incgr,reps[1]); #from GRAPE
  g:=locinfo!.localGirth / 2;
  dp:=locinfo!.localDiameter;
  sl:=locinfo!.localParameters[1][3]-1;
  locinfo:=LocalInfo(incgr,reps[2]);
  dl:=locinfo!.localDiameter;
  sp:=locinfo!.localParameters[1][3]-1;
  np:=Index(geo!.group, geo!.parabolics[1]);
  nl:=Index(geo!.group, geo!.parabolics[2]);
  return[[g,dp,dl], [sp,np], [sl,nl]];
end );

