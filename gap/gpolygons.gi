#############################################################################
##
##  gpolygons.gi              FinInG package
##                                                              John Bamberg
##                                                              Anton Betten
##                                                             Philippe Cara
##                                                              Jan De Beule
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
##  Implementation stuff for generalised polygons.
##
#############################################################################

########################################
#
# Things To Do:
#
# - Twisted triality hexagon (test it, but hard to)
# - Iterators for Kantor families, or enumerators maybe
# - iterators for generalised hexagons?
#   at the moment we just take the full set, could be difficult!
# - join and meet operations
#
# Documentation check-list
# - SplitCayleyHexagon
# - TwistedTrialityHexagon
# - EGQByKantorFamily
# - IsKantorFamily
# - IsqClan
# - EGQByqClan
# - BLTSetByqClan
# - EGQByBLTSet
#
########################################


#############################################################################
#
#   Basic methods and iterators
#
#############################################################################

InstallMethod( ElementsOfIncidenceStructure, [IsGeneralisedPolygon, IsPosInt],
  function( gp, j )
    local s, t, sz;
    if j in [1,2] then 
       s := Order(gp)[j]; t := Order(gp)[3-j];
    else 
       Error("Incorrect type value");
    fi;
    if IsProjectivePlane(gp) then
       sz := s^2 + s + 1;
    elif IsGeneralisedQuadrangle(gp) then 
       sz := (1+s)*(1+s*t);
    elif IsGeneralisedOctogon(gp) then
       sz := (1+s)*(1+s*t+s^2*t^2+s^3*t^3);
    fi;        

    return Objectify(
      NewType( ElementsCollFamily, IsElementsOfIncidenceStructure and
                                IsAllElementsOfGeneralisedPolygon and
                                IsAllElementsOfGeneralisedPolygonRep),
        rec( geometry := gp, type := j, size := sz )
      );
  end );

InstallMethod( ElementsOfIncidenceStructure, [IsElationGQByKantorFamily, IsPosInt],
  function( gp, j )
    local s, t;
    if j in [1,2] then 
       s := Order(gp)[j]; t := Order(gp)[3-j];
    else 
       Error("Incorrect type value");
    fi;
    return Objectify(
      NewType( ElementsCollFamily, IsElementsOfIncidenceStructure and
                                IsAllElementsOfKantorFamily and
                                IsAllElementsOfGeneralisedPolygonRep),
        rec( geometry := gp, type := j, size := (1+s)*(1+s*t) )
      );
  end );

InstallMethod( ElementsOfIncidenceStructure, [IsGeneralisedHexagon, IsPosInt],
  function( gp, j )
    local s, t, sz;
    if j in [1,2] then 
       s := Order(gp)[j]; t := Order(gp)[3-j];
    else 
       Error("Incorrect type value");
    fi;
    return Objectify(
      NewType( ElementsCollFamily, IsElementsOfIncidenceStructure and
                                IsAllElementsOfGeneralisedHexagon and
                                IsAllElementsOfGeneralisedPolygonRep),
        rec( geometry := gp, type := j, size := (1+s)*(1+s*t+s^2*t^2) )
      );
  end );

InstallMethod( Points, [IsGeneralisedPolygon],
  function( gp )
    return ElementsOfIncidenceStructure(gp, 1);
  end);

InstallMethod( Lines, [IsGeneralisedPolygon],
  function( gp )
    return ElementsOfIncidenceStructure(gp, 2);
  end);

InstallMethod(Size, "for elements of a generalised polygon",
        [IsAllElementsOfGeneralisedPolygon], vs -> vs!.size );

InstallMethod(Iterator, "for elements of a generalised polygon",
        [IsAllElementsOfGeneralisedPolygon],
  function( vs )
    local ps, j, vars;
    ps := vs!.geometry;
    j := vs!.type;
    if j = 1 then 
       vars := List(ps!.points, x -> Wrap(ps, 1, x));
       return IteratorList( vars );
    elif j = 2 then 
       vars := List(ps!.lines, x -> Wrap(ps, 2, x));
       return IteratorList( vars );
    else Error("Element type does not exist"); return;
    fi;
  end );

InstallMethod(Iterator, "for elements of a generalised hexagon",
        [IsAllElementsOfGeneralisedHexagon],
  function( vs )
    local ps, j, vars, coll, reps;
    ps := vs!.geometry;
    j := vs!.type;
    coll := CollineationGroup( ps );
    reps := RepresentativesOfElements( ps );
    vars := Enumerate(Orb(coll, reps[j], OnProjSubspaces));
    return IteratorList( vars );
  end );

InstallMethod(Iterator, "for elements of an EGQ defined by a Kantor family",
        [IsAllElementsOfKantorFamily],

##  We can do much better here.
##  Perhaps we need to think about implementing enumerators/iterators
##  for Kantor families. One day there might be enumerators for cosets?

  function( vs )
    local ps, j, vars;
    ps := vs!.geometry;
    j := vs!.type;
    if j = 1 then 
       vars := ps!.points;
       return IteratorList( vars );
    elif j = 2 then 
       vars := ps!.lines;
       return IteratorList( vars );
    else Error("Element type does not exist"); return;
    fi;
  end );

InstallMethod( IsIncident, "for elements of a generalised polygon", 
              [IsElementOfGeneralisedPolygon, IsElementOfGeneralisedPolygon],
  function( x, y )
    local inc;
    inc := x!.geo!.incidence;
    return inc(x!.obj, y!.obj);
  end );

InstallMethod( IsIncident, "for elements of a Kantor family",
              [IsElementOfKantorFamily, IsElementOfKantorFamily],
  function( x, y )
    local inc;
    inc := x!.geo!.incidence;
    return inc(x, y);
  end );

InstallMethod( Wrap, "for a generalised polygon and an object",
  [IsGeneralisedPolygon, IsPosInt, IsObject],
  function( geo, type, o )
    local w;
    w := rec( geo := geo, type := type, obj := o );
    Objectify( NewType( ElementsOfIncidenceStructureFamily,   # ElementsFamily,
          IsElementOfIncidenceStructureRep and IsElementOfGeneralisedPolygon ), w );
    return w;
  end );

InstallMethod( Wrap, "for an EGQ (Kantor family), and an object",
  [IsElationGQByKantorFamily, IsPosInt, IsPosInt, IsObject],
  function( geo, type, class, o )
    local w;
    w := rec( geo := geo, type := type, class := class, obj := o );
    Objectify( NewType( ElementsOfIncidenceStructureFamily,   #ElementsFamily,
      IsElementOfKantorFamilyRep and IsElementOfKantorFamily ), w );
    return w;
  end );

#############################################################################
#
#  Generalised Hexagons
#
#############################################################################

InstallMethod( SplitCayleyHexagon, "input is a finite field", 
               [ IsField and IsFinite ],
  function( f )
    local m, mp, ml, gens, frob, newgens, geo, ty, inc,
          coll, repline, reppoint, hq, hvm, ps,
          hvmc, c, change, hvmform, form, nonzerof, x;
    # The generators of Ree(q) were taken from Hendrik 
    # van Maldeghem's book: "Generalized Polygons".
    # Lines with ** are where there are mistake's in
    # Hendrik's book (see Alan Offer's thesis).

    if IsOddInt(Size(f)) then
       m:=[[ 0, 0, 0, 0, 0, 1, 0],
           [ 0, 0, 0, 0, 0, 0, 1],
           [ 0, 0, 0, 0, 1, 0, 0],
           [ 0, 0, 0, -1, 0, 0, 0],               ## **
           [ 0, 1, 0, 0, 0, 0, 0],
           [ 0, 0, 1, 0, 0, 0, 0],
           [ 1, 0, 0, 0, 0, 0, 0]]*One(f);  
       ConvertToMatrixRep(m, f);
       mp:=d->[[1,  0,  0,  0,  0,  d,  0],  
           [0,  1,  0,  0, -d,  0,  0],  
           [0,  0,  1,  0,  0,  0,  0],  
           [0,  0,  2*d,  1,  0,  0,  0],         ## **
           [0,  0,  0,  0,  1,  0,  0],  
           [0,  0,  0,  0,  0,  1,  0],  
           [0,  0,d^2,  d,  0,  0,  1]]*One(f);   ## **
       ml:=d->[[1, -d,  0,  0,  0,  0,  0],  
           [0,  1,  0,  0,  0,  0,  0],  
           [0,  0,  1,  0,  0,  0,  0],  
           [0,  0,  0,  1,  0,  0,  0],  
           [0,  0,  0,  0,  1,  0,  0],  
           [0,  0,  0,  0,  d,  1,  0],  
           [0,  0,  0,  0,  0,  0,  1]]*One(f);
    
       ## the corresponding sesquilinear form here for 
       ## q odd is the matrix 
       ## [[0,0,0,0,1,0,0],[0,0,0,0,0,1,0],
       ## [0,0,0,0,0,0,1],[0,0,0,-2,0,0,0],
       ## [1,0,0,0,0,0,0],[0,1,0,0,0,0,0],[0,0,1,0,0,0,0]];

       ## now we transfer elements and automorphisms
       ## to the parabolic quadric given by FinInG 
       ps := ParabolicQuadric(6, f);

       ## this is Hendrik's form
       hvm := List([1..7], i -> [0,0,0,0,0,0,0]*One(f));
       hvm{[1..3]}{[5..7]} := IdentityMat(3, f);
       hvm{[5..7]}{[1..3]} := IdentityMat(3, f);
       hvm[4][4] := -2*One(f);
       hvmform := FormByMatrix(hvm, f, "orthogonal");
   
       hvmc := BaseChangeToCanonical( hvmform );;    
       change := hvmc^-1;;
       nonzerof := AsList(f){[2..Size(f)]};
       gens := Union([m], List(nonzerof, mp), List(nonzerof, ml));
       gens := List(gens, t -> change^-1 * t * change);;
       for x in gens do
          ConvertToMatrixRep(x, f);
       od;
       frob := FrobeniusAutomorphism(f); 
       newgens := List(gens, x -> ProjectiveSemilinearMap(x, f));  
       if not IsPrimeInt(Size(f)) then 
          Add(newgens, ProjectiveSemilinearMap( IdentityMat(7,f), frob, f )); 
       fi;  ## this is OK, I think
       coll := GroupWithGenerators(newgens);
       reppoint := RepresentativesOfElements(ps)[1];

       ## Hendrik's canonical line is <(1,0,0,0,0,0,0), (0,0,0,0,0,0,1)>
       repline := [[1,0,0,0,0,0,0], [0,0,0,0,0,0,1]] * One(f) * change;
       ConvertToMatrixRep(repline, f);
       repline := VectorSpaceToElement(ps, repline);
    else
       ## Here we embed the hexagon in W(5,q)
       m:=[[ 0, 0, 0, 0, 1, 0],
           [ 0, 0, 0, 0, 0, 1],
           [ 0, 0, 0, 1, 0, 0],
           [ 0, 1, 0, 0, 0, 0],
           [ 0, 0, 1, 0, 0, 0],
           [ 1, 0, 0, 0, 0, 0]]*One(f);  
       ConvertToMatrixRep( m );
       mp:=d->[[1,  0,  0,  0,  d,  0],  
           [0,  1,  0,  d,  0,  0],  
           [0,  0,  1,  0,  0,  0],  
           [0,  0,  0,  1,  0,  0],  
           [0,  0,  0,  0,  1,  0],  
           [0,  0,d^2,  0,  0,  1]]*One(f);  
       ml:=d->[[1,  d,  0,  0,  0,  0],  
           [0,  1,  0,  0,  0,  0],  
           [0,  0,  1,  0,  0,  0],  
           [0,  0,  0,  1,  0,  0],  
           [0,  0,  0,  d,  1,  0],  
           [0,  0,  0,  0,  0,  1]]*One(f);
       ## this is Hendrik's form
       hvm := List([1..6], i -> [0,0,0,0,0,0]*One(f));
       hvm{[1..3]}{[4..6]} := IdentityMat(3, f);
       hvm{[4..6]}{[1..3]} := IdentityMat(3, f);       
       hvmform := FormByMatrix(hvm, f, "symplectic");   
       hvmc := BaseChangeToCanonical( hvmform );;    
       ps := SymplecticSpace(5, f);
       change := hvmc^-1;;
       nonzerof := AsList(f){[2..Size(f)]};
       gens := Union([m], List(nonzerof,mp), List(nonzerof,ml));
       gens := List(gens, t -> change^-1 * t * change);
       for x in gens do
          ConvertToMatrixRep(x,f);
       od;
       frob := FrobeniusAutomorphism(f); 
       newgens := List(gens, x -> ProjectiveSemilinearMap(x, f));  
       if not IsPrimeInt(Size(f)) then 
          Add(newgens, ProjectiveSemilinearMap( IdentityMat(6,f), frob, f )); 
       fi;
       coll := GroupWithGenerators(newgens);
       reppoint := RepresentativesOfElements(ps)[1];

       ## Hendrik's canonical line is <(1,0,0,0,0,0), (0,0,0,0,0,1)>
       repline := [[1,0,0,0,0,0], [0,0,0,0,0,1]] * One(f) * change;
       ConvertToMatrixRep(repline, f);
       repline := VectorSpaceToElement(ps, repline);
    fi; 

    if IsPrime(Size(f)) then
       SetName(coll, Concatenation("G_2(",String(Size(f)),")") );
    else
       SetName(coll, Concatenation("G_2(",String(Size(f)),").", String(Order(frob))) );
    fi;

    inc := function(x, y) return x!.obj in y!.obj; end;
    geo := rec( points := [], lines := [], incidence:= inc);
    ty := NewType( GeometriesFamily,
             IsGeneralisedHexagon and IsGeneralisedPolygonRep );
    Objectify( ty, geo );
    SetAmbientSpace(geo, ps);
    SetOrder(geo, [Size(f), Size(f)]);
    SetCollineationAction(coll, OnProjSubspaces);
    SetCollineationGroup(geo, coll);
    SetTypesOfElementsOfIncidenceStructure(geo, ["point","line"]);
    SetRepresentativesOfElements(geo, [reppoint, repline]);
    return geo;
  end );

InstallMethod( SplitCayleyHexagon, "input is a prime power", [ IsPosInt ],
  function( q )
    return SplitCayleyHexagon(GF(q));
  end );

InstallMethod( TwistedTrialityHexagon, "input is a finite field", 
                [ IsField and IsFinite ],
  function( f )
    local m, mp, ml, gens, frob, newgens, geo, ty, inc, x,
          coll, points, lines, repline, hq, hvm, ps,
          hvmc, c, change, hvmform, form, nonzerof, q, pps, sigma,
          reppoint;
    # The generators of 3D4(q) were taken from Hendrik 
    # van Maldeghem's book: "Generalized Polygons".

    ## field must be GF(q^3);
    q := RootInt(Size(f), 3);
    if not IsInt(q) then
       Error("Field order must be a cube of a prime power");
    fi;
    pps := PrimePowersInt( Size(f) );
    frob := FrobeniusAutomorphism(f);
    sigma := frob^LogInt(q,pps[1]);    ## automorphism of order 3

    m:=[[ 0, 0, 0, 0, 0, 1, 0, 0],
        [ 0, 0, 0, 0, 0, 0, 1, 0],
        [ 0, 0, 0, 0, 1, 0, 0, 0],
        [ 0, 0, 0, 0, 0, 0, 0, 1],   
        [ 0, 1, 0, 0, 0, 0, 0, 0],
        [ 0, 0, 1, 0, 0, 0, 0, 0],
        [ 1, 0, 0, 0, 0, 0, 0, 0],
        [ 0, 0, 0, 1, 0, 0, 0, 0]]*One(f);  
    ConvertToMatrixRep(m, f);
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

       ## now we transfer elements and automorphisms
       ## to the hyperbolic quadric given by FinInG 
    ps := HyperbolicQuadric(7, f);

       ## this is Hendrik's form

    hvm := List([1..8], i -> [0,0,0,0,0,0,0,0]*One(f));
    hvm{[1..4]}{[5..8]} := IdentityMat(4, f);
    hvmform := QuadraticFormByMatrix(hvm, f);
    hvmc := BaseChangeToCanonical( hvmform );;    
    change := hvmc^-1;;

    nonzerof := AsList(f){[2..Size(f)]};
    gens := Union([m], List(nonzerof, mp), List(nonzerof, ml));
    gens := List(gens, t -> change^-1 * t * change);;
    for x in gens do
        ConvertToMatrixRep(x, f);
    od;
    newgens := List(gens, x -> ProjectiveSemilinearMap(x,f));
    coll := GroupWithGenerators(newgens);

       ## Hendrik's canonical point is <(1,0,0,0,0,0,0,0)>
    ps := HyperbolicQuadric(7, f);
    reppoint := ([1,0,0,0,0,0,0,0] * One(f)) * change;
    ConvertToVectorRep(reppoint, f);
    reppoint := VectorSpaceToElement(ps, reppoint);

       ## Hendrik's canonical line is <(1,0,0,0,0,0,0,0), (0,0,0,0,0,0,1,0)>
    repline := ([[1,0,0,0,0,0,0,0], [0,0,0,0,0,0,1,0]] * One(f)) * change;
    ConvertToMatrixRep(repline, f);
    repline := VectorSpaceToElement(ps, repline);
  
    SetName(coll, Concatenation("4D_3(",String(q),")") );
    
    inc := function(x, y) return x!.obj in y!.obj; end;
    geo := rec( points := [], lines := [], incidence:= inc);
    ty := NewType( GeometriesFamily,
             IsGeneralisedHexagon and IsGeneralisedPolygonRep );
    Objectify( ty, geo );
    SetAmbientSpace(geo, ps);
    SetOrder(geo, [q^3, q]);
    SetCollineationAction(coll, OnProjSubspaces);
    SetCollineationGroup(geo, coll);
    SetTypesOfElementsOfIncidenceStructure(geo, ["point","line"]);
    SetRepresentativesOfElements(geo, [reppoint, repline]);
    return geo;
  end );

InstallMethod( TwistedTrialityHexagon, "input is a prime power", [ IsPosInt ],
  function( q )
    return TwistedTrialityHexagon(GF(q));
  end );

#############################################################################
#
#  Kantor Families and associated EGQ's
#
#############################################################################

InstallMethod( \=, [IsElementOfKantorFamily, IsElementOfKantorFamily], 
  function( a, b ) 
    return a!.obj = b!.obj; 
  end );

InstallMethod( \<, [IsElementOfKantorFamily, IsElementOfKantorFamily], 
  function( a, b ) 
    if a!.type <> b!.type then return a!.type < b!.type;
    else
       if a!.class <> b!.class then return a!.class < b!.class;
       else return a!.obj < b!.obj; 
       fi;
    fi;
  end );

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

InstallMethod( EGQByKantorFamily, [IsGroup, IsList, IsList],
  function( g, f, fstar)
    local pts1, pts2, pts3, ls1, ls2, inc, 
          x, y, geo, ty, points, lines, pointreps, linereps;
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

    geo := rec( points := [], lines := [], incidence := inc );
    ty := NewType( GeometriesFamily, IsElationGQByKantorFamily and IsGeneralisedPolygonRep );  
    Objectify( ty, geo );

    Info(InfoFinInG, 1, "Computing points from Kantor family...");

    ## wrapping
    pts1 := List(g, x -> Wrap(geo,1,1,x));
    pts2 := Set(fstar, b -> Set(RightCosets(g,b),x->[b,CanonicalRightCosetElement(b, Representative(x))]));
    pts2 := Concatenation(pts2);
    pts2 := List(pts2, x -> Wrap(geo,1,2,x));

    Info(InfoFinInG, 1, "Computing lines from Kantor family...");

    ls1 := Set(f, a -> Set(RightCosets(g,a), x -> [a,CanonicalRightCosetElement(a, Representative(x))]));
    ls1 := Concatenation(ls1); 
    ls1 := List(ls1, x -> Wrap(geo,2,1,x)); ## this is a strictly sorted list

    ## symbols (note that we're making incidence easier here)
    ls2 := Set(fstar, x -> Wrap(geo, 2, 2, x)); 
    pts3 := [ Wrap(geo, 1, 3, 0) ];

    points := Concatenation(pts1,pts2,pts3); 
    lines := Concatenation(ls1, ls2);
    pointreps := Concatenation( [pts1[1]], Set(fstar, b -> Wrap(geo,1,2,[b, One(b)])), pts3);
    linereps := Concatenation(Set(f, a -> Wrap(geo,2,1,[a, One(g)])), ls2);
    
    geo!.points := points;
    geo!.lines := lines;
    SetBasePointOfEGQ( geo, pts3[1] );
    SetAmbientSpace(geo, geo);
    SetOrder(geo, [Index(g,fstar[1]), Size(f)-1]);
    SetCollineationAction(g, OnKantorFamily);
    SetElationGroup(geo, g);
    SetTypesOfElementsOfIncidenceStructure(geo, ["point","line"]);
    ## Orbit reps:
    SetRepresentativesOfElements(geo, Concatenation(pointreps,linereps));
    return geo;
  end );

InstallMethod( IsKantorFamily, [IsGroup, IsList, IsList],
  function( g, f, fstar)
    local flag, a, b, c, ab, astar, tplus1;
    tplus1 := Size(f);
    flag := true;
    if not ForAll([1..Size(fstar)], x -> IsSubgroup(fstar[x], f[x])) then
       Error( "second and third arguments are incompatile");
       return;
    fi;
    if not ForAll(fstar, x -> IsSubgroup(g, x)) then
       Error( "elements of second argument are not subgroups of first argument" );
       return;
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
#
#   q-Clans and EGQ's made from them
#
#############################################################################

InstallMethod( IsAnisotropic, [IsFFECollColl,  IsField and IsFinite],
  function( m, f )
    local pairs, o;
    o := Zero(f);
    pairs := Difference(AsList(f^2),[[o,o]]);
    return ForAll(pairs, x -> x * m * x <> o);
  end );

InstallMethod( IsqClan, "input are 2x2 matrices", 
              [ IsFFECollCollColl,  IsField and IsFinite],
  function( clan, f )
    return ForAll(Combinations(clan,2), x -> IsAnisotropic(x[1]-x[2], f));
  end );

InstallMethod( qClan, [ IsFFECollCollColl, IsField ],
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

InstallMethod( ViewObj, [ IsqClanObj and IsqClanRep ],
  function( x )
    Print("<q-clan over ",x!.basefield,">");
  end );

InstallMethod( PrintObj, [ IsqClanObj and IsqClanRep ],
  function( x )
    Print("qClan( ", x!.matrices, ", ", x!.basefield , ")");
  end );

InstallOtherMethod( AsList, [IsqClanObj and IsqClanRep],
  function( qclan )
    return qclan!.matrices;
  end );

InstallOtherMethod( AsSet, [IsqClanObj and IsqClanRep],
  function( qclan )
    return Set(qclan!.matrices);
  end );

InstallMethod( BaseField, [IsqClanObj and IsqClanRep],
  function( qclan )
    return qclan!.basefield;
  end );

InstallMethod( IsLinearqClan, [ IsqClanObj ],
  function( qclan )
    local blt;
    blt := BLTSetByqClan( qclan ); 
    return ProjectiveDimension(Span(blt)) = 2;
  end );


InstallMethod( LinearqClan, [ IsPosInt ],
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

InstallMethod( FisherThasWalkerKantorBettenqClan, [ IsPosInt ],
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

InstallMethod( KantorMonomialqClan, [ IsPosInt ],
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

InstallMethod( KantorKnuthqClan, [ IsPosInt ],
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


InstallMethod( FisherqClan, [ IsPosInt ],
  function(q)
    local f, g, clan, n, zeta, omega, squares, nonsquares, i, z, a, t, j;
    if not IsPrimePowerInt(q) or IsEvenInt(q) then
       Error("Argument must be an odd prime power");
    fi;
	squares := ShallowCopy(AsList(Group(Z(q)^2)));; Add(squares, 0*Z(q));
	nonsquares := Difference(AsList(GF(q)),squares);;
	n := First(nonsquares, t -> t-1 in squares);

	zeta := PrimitiveRoot(GF(q^2));
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



InstallMethod( FlockGQByqClan, [ IsqClanObj ],
 function( qclan )
  local f, q, mat, form, w5, p, blt, x, perp, pperp, pg5, a, bas, gens, zero, elations, action,
        projpoints, gqpoints, gqlines, gqpoints2, gqlines2, res, geo, ty, points, lines, clan,
		pgammasp, stabp, stabblt, hom, omega, imgs;
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
  for x in clan do
      Add(blt, VectorSpaceToElement(w5, [[1,0,0,0,0,0], [0,1,0,x[1][2],x[1][1],0], [0,0,1,x[2][2],x[1][2],0]] * One(f)));
  od;
    Info(InfoFinInG, 1, "Making flock GQ...");

  perp := Polarity(w5);;
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

###  stabblt := SetwiseStabilizer(stabp, OnProjSubspaces, blt)!.setstab;  

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
            m[2][1] := a; m[3][1] := b; m[4][1] := c; m[5][1] := d;
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
  gens := List(gens, t -> ProjectiveSemilinearMap(t,f));
  elations := SubgroupNC( stabblt, gens ); 
  SetElationGroup( geo, elations );
  SetCollineationAction( elations, action );

  return geo;
end );


InstallMethod( BLTSetByqClan, "input is a q-Clan", 
              [ IsqClanObj and IsqClanRep ],
  function( clan )
    ##
    ## The q-clan must consist only of symmetric matrices
    ##
    local q, i, f,  blt, m, sesq, c1, c2, change, w, ps;
    f := clan!.basefield;
    q := Size(f);
    i := One(f); 
    blt := List(clan!.matrices, t -> [i, t[2][2], -t[1][2], t[1][1],  t[1][2]^2 -t[1][1]*t[2][2]]);
    Add(blt, [0,0,0,0,1]*i);  ## last point is distinguished point.
      
    ## This BLT-set is in Q(4,q) defined by Gram matrix
    w := PrimitiveRoot(f);
    m := [[0,0,0,0,1],[0,0,0,1,0],[0,0,w^((q+1)/2),0,0],[0,1,0,0,0],[1,0,0,0,0]]*i;
    sesq := BilinearFormByMatrix(m, f);
    ps := PolarSpace( sesq );    
    return List(blt, x -> VectorSpaceToElement(ps, x)); 
end );


InstallMethod( KantorFamilyByqClan, "input is a q-Clan", 
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


InstallMethod( EGQByqClan, "input is a q-Clan", 
              [ IsqClanObj and IsqClanRep ],
  function( clan )
    local kantor;
    kantor := KantorFamilyByqClan( clan );
    
    Info(InfoFinInG, 1, "Computed Kantor family. Now computing EGQ...");
    return EGQByKantorFamily(kantor[1], kantor[2], kantor[3]);
  end );







InstallMethod( EGQByBLTSet, 
         "constructs an EGQ from a BLT-set of lines of W(3,q) via the Knarr construction",
     [IsList, IsSubspaceOfProjectiveSpace, IsSubspaceOfProjectiveSpace],

  function( blt, p, solid)
   ## The point p is a point of W(5,q).
   ## "solid" is a 3-space contained in P^perp
   ## blt is a BLT-set of lines of W(3,q)

   local w3q, f, q, w5q, perp, pperp, res, x, pg5, gqpoints, gqpoints2,
         projpoints, gqlines, gqlines2, em, blt2, pis, info,
         geo, points, lines, ty;

   w3q := blt[1]!.geo;
   f := w3q!.basefield;
   q := Size(f);
   w5q := SymplecticSpace(5, f);
   perp := Polarity(w5q);
   pperp := perp(p);
   
   ## check everything is kosher
   if not solid in pperp then
      Error("Solid is not contained in perp of point");
   fi;
   if p in solid then
      Error("Chosen point is contained in the chosen solid");
   fi;

   pg5 := AmbientSpace( w5q );
   projpoints := ElementsOfIncidenceStructure(pg5, 1);
   
   Info(InfoFinInG, 1, "Computing points(1) of Knarr construction...");
   
   gqpoints := Filtered(projpoints, x -> not x in pperp);;
   Add(gqpoints, p);

   em := NaturalEmbeddingBySubspace(w3q, w5q, solid);
   blt2 := List(blt,t->t^em);

   Info(InfoFinInG, 1, "Computing lines(1) of Knarr construction...");
  
   pis := List(blt2, l -> Span(p, l));
   gqlines := pis;
   gqpoints2 := [];

   Info(InfoFinInG, 1, "Computing points(2) of Knarr construction...");

   for x in pis do
       res := ShadowOfElement(pg5, x, 2);
       res := Filtered(res, t -> not p in t);
       Append(gqpoints2, res);
   od;
   gqpoints2 := Set(gqpoints2); 

   Info(InfoFinInG, 1, "Computing lines(2) of Knarr construction...please wait");

   gqlines2 := [];
   info := InfoLevel( InfoFinInG );
   SetInfoLevel(InfoFinInG, 0);

   for x in gqpoints2 do
       res := Planes(w5q, x);
       res := Filtered(res, t -> not t in pperp);
       Append(gqlines2, res); 
   od;
   SetInfoLevel(InfoFinInG, info);
 
   points := Concatenation(gqpoints, gqpoints2);
   lines := Concatenation(gqlines, gqlines2);

   geo := rec( points := points, lines := lines, 
                incidence := IsIncident);
   ty := NewType( GeometriesFamily, IsElationGQ and IsGeneralisedPolygonRep);
   Objectify( ty, geo );

   SetBasePointOfEGQ( geo, Wrap(geo, 1, p) );  
   SetAmbientSpace(geo, geo);
   SetOrder(geo, [q^2, q]);
   SetTypesOfElementsOfIncidenceStructure(geo, ["point","line"]);
   return geo;
 end );

InstallMethod( EGQByBLTSet, 
     "constructs an EGQ from a BLT-set of points of Q(4,q) via the Knarr construction",
     [ IsList ], 
  function( blt )   
   local q4q, f, w3q, duality, w5q, p, pg5, solid, 
         q4qcanonical, iso, bltdual, geo, bas, 
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
            m[2][1] := a; m[3][1] := b; m[4][1] := c; m[5][1] := d;
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
   gens := List(gens, t -> ProjectiveSemilinearMap(t,f));
   elations := Group( gens ); 
   SetElationGroup( geo, elations );

   action := function(el, x)
               return Wrap( geo, el!.type, OnProjSubspaces(el!.obj, x));
             end;
   SetCollineationAction( elations, action );
   return geo;
 end );


InstallMethod( DiagramOfGeometry, [ IsGeneralisedQuadrangle ],
  function( geo )
    local types, v1, v2, e, way, diagram, order;
    types := TypesOfElementsOfIncidenceStructure( geo );
    order := Order( geo );
    v1 := rec( type := types[1] );
    v2 := rec( type := types[2] );
    Objectify( NewType( VertexOfDiagramFamily, IsVertexOfDiagram and
                      IsVertexOfDiagramRep ), v1);
    Objectify( NewType( VertexOfDiagramFamily, IsVertexOfDiagram and
                      IsVertexOfDiagramRep ), v2);
    SetOrderVertex(v1, order[1]);
    SetOrderVertex(v2, order[2]);
    e := rec( edge := [v1,v2] );
    Objectify( NewType( EdgeOfDiagramFamily, 
                      IsEdgeOfDiagram and IsEdgeOfDiagramRep ), e);
    SetResidueLabelForEdge( e, "GQ");
    way := [[1,1], [1,2]];
    diagram := rec( vertices := [v1, v2], edges := [e], drawing := way );;
    Objectify( NewType( DiagramFamily, IsDiagram and IsDiagramRep ), diagram);
    SetGeometryOfDiagram( diagram, geo );
    return diagram;
  end );




#############################################################################
#
#  Projective planes
#
#############################################################################


  
InstallMethod( ProjectivePlaneByBlocks, [ IsHomogeneousList ], 
  function( blocks )
    local pts, v, q, pp, ty, i;
    pts := Union(blocks);
    ## make sure there is prime power order
    v := Size(pts);
    q := (-1 + Sqrt(-3 + 4* v)) / 2;
    if not IsPrimePowerInt(q) then
       Error("Number of points is not a prime power");
    fi;
    if not ForAll(blocks, b -> Size(b) = q + 1 ) then
       Error("Not every block has size ", q+1);
    fi;
    i := function( x, y )
      if IsSet( x ) and not IsSet( y ) then 
         return y in x;
      elif IsSet( y ) and not IsSet( x ) then
         return x in y;
      else 
         return false;
      fi;
    end;
      
    pp := rec( points := pts, lines := blocks, 
               incidence := i );
    ty := NewType( GeometriesFamily,
             IsProjectivePlane and IsGeneralisedPolygonRep );
    Objectify( ty, pp );
    SetAmbientSpace(pp, pp);
    SetOrder(pp, [q, q]);
    SetTypesOfElementsOfIncidenceStructure(pp, ["point","line"]);
    return pp;
  end );

InstallMethod( ProjectivePlaneByIncidenceMatrix, [ IsMatrix ], 
  function( mat )
    ## Rows represent blocks and columns represent points...  
    local v, q, row, blocks, pp;
    v := Size(mat);
    if not ForAll(mat, t->Size(t)=v) then
       Error("Matrix is not square");
    fi;
    
    q := (-1 + Sqrt(-3 + 4* v)) / 2;
    if not IsPrimePowerInt(q) then
       Error("Number of points is not a prime power");
    fi;
    
    blocks := [];
    for row in mat do
        Add(blocks, Positions(row,1));
    od;
    
    pp := ProjectivePlaneByBlocks( blocks );
    Setter( IncidenceMatrixOfGeneralisedPolygon )( pp, mat );
    return pp;
  end );

InstallMethod( CollineationGroup, "for a projective plane", 
             [ IsProjectivePlane and IsGeneralisedPolygonRep ],
  function( plane )
    local graph, aut, act, stab, coll;
    graph := IncidenceGraphOfGeneralisedPolygon( plane );
    aut := AutomorphismGroup( graph );
    stab := Stabilizer(aut, plane!.points, OnSets);
    coll := Action(stab, plane!.points, OnPoints);
    act := function( x, g )
             if x!.type = 1 then
                return Wrap(plane, 1, OnPoints(x!.obj, g));
             elif x!.type = 2 then
                return Wrap(plane, 2, OnSets(x!.obj, g));
             fi;
           end;
    SetCollineationAction( coll, act );
    return coll;   
  end );

InstallMethod( Span, "for two elements of a projective plane",
       [IsElementOfGeneralisedPolygon, IsElementOfGeneralisedPolygon],  ## do we want special filters?
  function( x, y )
    local pp, l;  #, mat;
    pp := x!.geo;
    if y!.geo <> pp then
       Error("Elements do not belong to the same projective plane");
    fi;
    if x!.type = 2 or y!.type = 2 then
       return pp;
   # elif HasIncidenceMatrixOfGeneralisedPolygon(pp) then
   #    mat := IncidenceMatrixOfGeneralisedPolygon(pp);
   #    colx := x!.obj;
   #    coly := y!.obj;
    else
       l := First(pp!.lines, t -> x!.obj in t and y!.obj in t);
       return Wrap(pp, 2, l);
    fi;
  end );

InstallMethod( Meet, "for two elements of a projective plane",
       [IsElementOfGeneralisedPolygon, IsElementOfGeneralisedPolygon],  ## do we want special filters?
  function( x, y )
    local pp;
    pp := x!.geo;
    if y!.geo <> pp then
       Error("Elements do not belong to the same projective plane");
    fi;
    if x!.type = 1 or y!.type = 1 then
       return [];
    else
       return Wrap(pp, 1, IntersectionSet(x!.obj,y!.obj));
    fi;
  end );




#############################################################################
#
#  General operations
#
#############################################################################


InstallOtherMethod( \^, [IsElementOfGeneralisedPolygon, IsPerm],
  function(x, em)
    local act;
    if HasCollineationAction( x!.geo ) then
       act := CollineationAction(x!.geo);
    else
       Error("Does not have a CollineationAction installed");
    fi;
    return act(x, em);
  end );

InstallMethod( BlockDesignOfGeneralisedPolygon,
             [ IsProjectivePlane and IsGeneralisedPolygonRep ], 
  function( gp )
    local points, lines, des;
    if not "design" in RecNames(GAPInfo.PackagesLoaded) then
       Error("You must load the DESIGN package\n");
    fi;
    if IsBound(gp!.BlockDesignOfGeneralisedPolygonAttr) then
       return gp!.BlockDesignOfGeneralisedPolygonAttr;
    fi;
    points := gp!.points;
    lines := gp!.lines;
    Info(InfoFinInG, 1, "Computing block design of generalised polygon...");
    des := BlockDesign(Size(points), Set(lines, AsSet));
    Setter( BlockDesignOfGeneralisedPolygonAttr )( gp, des );
    return des;
  end );

InstallMethod( BlockDesignOfGeneralisedPolygon,
             [ IsGeneralisedPolygon and IsGeneralisedPolygonRep ], 
  function( gp )
    local points, lines, des, blocks, l, b, elations, gg, orbs;
    if not "design" in RecNames(GAPInfo.PackagesLoaded) then
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
	   orbs := List(orbs, l -> Filtered([1..Size(points)], i -> points[i] in l));
	   gg := Action(elations, points, CollineationAction( elations ) );
          Info(InfoFinInG, 1, "Computing block design of generalised polygon...");    
	   des := BlockDesign(Size(points), orbs, gg ); 
	elif HasCollineationGroup(gp) then
	   gg := CollineationGroup(gp);
	   orbs := List( Orbits(gg, lines, CollineationAction(gg)), Representative);
	   orbs := List(orbs, l -> Filtered([1..Size(points)], i -> points[i] in l));
	   gg := Action(gg, points, CollineationAction( elations ) );
	   des := BlockDesign(Size(points), orbs, gg );
	else
  	   blocks := [];
       for l in lines do
          b := Filtered([1..Size(points)], i -> points[i] in l);
          Add(blocks, b);
       od;   
       des := BlockDesign(Size(points), Set(blocks));
	fi;

    Setter( BlockDesignOfGeneralisedPolygonAttr )( gp, des );
    return des;
  end );

InstallMethod( IncidenceGraphOfGeneralisedPolygon,
             [ IsGeneralisedPolygon ], 
  function( gp )
    local points, lines, graph, sz, adj, elations, gg, coll;
    if not "grape" in RecNames(GAPInfo.PackagesLoaded) then
       Error("You must load the GRAPE package\n");
    fi;
    if IsBound(gp!.IncidenceGraphOfGeneralisedPolygonAttr) then
       return gp!.IncidenceGraphOfGeneralisedPolygonAttr;
    fi;
    points := AsList( Points( gp ) );;  
    lines := AsList( Lines( gp ) );;    

    Info(InfoFinInG, 1, "Computing incidence graph of generalised polygon...");
    
    sz := Size(points);
    adj := function(i,j)
             if i <= sz and j > sz then
                return points[i] in lines[j-sz];
             elif j <= sz and i > sz then
                return points[j] in lines[i-sz];
             else
                return false;
             fi;
           end;

	if HasCollineationGroup(gp) then
	   coll := CollineationGroup(gp);
	   gg := Action(coll, Concatenation(points, lines));  ## here we have not assumed an action!
	   graph := Graph( gg, [1..sz+Size(lines)], OnPoints, adj );  
	elif IsElationGQ(gp) and HasElationGroup( gp ) then
	   elations := ElationGroup(gp);
	   gg := Action(elations, Concatenation(points, lines), CollineationAction( elations ) );
	   graph := Graph( gg, [1..sz+Size(lines)], OnPoints, adj );  
	else
	   graph := Graph( Group(()), [1..sz+Size(lines)], OnPoints, adj );  
	fi;

    Setter( IncidenceGraphOfGeneralisedPolygonAttr )( gp, graph );
    return graph;
  end );

InstallMethod( IncidenceGraphOfGeneralisedPolygon,
             [ IsProjectivePlane and IsGeneralisedPolygonRep ], 
  function( gp )
    local points, lines, graph, sz, adj;
    if not "grape" in RecNames(GAPInfo.PackagesLoaded) then
       Error("You must load the GRAPE package\n");
    fi;
    if IsBound(gp!.IncidenceGraphOfGeneralisedPolygonAttr) then
       return gp!.IncidenceGraphOfGeneralisedPolygonAttr;
    fi;
    points := AsList( Points( gp ) );;  
    lines := AsList( Lines( gp ) );;    

    Info(InfoFinInG, 1, "Computing incidence graph of projective plane...");
    sz := Size(points);
    adj := function(i,j)
             if i <= sz and j > sz then
                return points[i] in lines[j-sz];
             elif j <= sz and i > sz then
                return points[j] in lines[i-sz];
             else
                return false;
             fi;
           end;
    graph := Graph( Group(()), [1..2 * sz], OnPoints, adj );  
    Setter( IncidenceGraphOfGeneralisedPolygonAttr )( gp, graph );
    return graph;
  end );

InstallMethod( IncidenceMatrixOfGeneralisedPolygon,
             [ IsGeneralisedPolygon ],
  function( gp )
    local graph, mat, incmat, szpoints, szlines;
    graph := IncidenceGraphOfGeneralisedPolygon( gp );
    mat := CollapsedAdjacencyMat(Group(()), graph);

    ## The matrix above is the adjacency matrix of the
    ## bipartite incidence graph.
    
    szpoints := Size(Points(gp));
    szlines := Size(Lines(gp));

    incmat := mat{[1..szpoints]}{[szpoints+1..szpoints+szlines]};
    return incmat;
  end );


#############################################################################
# Display methods: Generalised Polygons
#############################################################################

InstallMethod( ViewObj, [ IsGeneralisedPolygon and IsGeneralisedPolygonRep and HasOrder],
  function( p )
    Print("<generalised polygon of order ",Order(p),">");
  end );

InstallMethod( PrintObj, [ IsGeneralisedPolygon and IsGeneralisedPolygonRep and HasOrder],
  function( p )
    Print("Generalised polygon of order ",Order(p),
     ", consisting of ", Size(p!.points), " points and ", Size(p!.lines)," lines");
  end );

InstallMethod( ViewObj, [ IsElationGQ and HasOrder],
  function( p )
    Print("<EGQ of order ",Order(p),">");
  end );

InstallMethod( ViewObj, [ IsElationGQ and HasOrder and HasBasePointOfEGQ ],
  function( p )
    Print("<EGQ of order ",Order(p), 
     " and basepoint ", BasePointOfEGQ(p),">");
  end );

InstallMethod( PrintObj, [ IsElationGQ and HasOrder],
  function( p )
    Print("Elation generalised quadrangle of order ",Order(p),
     ", consisting of ", Size(p!.points), " points and ", Size(p!.lines)," lines");
  end );

InstallMethod( ViewObj, [ IsProjectivePlane and HasOrder],
  function( p )
    Print("<projective plane of order ",Order(p)[1],">");
  end );

InstallMethod( PrintObj, [ IsProjectivePlane and HasOrder ],
  function( p )
    Print("Projective plane of order ",Order(p)[1],
     ", consisting of ", Size(p!.points), " points and ", Size(p!.lines)," lines");
  end );

InstallMethod( ViewObj, [ IsGeneralisedQuadrangle and HasOrder],
  function( p )
    Print("<generalised quadrangle of order ",Order(p),">");
  end );

InstallMethod( PrintObj, [ IsGeneralisedQuadrangle and HasOrder ],
  function( p )
    Print("Generalised quadrangle of order ",Order(p),
     ", consisting of ", Size(p!.points), " points and ", Size(p!.lines)," lines");
  end );

InstallMethod( ViewObj, 
  [ IsClassicalGQ and HasOrder and IsEllipticQuadric],
  function( p )
    Print("Q-(5, ",Size(p!.basefield),")");
  end );

InstallMethod( PrintObj,
  [ IsClassicalGQ and HasOrder and IsEllipticQuadric ],
        function( p )
          Print("EllipticQuadric(5, ",p!.basefield,")");
        end );

InstallMethod( Display, 
  [ IsClassicalGQ and HasOrder and IsEllipticQuadric ],
  function( p )
    Print("Q-(5, ",Size(p!.basefield),")\n");
    if HasQuadraticForm(p) then
       Display(QuadraticForm(p));
    fi;
    Display(SesquilinearForm(p));
  end );

InstallMethod( ViewObj,
  [ IsClassicalGQ and HasOrder and IsSymplecticSpace],
        function( p )
          Print("W(3, ",Size(p!.basefield),")");
  end );

InstallMethod( PrintObj,
  [ IsClassicalGQ and HasOrder and IsSymplecticSpace ],
        function( p )
          Print("SymplecticSpace(3, ",p!.basefield,")");
  end);

InstallMethod( ViewObj,
  [ IsClassicalGQ and HasOrder and IsParabolicQuadric ],
        function( p )
          Print("Q(4, ",Size(p!.basefield),")");
        end);

InstallMethod( PrintObj,
  [ IsClassicalGQ and HasOrder and IsParabolicQuadric ],
        function( p )
          Print("ParabolicQuadric(4, ",p!.basefield,")");
  end);

InstallMethod( ViewObj,
  [ IsClassicalGQ and HasOrder and IsHyperbolicQuadric ],
        function( p )
          Print("Q+(", p!.dimension,", ",Size(p!.basefield),")");
  end);

InstallMethod( PrintObj,
  [ IsClassicalGQ and HasOrder and IsHyperbolicQuadric ],
        function( p )
          Print("HyperbolicQuadric(", p!.dimension,", ",p!.basefield,")");
        end);

InstallMethod( ViewObj,
  [ IsClassicalGQ and HasOrder and IsHermitianVariety ],
        function( p )
          Print("H(",p!.dimension,", ",Sqrt(Size(p!.basefield)),"^2)");
        end);

InstallMethod( PrintObj,
  [ IsClassicalGQ and HasOrder and IsHermitianVariety ],
        function( p )
          Print("HermitianVariety(",p!.dimension,",",p!.basefield,")");
        end);

InstallMethod( ViewObj, [ IsClassicalGQ and HasOrder],
  function( p )
    Print("<classical ", SesquilinearForm(p)!.type, 
       " generalised quadrangle of order ",Order(p),">");
  end );

InstallMethod( PrintObj, [ IsClassicalGQ and HasOrder],
  function( p )
    Print("<classical ", SesquilinearForm(p)!.type, 
       " generalised quadrangle of order ",Order(p),">"); 
  end );

InstallMethod( ViewObj, [ IsGeneralisedHexagon and HasOrder ],
  function( p )
    Print("<generalised hexagon of order ",Order(p), ">");
  end );

InstallMethod( PrintObj, [ IsGeneralisedHexagon and HasOrder ],
  function( p )
    Print("Generalised hexagon of order ",Order(p),
     ", consisting of ", Size(p!.points), " points and ", Size(p!.lines)," lines");
  end );

InstallMethod( ViewObj, [ IsGeneralisedOctogon and HasOrder ],
  function( p )
    Print("<generalised octogon of order ",Order(p), ">");
  end );

InstallMethod( PrintObj, [ IsGeneralisedOctogon and HasOrder ],
  function( p )
    Print("Generalised ",p!.gonality,"-gon of order ",Order(p),
     ", consisting of ", Size(p!.points), " points and ", Size(p!.lines)," lines");
  end );

#############################################################################
# Display methods: Element collections
#############################################################################

InstallMethod( ViewObj, [ IsAllElementsOfGeneralisedPolygon and
                          IsAllElementsOfGeneralisedPolygonRep ],
  function( vs )
    local l;
    l := ["points","lines"];
    Print("<", l[vs!.type]," of ");
    ViewObj(vs!.geometry);
    Print(">");
  end );

InstallMethod( PrintObj, [ IsAllElementsOfGeneralisedPolygon and
                          IsAllElementsOfGeneralisedPolygonRep ],
  function( vs )
    Print("ElementsOfIncidenceStructure( ",vs!.geometry," , ",vs!.type,")");
  end );

#############################################################################
# Display methods: Elements
#############################################################################

InstallMethod( ViewObj, [ IsElementOfKantorFamily ],
  function( v )
    if v!.type = 1 then Print("<a point of a Kantor family>");
    else Print("<a line of a Kantor family>");
    fi;
  end );

InstallMethod( PrintObj, [ IsElementOfKantorFamily ],
  function( v )
    Print(v!.obj);
  end );

InstallMethod( ViewObj, [ IsElementOfGeneralisedPolygon ],
  function( v )
    Print("<a ",TypesOfElementsOfIncidenceStructure(v!.geo)[v!.type]," of ");
    ViewObj(v!.geo);
    Print(">");
  end );

InstallMethod( PrintObj, [ IsElementOfGeneralisedPolygon ],
  function( v )
    Print(v!.obj);
  end );  

