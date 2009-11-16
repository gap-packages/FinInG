############################################################################
##
##  schemes.gi              Desargues package
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
##  Implementation stuff for schemes
##
#############################################################################

########################################
#
# Things To Do:
#
# - operations for GrassmannVariety, SegreVariety, VeroneseVariety
#   what does the user need? make example code?
# - groups for GrassmannVariety, SegreVariety
# - put in John's code for "QuadricDefinedByPoints" and "HermitianVarietyDefinedByPoints"
#   perhaps this should be generalised?
# - what are things go in here?
# - testing, documentation
#
########################################



#############################################################################
# Constructor methods:
#############################################################################

InstallMethod( ProjectiveAlgebraicVariety, [ IsProjectiveSpace, IsPolynomial ],
  function( pg, f )
    local geo, ty;
    geo := rec( poly := f, basefield := pg!.basefield, vectorspace := pg!.vectorspace );
    ty := NewType( GeometriesFamily, IsProjectiveAlgebraicVariety and
                      IsProjectiveAlgebraicVarietyRep );
    ObjectifyWithAttributes( geo, ty,
           AmbientSpace, pg );
    return geo;
  end );

InstallMethod( \in, "for a point and a projective alg. variety",
  [IsElementOfIncidenceStructure, IsProjectiveAlgebraicVariety],
  function( w, ps )
    local ow, poly, indets;
    # if the vector spaces don't agree we can't go any further
    if w!.geo!.vectorspace <> ps!.vectorspace then
      return false;
    fi;
    # check if w satisfies the polynomial defining ps
    ow := w!.obj;
    poly := ps!.poly;
    indets := [1..NrIndeterminatesPolynomial(poly)];
    return IsZero(Value(poly, indets, ow));
  end );


InstallMethod( TypesOfElementsOfIncidenceStructure, "for a projective alg. variety", [ IsProjectiveAlgebraicVariety ],
  function( pav )
    return [ "point" ];
  end );

InstallMethod( TypesOfElementsOfIncidenceStructurePlural, "for a projective alg. variety", [IsProjectiveAlgebraicVariety],
  function( vero )
    return ["points"];
  end );


InstallMethod( ElementsOfIncidenceStructure, [IsProjectiveAlgebraicVariety, IsPosInt],
  function( pav, j )
    local vars;
    vars := rec( geometry := pav, type := j );
    Objectify(
      NewType( ElementsCollFamily, IsElementsOfIncidenceStructure and
                                IsAllElementsOfProjectiveAlgebraicVariety and
                                IsAllElementsOfProjectiveAlgebraicVarietyRep),
        vars );
    return vars;
  end );

InstallMethod( Wrap, "for a projective algebraic variety and an object (point)",
  [IsProjectiveAlgebraicVariety, IsPosInt, IsObject],
  function( geo, type, o )
    local w;
    w := rec( geo := geo, type := type, obj := o );
    Objectify( NewType( ElementsFamily,
      IsElementOfProjectiveAlgebraicVarietyRep and IsElementOfProjectiveAlgebraicVariety ), w );
    return w;
  end );

InstallMethod( NrIndeterminatesPolynomial, [IsPolynomial],
  function( poly )
    local ll, oddnums;
    ll := ExtRepPolynomialRatFun(poly); 
    oddnums := [1, 3..Size(ll)-1];;
    ll := Concatenation(Set(ll{oddnums}));
    oddnums := [1, 3..Size(ll)-1];;
    ll := ll{oddnums};
    return Size(Set(ll));  ##Sum(List(ll,Size))/2;
  end );

InstallMethod(Iterator, "for varieties of a projective algebraic variety",
        [IsAllElementsOfProjectiveAlgebraicVariety],
  function( vs )
    local pav, j, poly, pg, indets, iszero, newiter, r;
    pav := vs!.geometry;
    j := vs!.type;
    poly := pav!.poly;
    pg := AmbientSpace( pav );

   # r := PolynomialRing(pav!.basefield, Dimension(pav!.vectorspace));
   # indets := IndeterminatesOfPolynomialRing(r);

    indets := [1..NrIndeterminatesPolynomial(poly)];
    iszero := v -> IsZero(Value(poly, indets, v!.obj));

    return IteratorList(Filtered(Points(pg),iszero));
  end );

#############################################################################
#
#  Grassmann Varieties
#
#############################################################################

InstallMethod( GrassmannVariety, [ IsPosInt, IsPosInt, IsField ],
  function( k, d, f )
    local geo, ty, pg, p;
    geo := rec( kdim := k, ddim := d, basefield := f);
    ty := NewType( GeometriesFamily, IsGrassmannVariety and
                      IsGrassmannVarietyRep );
    pg := ProjectiveSpace(Binomial(d+1, k+1) - 1, f);
    ObjectifyWithAttributes( geo, ty, AmbientSpace, pg );
    return geo;
  end );

InstallMethod( GrassmannVariety, [ IsPosInt, IsPosInt, IsPosInt ],
  function( k, d, q )
    return GrassmannVariety( k, d, GF(q) );
  end );

InstallMethod( ElementsOfIncidenceStructure, [IsGrassmannVariety, IsPosInt],
  function( gv, j )
    local vars;
    vars := rec( geometry := gv, type := j );
    Objectify(
      NewType( ElementsCollFamily, IsElementsOfIncidenceStructure and
                                IsAllElementsOfGrassmannVariety and
                                IsAllElementsOfGrassmannVarietyRep),
        vars );
    return vars;
  end );

InstallMethod( Size, [ IsAllElementsOfGrassmannVariety and 
                       IsAllElementsOfGrassmannVarietyRep],
  function( vars )
    local geo, F, d, k;
    geo := vars!.geometry;
    F := geo!.basefield;
    d := geo!.ddim;
    k := geo!.kdim;
    return Size(ElementsOfIncidenceStructure(ProjectiveSpace(d, F), k+1)); 
  end );

InstallMethod( Wrap, "for a GrassmannVariety and a vector/matrix",
  [IsGrassmannVariety, IsPosInt, IsObject],
  function( geo, type, o )
    local w;
    w := rec( geo := geo, type := type, obj := o );
    Objectify( NewType( ElementsFamily,
      IsElementOfGrassmannVarietyRep and IsElementOfGrassmannVariety ), w );
    return w;
  end );

InstallMethod( CollineationGroup, [ IsGrassmannVariety ],
  ## To be completed.
  function( vn )
    local f, d, d2, tups, beta, betainv, hom, gens, newgens, frob, g;
    f := vn!.basefield;
    d := vn!.dimension + 1;
    d2 := (-1 + Sqrt(1+8*d))/2;
    if not IsInt(d2) then
       Error("the dimension is not correct");
    fi;
    tups := Filtered(Tuples([1..d2], 2),i->i[2]>=i[1]);

    beta := function( m )
      local rows;
      rows := List([1..d2], i -> m[i]{[i..d2]});
      return Concatenation(rows);
    end;

    betainv := function( v )
      local matb, i, j, x;
      matb := ShallowCopy( NullMat(d2, d2, f) );
          for i in [1..d2] do
              for j in [i..d2] do
                  x := v[Position(tups,[i,j])];
                  matb[i][j] := x;
                  matb[j][i] := x;
              od;
          od;
      return matb;
    end;
      
    hom := function( m )
      ## m is a matrix, and we consider
      ## its action on basis elements
      local basis1, basis2, image;
      basis1 := IdentityMat(d, f);
      basis2 := List(basis1, betainv);
      image := List(basis2, b -> beta( TransposedMat(m) * b * m ));         
      return image;
    end;
    
    gens := GeneratorsOfGroup( GL(d2, f) );
    newgens := List(gens, hom);
    frob := FrobeniusAutomorphism(f)^0;;
    g := Group(List(newgens,t->ProjElWithFrob(t,frob,f)));
    return g;
  end );

InstallMethod( Iterator, "for varieties of a GrassmannVariety",
              [IsAllElementsOfGrassmannVariety],
  function( vs )
    local gr, d, k, F, pg, vmap, iter, iterpg;
    gr := vs!.geometry;
    F := gr!.basefield;
    d := gr!.ddim;
    k := gr!.kdim;

    ## the domain of the Grassmann map
    
    pg := ProjectiveSpace(d, F); 
    vmap := GrassmannMap( k, pg )!.fun; 
    iterpg := Iterator( ElementsOfIncidenceStructure(pg, k+1) );

    iter := IteratorByFunctions( rec(
            NextIterator := function(iter)
              local x;
              x := NextIterator(iter!.S);
              return vmap( x );
            end,
            IsDoneIterator := function(iter)
              return IsDoneIterator(iter!.S);
            end,
            ShallowCopy := function(iter)
              return rec( S := ShallowCopy(iter!.S) );
            end,
            S := iterpg ));
    return iter;
  end );


#############################################################################
#
#  Veronese Varieties
#
#############################################################################


InstallMethod( VeroneseVariety, [ IsPosInt, IsField and IsFinite ],
  function( d, f )
    local geo, ty, pg, p;
    geo := rec( dimension := d, basefield := f, 
                vectorspace := FullRowSpace(f, d+1) );
    ty := NewType( GeometriesFamily, IsVeroneseVariety and
                      IsVeroneseVarietyRep );
    pg := ProjectiveSpace(d, f);
     # p := VectorSpaceToElement(pg, IdentityMat(d,f)[1]); 
    ObjectifyWithAttributes( geo, ty,
           AmbientSpace, pg );
        #   RepresentativesTypesOfElementsOfIncidenceStructure, [p] );
    return geo;
  end );

InstallMethod( VeroneseVariety, [ IsPosInt, IsPosInt ],
  function( d, q )
    return VeroneseVariety( d, GF(q) );
  end );

InstallMethod( ElementsOfIncidenceStructure, [IsVeroneseVariety, IsPosInt],
  function( pav, j )
    local vars;
    vars := rec( geometry := pav, type := j );
    Objectify(
      NewType( ElementsCollFamily, IsElementsOfIncidenceStructure and
                                IsAllElementsOfVeroneseVariety and
                                IsAllElementsOfVeroneseVarietyRep),
        vars );
    return vars;
  end );

InstallMethod( Size, [ IsAllElementsOfVeroneseVariety and 
                       IsAllElementsOfVeroneseVarietyRep],
  function( vars )
    local geo, j, F, d, d2;
    geo := vars!.geometry;
    j := vars!.type;
    F := geo!.basefield;
    d := geo!.dimension;
    d2 := (-1 + Sqrt(1+8*(d+1)))/2 - 1;
    return Size(ElementsOfIncidenceStructure(ProjectiveSpace(d2, F), j)); 
  end );

InstallMethod( Wrap, "for a veronsean and a vector/matrix",
  [IsVeroneseVariety, IsPosInt, IsObject],
  function( geo, type, o )
    local w;
    w := rec( geo := geo, type := type, obj := o );
    Objectify( NewType( ElementsFamily,
      IsElementOfVeroneseVarietyRep and IsElementOfVeroneseVariety ), w );
    return w;
  end );

InstallMethod( CollineationGroup, [ IsVeroneseVariety ],
  function( vn )
    local f, d, d2, tups, beta, betainv, hom, gens, newgens, frob, g;
    f := vn!.basefield;
    d := vn!.dimension + 1;
    d2 := (-1 + Sqrt(1+8*d))/2;
    return Range( Intertwiner( VeroneseMap( ProjectiveSpace(d2-1, f) ) ) );
  end );

InstallMethod( Iterator, "for varieties of a VeroneseVariety",
              [IsAllElementsOfVeroneseVariety],
  function( vs )
    local vero, j, n, n2, F, pg, vmap, iter, iterpg;
    vero := vs!.geometry;
    j := vs!.type;
    n := vero!.dimension;
    F := vero!.basefield;

    ## the following is the dimension of the smaller 
    ## projective space (the domain of the Veronese map)

    n2 := (-1 + Sqrt(1+8*(n+1)))/2 - 1;
    if not IsInt(n2) then
       Error("the dimension is not correct");
    fi;

    ## the domain of the veronese map
    
    pg := ProjectiveSpace(n2, F); 

    vmap := VeroneseMap( pg )!.fun; 
    iterpg := Iterator( ElementsOfIncidenceStructure(pg, j) );

    iter := IteratorByFunctions( rec(
            NextIterator := function(iter)
              local x;
              x := NextIterator(iter!.S);
              return vmap( x );
            end,
            IsDoneIterator := function(iter)
              return IsDoneIterator(iter!.S);
            end,
            ShallowCopy := function(iter)
              return rec( S := ShallowCopy(iter!.S) );
            end,
            ## local variable(s)
            S := iterpg ));
    return iter;
  end );

#############################################################################
#
#  Segre Varieties
#
#############################################################################

## Segre varieties must remember where they come from,
## that is, the list of dimensions in the source of the
## Segre map. The record component "dimlist" stores these
## dimensions as algebraic dimensions.


InstallMethod( SegreVariety, "for a list of projective spaces", 
         [ IsHomogeneousList ],
  function( listspaces )
    local F, listofdims, l, geo, ty, d;
    F := listspaces[1]!.basefield;
    listofdims := List(listspaces, i -> ProjectiveDimension(i) + 1);
    for l in listspaces do
        if l!.basefield <> F then 
           Error("The proj. spaces need to be defined over the same field"); 
        fi;
    od;
  
    d:= Product( listofdims );
    geo := rec( dimension := d-1, basefield := F, dimlist := listofdims,
                vectorspace := FullRowSpace(F, d) );
    ty := NewType( GeometriesFamily, IsSegreVariety and IsSegreVarietyRep ); 
    ObjectifyWithAttributes( geo, ty, AmbientSpace, ProjectiveSpace(d-1, F));
    return geo;
  end );

InstallMethod( SegreVariety, "for a list of dimensions and a field",
           [ IsHomogeneousList, IsField ], 
  function( listofdims, F )
    return SegreVariety( List(listofdims, d -> ProjectiveSpace(d, F)) );
  end );

InstallMethod( SegreVariety, "for two projective spaces",
           [ IsProjectiveSpace, IsProjectiveSpace ], 
  function( pg1, pg2 )
    return SegreVariety( [pg1, pg2] );
  end );

InstallMethod( SegreVariety, "for two dimensions and a field",
           [ IsPosInt, IsPosInt, IsField ], 
  function( d1, d2, F )
    return SegreVariety( [ProjectiveSpace(d1, F), ProjectiveSpace(d2, F)] );
  end );

InstallMethod( ElementsOfIncidenceStructure, [IsSegreVariety, IsPosInt],
  function( sv, j )
    local vars;
    vars := rec( geometry := sv, type := j );
    Objectify(
      NewType( ElementsCollFamily, IsElementsOfIncidenceStructure and
                                IsAllElementsOfSegreVariety and
                                IsAllElementsOfSegreVarietyRep),
        vars );
    return vars;
  end );

InstallMethod( Size, [ IsAllElementsOfSegreVariety and 
                       IsAllElementsOfSegreVarietyRep],
  function( vars )
    local geo, F, dimlist, listsizes;
    geo := vars!.geometry;
    F := geo!.basefield;
    dimlist := geo!.dimlist;
    listsizes := List(dimlist, d -> 
            Size(ElementsOfIncidenceStructure(ProjectiveSpace(d-1, F),1)));
    return Product(listsizes); 
  end );

InstallMethod( Wrap, "for a SegreVariety and a vector/matrix",
  [IsSegreVariety, IsPosInt, IsObject],
  function( geo, type, o )
    local w;
    w := rec( geo := geo, type := type, obj := o );
    Objectify( NewType( ElementsFamily,
      IsElementOfSegreVarietyRep and IsElementOfSegreVariety ), w );
    return w;
  end );


##  A generalisation of "CartesianIterator" from the package RDS: misc.gd/misc.gi
##  so that it takes iterators as input.

InstallMethod( ProductOfIterators, [IsList],
        function( iterlist )
    local iters, nextIterator, isDoneIterator, shallowCopy;

    if iterlist=[] or ForAll(iterlist,IsDoneIterator) then
        return Iterator([]);
    fi;
    iters:=List(iterlist,i->ShallowCopy(i));
    iters:=Filtered(iters,i->not IsDoneIterator(i));

    nextIterator:=function(iter)
       local i, j, val;
 
       ## take care of the first step (initialisation)

       if not IsBound(iter!.value[Size(iterlist)]) then
          val := [];  
          for i in [1..Size(iterlist)] do
              Add( val, NextIterator( iter!.iters[i] ) );
          od;  
          iter!.value := ShallowCopy(val); 
       else

          # find the first position i such that iterlist[i] is not done. 

          i := 1;
          while i < Size(iterlist) and IsDoneIterator( iter!.iters[i] ) do
                i := i + 1;
          od;

          # increment in this position 
          #  *****  (this is where the problem with "choiceiter" occurs) ******

          iter!.value[i] := ShallowCopy(NextIterator( iter!.iters[i] ));

          # if an iterator in position j < i is done, reset it

          for j in [1..i-1] do
             if IsDoneIterator( iter!.iters[j] ) then
                iter!.iters[j] := ShallowCopy(iterlist[j]);
                iter!.value[j] := NextIterator( iter!.iters[j] ); 
             fi;
          od;
       fi;
       val := StructuralCopy(iter!.value); 
       return val;
    end;

    isDoneIterator:=function(iter)
        if ForAll(iter!.iters, IsDoneIterator) then
           return true;
        else
           return false;
        fi;
    end;

    shallowCopy:=function(iter)
        if ForAll(iter!.iters,IsDoneIterator)
           then return Iterator([]);
        else
            return rec(iters:=List(iter!.iters,i->ShallowCopy(i)),
                       value := iters!.value,
                       IsDoneIterator:=isDoneIterator,
                       NextIterator:=nextIterator,
                       ShallowCopy:=shallowCopy);
        fi;
    end;
    
    return IteratorByFunctions(rec(iters:=iters,
              value := [],
              IsDoneIterator:=isDoneIterator,
              NextIterator:=nextIterator,
              ShallowCopy:=shallowCopy));
  end );


InstallMethod( Iterator, "for varieties of a Segre variety",
              [IsAllElementsOfSegreVariety],
  function( vs )
    local segre, j, d, dimlist, pglist, F, iters, vmap, iter, proditer;
    segre := vs!.geometry;
    d := segre!.dimension;
    F := segre!.basefield;
    dimlist := segre!.dimlist;
    pglist := List(dimlist, d -> ProjectiveSpace(d-1, F));

    ## Need to take Cartesian product of iterators
    
    iters := List(pglist, p -> Iterator(ElementsOfIncidenceStructure(p, 1)));
    vmap := SegreMap( pglist );
    proditer := ProductOfIterators( iters );

    iter := IteratorByFunctions( rec(
            NextIterator := i -> vmap( NextIterator(i!.S) ),
            IsDoneIterator := i -> IsDoneIterator(i!.S),
            ShallowCopy := i -> rec( S := ShallowCopy(i!.S) ),
            S := proditer ));
    return iter;
  end );


#############################################################################
#
#  Miscellaneous
#
#############################################################################

InstallMethod( ConicOnFivePoints, "given a set of five points of a projective plane", 
   [ IsHomogeneousList and IsSubspaceOfProjectiveSpaceCollection ],
  
  function( pts )

  #  To find the conic, we simply solve an equation
  #
  #  ax^2 + by^2 + cz^2 + dxy + exz + fyz = 0
  #  [x^2,y^2,z^2,xy,xz,yz] . [a,b,c,d,e] = 0
  #
  #  This function returns a projective algebraic variety

    local gf, r, vecs, mat, sol, poly, mat2, plane, embed, 
          pg, d, dplus1, pairs, vars, indets;
    if Size(pts) < 5 then
       Error("Not enough points");
    fi;

    if ForAny(pts, t -> ProjectiveDimension(t) <> 0) then
       Error("Not a set of points");
    fi;

    ## check that the points span a plane
 
    if Rank( List(pts, t -> t!.obj) ) <> 3 then
       Error("Points do not span a plane");
    fi;

    pg := AmbientSpace(pts[1]!.geo);
    gf := pg!.basefield;
    r := PolynomialRing(gf, 3);
    d := pg!.dimension;
    dplus1 := d + 1;
    r := PolynomialRing(gf, dplus1);
    indets := IndeterminatesOfPolynomialRing(r);
    vecs := List(pts, t -> t!.obj);
    pairs := UnorderedTuples( [1..dplus1], 2 );;
    vars := List(pairs, p -> indets[p[1]] * indets[p[2]]);
    mat := List(vecs, t -> List( pairs, p -> t[p[1]] * t[p[2]] ) );;
    mat2 := ShallowCopy(mat);
    sol := NullspaceMat(TransposedMat(mat2))[1];
    poly := vars * sol;

    return ProjectiveAlgebraicVariety( pg, poly );
  end );  



#############################################################################
# View methods:
#############################################################################

InstallMethod( ViewObj, [ IsProjectiveAlgebraicVariety and 
                           IsProjectiveAlgebraicVarietyRep ],
  function( geo )
    Print("V( ", geo!.poly, " )");
  end );

InstallMethod( PrintObj, [ IsProjectiveAlgebraicVariety and 
                           IsProjectiveAlgebraicVarietyRep ],
  function( geo )
    Print("ProjectiveAlgebraicVariety( ", geo!.poly, " ) of ", AmbientSpace(geo));
  end );

InstallMethod( ViewObj, [ IsAllElementsOfProjectiveAlgebraicVariety and 
                           IsAllElementsOfProjectiveAlgebraicVarietyRep ],
  function( vars )
    Print("<points of ");
    ViewObj( vars!.geometry);
    Print( ">");
  end );

InstallMethod( PrintObj, [ IsAllElementsOfProjectiveAlgebraicVariety and 
                           IsAllElementsOfProjectiveAlgebraicVarietyRep  ],
  function( vars )
    Print("<points of ");
    Print( vars!.geometry);
    Print( ">");
  end );


InstallMethod( ViewObj, [ IsGrassmannVariety and IsGrassmannVarietyRep ],
  function( p )
    Print("Gr(",p!.kdim, ", ", p!.ddim,", ",Size(p!.basefield),")");
  end );

InstallMethod( PrintObj, [ IsGrassmannVariety and IsGrassmannVarietyRep ],
  function( p )
          Print("GrassmannVariety(",p!.kdim, ", ",p!.ddim,",",p!.basefield,")");
  end );

InstallMethod( Display, [ IsGrassmannVariety and IsGrassmannVarietyRep ],
  function( p )
    Print("GrassmannVariety(",p!.kdim, ", ",p!.ddim,",",p!.basefield,")\n");
    if HasDiagramOfGeometry( p ) then
       Display( DiagramOfGeometry( p ) );
    fi;
  end );


InstallMethod( ViewObj, [ IsVeroneseVariety and IsVeroneseVarietyRep ],
  function( p )
    Print("Ver(",p!.dimension,", ",Size(p!.basefield),")");
  end );

InstallMethod( PrintObj, [ IsVeroneseVariety and IsVeroneseVarietyRep ],
  function( p )
          Print("VeroneseVariety(",p!.dimension,",",p!.basefield,")");
  end );

InstallMethod( Display, [ IsVeroneseVariety and IsVeroneseVarietyRep ],
  function( p )
    Print("VeroneseVariety(",p!.dimension,",",p!.basefield,")\n");
    if HasDiagramOfGeometry( p ) then
       Display( DiagramOfGeometry( p ) );
    fi;
  end );


InstallMethod( ViewObj, [ IsSegreVariety and IsSegreVarietyRep ],
  function( p )
    local n;
    Print("Segre(");
    for n in p!.dimlist do
        Print( n, ", ");
    od;
    Print(Size(p!.basefield),")");
  end );

InstallMethod( PrintObj, [ IsSegreVariety and IsSegreVarietyRep ],
  function( p )
          Print("SegreVariety(", p!.dimlist, ",",p!.basefield,")");
  end );

InstallMethod( Display, [ IsSegreVariety and IsSegreVarietyRep ],
  function( p )
    Print("SegreVariety(", p!.dimlist, ",",p!.basefield,")\n");
    if HasDiagramOfGeometry( p ) then
       Display( DiagramOfGeometry( p ) );
    fi;
  end );


