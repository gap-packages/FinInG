############################################################################
##
##  affinespace.gi              FinInG package
##                                                              John Bamberg
## 							                                    Anton Betten
##                                                             Philippe Cara
##                                                              Jan De Beule
## 							                                  Michel Lavrauw
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
##  Implementation stuff for affine spaces
##
#############################################################################

########################################
#
# Things To Do:
#
# - testing
# - ProjectiveCompletion (choice of hyperplane?)
#
########################################



#############################################################################
#
# Construction of affine spaces
#
#############################################################################

InstallMethod( AffineSpace, [ IsPosInt, IsField ],
  function( d, f )
    local geo;
    geo := rec( dimension := d, basefield := f, 
                vectorspace := FullRowSpace(f, d) );
    Objectify( NewType( GeometriesFamily,
                        IsAffineSpace and IsAffineSpaceRep ), geo );
    SetAmbientSpace(geo,geo);
    return geo;
  end );

InstallMethod( AffineSpace, "for a dimension and a prime power",
  [ IsPosInt, IsPosInt ],
  function( d, q )
          return AffineSpace(d, GF(q));
  end );



#############################################################################
#
# Attributes of affine spaces
#
#############################################################################

InstallMethod( RankAttr, "for an affine space",
  [ IsAffineSpace and IsAffineSpaceRep ],
  function( as )
    return as!.dimension;
  end );

InstallMethod( TypesOfElementsOfIncidenceStructure, "for an affine space", [IsAffineSpace],
  function( ps )
    local d,i,types;
    types := ["point"];
    d := ps!.dimension;
    if d >= 2 then Add(types,"line"); fi;
    if d >= 3 then Add(types,"plane"); fi;
    if d >= 4 then Add(types,"solid"); fi;
    for i in [5..d] do
        Add(types,Concatenation("affine subspace of dim. ",String(i)));
    od;
    return types;
  end );

InstallMethod( TypesOfElementsOfIncidenceStructurePlural, "for an affine space",
  [IsAffineSpace],
  function( ps )
    local d,i,types;
    types := ["points"];
    d := ps!.dimension;
    if d >= 2 then Add(types,"lines"); fi;
    if d >= 3 then Add(types,"planes"); fi;
    if d >= 4 then Add(types,"solids"); fi;
    for i in [5..d] do
        Add(types,Concatenation("affine. subspaces of dim. ",String(i)));
    od;
    return types;
  end );


#############################################################################
#
#  Methods for making subspaces
#
#  Affine subspaces can be constructed in the following ways:
#
# (1) from a vector v -> affine point v 
# (2) from a vector and a matrix [v, M] -> affine subspace v + <M>
# 
# From these two options, we have the following list of 
# arguments for "AffineSubspace" (for now...)
#
# [ IsAffineSpace, IsRowVector ]
# [ IsAffineSpace, IsRowVector, IsPlistRep ]
# [ IsAffineSpace, IsRowVector, Is8BitMatrixRep ]
# [ IsAffineSpace, IsRowVector, IsGF2MatrixRep ]
#
#############################################################################


InstallMethod( Wrap, "for an affine space and an object",
  [IsAffineSpace, IsPosInt, IsObject],
  function( geo, type, o )
    local w;
    w := rec( geo := geo, type := type, obj := o );
    Objectify( NewType( ElementsOfIncidenceStructureFamily, IsElementOfIncidenceStructure and
      IsElementOfIncidenceStructureRep and IsSubspaceOfAffineSpace ), w );
    return w;
  end );

InstallMethod( AffineSubspace, "for a row vector and Plist",
    [IsAffineSpace, IsRowVector, IsPlistRep],
  function( geom, v, m )
    local  x, n, i, gf, v2;
      ## when v is empty... 
      
      gf := geom!.basefield;
      
      if IsEmpty(v) then
        return [];
      fi;
      x := MutableCopyMat(m);
      TriangulizeMat(x);
      
      ## dimension should be correct
      
      if Length(v) <> geom!.dimension or Length(v) <> Length(x[1]) then
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
         return [];
      fi;
      x := x{[1..n-i]};
      if Length(x) = geom!.dimension then
         return geom;
      fi;   
      
      ## It is possible that (a) the user has entered a
      ## matrix with one row, or that (b) the user has
      ## entered a matrix with rank 1 (thus at this stage
      ## we will have a matrix with one row).
      
      ## We must also compress our vector/matrix.
      
      if IsZero(x) then
         ## return an affine point
         v2 := ShallowCopy(v);
         ConvertToVectorRep(v2, gf);
         return Wrap(geom, 1, v2);
      else
         ## find transversal element
         v2 := VectorSpaceTransversalElement( geom!.vectorspace, x, v );
         ConvertToVectorRep(x, gf);
         ConvertToMatrixRep(x, gf);
         return Wrap(geom, Length(x)+1, [v2,x]);
      fi;
 end );


InstallMethod( AffineSubspace, "for a row vector",
    [IsAffineSpace, IsRowVector],
  function( geom, v )
    local gf, v2;
    gf := geom!.basefield;

      ## when v is empty...  
    if IsEmpty(v) then
      return [];
    fi;
      
      ## dimension should be correct
      
    if Length(v) <> geom!.dimension then
       Error("Dimensions are incompatible");
    fi;
    v2 := ShallowCopy( v );
    ConvertToVectorRep(v, gf);
    return Wrap(geom, 1, v2);
 end );

InstallMethod( AffineSubspace, "for a row vector and 8-bit matrix",
    [IsAffineSpace, IsRowVector, Is8BitMatrixRep],
  function( geom, v, m )
  
  ## We have simply copied the code that was for IsPlistRep
  
    local  x, n, i, gf, v2;
    gf := geom!.basefield;     
    if IsEmpty(v) then
       return [];
    fi;
    x := MutableCopyMat(m);
    TriangulizeMat(x);
    if Length(v) <> geom!.dimension or Length(v) <> Length(x[1]) then
       Error("Dimensions are incompatible");
    fi;
    n := Length(x);
    i := 0;
    while i < n and ForAll(x[n-i], IsZero) do
          i := i+1; 
    od;
    if i = n then
       return [];
    fi;
    x := x{[1..n-i]};
    if Length(x) = geom!.dimension then
       return geom;
    fi;   
    if IsZero(x) then
       v2 := ShallowCopy(v);
       ConvertToVectorRep(v2, gf);
       return Wrap(geom, 1, v2);
    else
       v2 := VectorSpaceTransversalElement( geom!.vectorspace, x, v );
       ConvertToVectorRep(x, gf);
       ConvertToMatrixRep(x, gf);
       return Wrap(geom, Length(x), [v2,x]);
    fi;
  end ); 
  
  
InstallMethod( AffineSubspace, "for a row vector and 8-bit matrix",
    [IsAffineSpace, IsRowVector, IsGF2MatrixRep],
  function( geom, v, m )
  
  ## We have simply copied the code that was for IsPlistRep
  
    local  x, n, i, gf, v2;
    gf := geom!.basefield;     
    if IsEmpty(v) then
       return [];
    fi;
    x := MutableCopyMat(m);
    TriangulizeMat(x);
    if Length(v) <> geom!.dimension or Length(v) <> Length(x[1]) then
       Error("Dimensions are incompatible");
    fi;
    n := Length(x);
    i := 0;
    while i < n and ForAll(x[n-i], IsZero) do
          i := i+1; 
    od;
    if i = n then
       return [];
    fi;
    x := x{[1..n-i]};
    if Length(x) = geom!.dimension then
       return geom;
    fi;   
    if IsZero(x) then
       v2 := ShallowCopy(v);
       ConvertToVectorRep(v2, gf);
       return Wrap(geom, 1, v2);
    else
       v2 := VectorSpaceTransversalElement( geom!.vectorspace, x, v );
       ConvertToVectorRep(x, gf);
       ConvertToMatrixRep(x, gf);
       return Wrap(geom, Length(x), [v2,x]);
    fi;
  end ); 
  
InstallMethod( RandomSubspace, "for an affine space and a dimension",
                        [ IsAffineSpace, IsInt ],
  function(as, d)
    local vspace, w, sub;
        
    if d > RankAttr(as) then
       Error("The dimension of the subspace is larger than that of the affine space");
    fi;
	if IsNegInt(d) then
	   Error("The dimension of the subspace must be at least 0!");
    fi;

    vspace := as!.vectorspace;
    w := Random(vspace);

    if d = 1 then
	   return AffineSubspace( as, w );
	else
	   sub := BasisVectors(  Basis( RandomSubspace( vspace, d-1 ) ) );
	   return AffineSubspace( as, w, sub );
    fi;
end );  
		
InstallMethod( Random, "for a collection of subspaces of an affine space",
                       [ IsAllSubspacesOfAffineSpace ],
        # chooses a random element out of the collection of subspaces of given
        # dimension of an affine space
  function( subs )
    local x;
    x := RandomSubspace( subs!.geometry, subs!.type );
    return x;
  end );

InstallMethod( \in, "for a subspace and an affine space",
  [IsSubspaceOfAffineSpace, IsAffineSpace],
  function( x, as )
   local s;
    s := x!.geo;
    return s!.dimension = as!.dimension and 
           s!.basefield = as!.basefield;
  end );


#############################################################################
#
#  ElementsOfIncidenceStructure, enumerators, iterators
#
#############################################################################


InstallMethod( ElementsOfIncidenceStructure, [IsAffineSpace],
  function( as )
    return Objectify(
      NewType( ElementsCollFamily, IsAllElementsOfIncidenceStructure ),
        rec( geometry := as )
      );
  end);

InstallMethod( ElementsOfIncidenceStructure, [IsAffineSpace, IsPosInt],
  function( ps, j )
    return Objectify(
      NewType( ElementsCollFamily, IsElementsOfIncidenceStructure and
                                IsAllSubspacesOfAffineSpace and
                                IsAllSubspacesOfAffineSpaceRep),
        rec( geometry := ps, type := j,
          size := Size(Subspaces(ps!.vectorspace, j-1)) * 
                  Size(ps!.basefield)^(ps!.dimension - j + 1) ) );
  end );

InstallMethod( Points, [IsAffineSpace],
  function( as )
    return ElementsOfIncidenceStructure(as, 1);
  end);

InstallMethod( Lines, [IsAffineSpace],
  function( as )
    return ElementsOfIncidenceStructure(as, 2);
  end);

InstallMethod( Planes, [IsAffineSpace],
  function( as )
    return ElementsOfIncidenceStructure(as, 3);
  end);

InstallMethod( Solids, [IsAffineSpace],
  function( as )
    return ElementsOfIncidenceStructure(as, 4);
  end);

InstallMethod(Size, "for subspaces of an affine space",
      [IsAllSubspacesOfAffineSpace],
  function( vs ) return vs!.size; end);
  
  
InstallMethod( ComplementSpace, [IsVectorSpace, IsFFECollColl],
  function( space, mat )
  
  #  Taken from the code for BaseSteinitzVectors.
  #  This operation computes a list of vectors of <space>,
  #  in a deterministic way, such that they form a complement
  #  in <space> of the subspace spanned by <mat>.
    	
    local  z, l, b, i, j, k, stop, v, dim, bas;
    bas := MutableCopyMat( BasisVectors( Basis(space) ));
    z := Zero( bas[1][1] );
    if Length( mat ) > 0  then
        mat := MutableCopyMat( mat );
        TriangulizeMat( mat );
    fi;
    dim := Length( bas[1] );
    l := Length( bas ) - Length( mat );
    b := [  ];
    i := 1;
    j := 1;
    while Length( b ) < l  do
        stop := false;
        repeat
            if j <= dim and (Length( mat ) < i or mat[i][j] = z)  then
                v := PositionProperty( bas, k -> k[j] <> z );
                if v <> fail  then
                    v := bas[v];
                    v := 1 / v[j] * v;
                    Add( b, v );
                fi;
            else
                stop := true;
                if i <= Length( mat )  then
                    v := mat[i];
                    v := 1 / v[j] * v;
                else
                    v := fail;
                fi;
            fi;
            if v <> fail  then
                for k  in [ 1 .. Length( bas ) ]  do
                    bas[k] := bas[k] - bas[k][j] / v[j] * v;
                od;
                v := Zero( v );
                bas := Filtered( bas, k -> k <> v );
            fi;
            j := j + 1;
        until stop;
        i := i + 1;
    od;
    return SubspaceNC( space, b );
  end );

InstallMethod( VectorSpaceTransversalElement, [IsVectorSpace, IsFFECollColl, IsVector],
  function(space, subspace, v)
  
    # Returns a canonical vector from <v>+<subspace>
    
    local basis;
    basis := SemiEchelonBasis( Subspace(space, subspace) );
    return SiftedVector(basis, v);
  end );

InstallMethod( VectorSpaceTransversal, [IsVectorSpace, IsFFECollColl],
  function(space, subspace)
  
    # Returns a typed object containing a record with two components
    # Note: IsVectorSpaceTransversal is a subfilter of IsSubspacesOfVectorSpace
    
    return Objectify(
               NewType( CollectionsFamily( FamilyObj( space ) ),
                    IsVectorSpaceTransversal and IsVectorSpaceTransversalRep),
           rec( vectorspace := space, subspace := subspace ) );    
  end );
  
  
InstallMethod( Enumerator, [ IsVectorSpaceTransversal ],
  function( trans )  
    
    # returns an enumerator for the canonical elements of all cosets of 
    # <subspace> in <space>. 
    
    local complement, enumcomp, enum, space, subspace;
    space := trans!.vectorspace;
    subspace := trans!.subspace;
    complement := ComplementSpace( space, subspace );
    enumcomp := Enumerator( complement );
    enum := EnumeratorByFunctions( trans, rec(            
            ElementNumber := function(e, n)
              local v;
              v := enumcomp[n];
              return VectorSpaceTransversalElement(space, subspace, v);
            end,
            NumberElement := enumcomp!.NumberElement,
            Length := e -> enumcomp!.Length ));   
    return enum;
  end );


InstallMethod(Iterator, "for subspaces of an affine space",
        [IsAllSubspacesOfAffineSpace],  
  function( vs )
  ## An affine variety will be represented by a pair (vector,direction).
  ## So for example, an affine plane x+<W> will be represented by
  ## (x', proj. line)  (where x' is the transversal rep corresponding to x).
    local ps, j, vars, vec, subs, f;
    ps := vs!.geometry;
    j := vs!.type;
    vec := ps!.vectorspace;
    f := ps!.basefield;
    if j = 1 then 
       vars := List(vec, x -> Wrap(ps, 1, x));
       return IteratorList( vars );
    else
       ## we need a transversal for each subspace
       if j = 2 then 
         subs := List(ElementsOfIncidenceStructure(ProjectiveSpace(ps!.dimension-1,f), 1), 
                     x -> [x!.obj]);
       else
         subs := List(ElementsOfIncidenceStructure(ProjectiveSpace(ps!.dimension-1,f), j-1), 
                     x -> x!.obj);
       fi;
       vars := Union(List(subs, x -> 
                 List(VectorSpaceTransversal(vec, x), y -> Wrap(ps, j, [y,x]))));
       return IteratorList( vars );
    fi;
  end );
  
  
InstallMethod( Enumerator, "for subspaces of an affine space",
        [ IsAllSubspacesOfAffineSpace ],  
  function( vs )
  ## An affine variety will be represented by a pair (vector,direction).
  ## So for example, an affine plane x+<W> will be represented by
  ## (x', proj. line)  (where x' is the transversal rep corresponding to x).
  
    local as, j, vars, vec, subs, f, enum, enumV, classsize;
    as := vs!.geometry;
    j := vs!.type;
    vec := as!.vectorspace;
    f := as!.basefield;
    if j = 1 then 
       enumV := Enumerator( vec );
       enum := EnumeratorByFunctions( vs, rec(
            ElementNumber := function(e, n)
              local v;
              v := enumV[n];
              ConvertToVectorRep(v, f);
              return Wrap(as, 1, v);
            end,
            NumberElement := function(e, x)
              local v;
              v := x!.obj;
              return Position(enumV, v);
            end ));    
    else
       enumV := Enumerator( Subspaces( vec, j-1 ) ); 
       classsize := Size(f)^(Dimension(vec)-j+1);
       enum := EnumeratorByFunctions( vs, rec(
            ElementNumber := function(e, n)
               local l, k, enumtrans, v; 
               
               ## The way this works is that n is the position
               ## of the l-th coset incident with the k-th (j-1)-space
               ## of as.      
          
               l := n mod classsize;
               if l = 0 then l := classsize; fi;  
               k := (n-l) / classsize + 1;  
               v := BasisVectors(Basis(enumV[k]));
               enumtrans := Enumerator( VectorSpaceTransversal(vec, v) );
               return Wrap(as, j, [ enumtrans[l], v ] );
            end,
            NumberElement := function( e, x )
              local w, k, enumtrans, l;
              ## Here we must first find the unique direction of x
              ## incident with x, and then find its place in the ordering.
              w := x!.obj[2];        
              k := Position(enumV, w);  
              enumtrans := Enumerator( VectorSpaceTransversal(vec, w) );
              l := Position(enumtrans, x!.obj[1]);
              return (k-1)*classsize + l;
            end ) );
     fi;
    return enum;
 end );    
     
  
  



#############################################################################
#
# Display methods
#
#############################################################################

InstallMethod( ViewObj, [ IsAffineSpace and IsAffineSpaceRep ],
  function( p )
    Print("AG(",p!.dimension,", ",Size(p!.basefield),")");
  end );

InstallMethod( PrintObj, [ IsAffineSpace and IsAffineSpaceRep ],
  function( p )
    Print("AffineSpace(",p!.dimension,",",p!.basefield,")");
  end );  

InstallMethod( ViewObj, [ IsAllSubspacesOfAffineSpace and
  IsAllSubspacesOfAffineSpaceRep ],
  function( vs )
    Print("<",TypesOfElementsOfIncidenceStructurePlural(vs!.geometry)[vs!.type]," of ");
    ViewObj(vs!.geometry);
    Print(">");
  end );

InstallMethod( PrintObj, [ IsAllSubspacesOfAffineSpace and
  IsAllSubspacesOfProjectiveSpaceRep ],
  function( vs )
    Print("Elements( ",vs!.geometry," , ",vs!.type,")");
  end );

# special method to view an affine subspace as it is usually thought of

InstallMethod( Display, [ IsSubspaceOfAffineSpace ],
  function( x )
    local u, t;
    u := x!.obj;
    if x!.type = 1 then
      Print("Affine point: ");
      Display( u );
    else
      if x!.type in [2, 3, 4] then
         Print("Affine ", TypesOfElementsOfIncidenceStructure(x!.geo)[x!.type], ":\n");
      else
         Print("Affine ", x!.type, "-space :\n");
      fi;
      Print("Coset representative: ", u[1], "\n" );
      Print("Coset (direction): ", u[2], "\n" );      
    fi;
  end );

InstallMethod( ViewObj, [ IsVectorSpaceTransversal and IsVectorSpaceTransversalRep ],
  function( trans )
    Print("<vector space transversal of ", trans!.space,">");
  end );

#############################################################################
#
# Basic methods: IsIncident, Span, Meet, IsParallel, ProjectiveCompletion
#
#############################################################################

InstallMethod( IsIncident,  [IsSubspaceOfAffineSpace, IsSubspaceOfAffineSpace],
  function( x, y )
    local ambx, amby, typx, typy, mat,
          zero, nrows, ncols, vectors, 
          nvectors, i, j, z, nzheads, row;
    ambx := x!.geo;
    amby := y!.geo;
    typx := x!.type;
    typy := y!.type;
    
    ## x + <y> inc with a + <b> iff a-x in <b> and <y> subset of <b> 

    if ambx!.vectorspace = amby!.vectorspace then   

    ## First step: check that the translations are compatible,
    ## that is, that x!.obj[1] - y!.obj[1] is in the subspace
    ## spanned by "vectors"

       if typx = 1 and typy > 1 then
          return x!.obj - y!.obj[1] in Subspace(ambx!.vectorspace, y!.obj[2]);
       elif typy = 1 and typx > 1 then
          return y!.obj - x!.obj[1] in Subspace(ambx!.vectorspace, x!.obj[2]);
       elif typx = 1 and typy = 1 then
          return x = y;
       elif typx >= typy and typy > 1 then
          return x!.obj[1] - y!.obj[1] in Subspace(ambx!.vectorspace, x!.obj[2]);
       else 
          return x!.obj[1] - y!.obj[1] in Subspace(ambx!.vectorspace, y!.obj[2]);
       fi;

   ## Second step: checking that the directions are compatible.
   ## Algorithm is the same as for projective spaces.
   ## Note that here we will have typx, typy > 1.

       if typx >= typy then
          vectors := x!.obj[2];
          nvectors := typx-1;
          mat := MutableCopyMat(y!.obj[2]);
          nrows := typy;
       else
          vectors := y!.obj[2];
          nvectors := typy-1;
          mat := MutableCopyMat(x!.obj[2]);
          nrows := typx;
       fi;

       ncols:= amby!.dimension + 1;
       zero:= Zero( mat[1][1] );

   # here we are going to treat "vectors" as a list of basis vectors. first
   # figure out which column is the first nonzero column for each row
       nzheads := [];
       for i in [ 1 .. nvectors ] do
         row := vectors[i];
         j := PositionNot( row, zero );
         Add(nzheads,j);
       od;

       # now try to reduce each row of "mat" with the basis vectors
       for i in [ 1 .. nrows ] do
         row := mat[i];
         for j in [ 1 .. Length(nzheads) ] do
             z := row[nzheads[j]];
             if z <> zero then
               AddRowVector( row, vectors[ j ], - z );
             fi;
         od;

         # if the row is now not zero then y is not a subspace of x
         j := PositionNot( row, zero );
         if j <= ncols then
                return false;
         fi;

      od;
      
      return true;
    else
      Error( "type is unknown or not implemented" );
    fi;
    return false;
  end );


## An affine space is a complete lattice 
## (with the empty set as bottom element).

InstallMethod( Span, [IsSubspaceOfAffineSpace, IsSubspaceOfAffineSpace],
  function( x, y )  
    local ux1, uy1, ux2, uy2, ambx, amby, typx, typy, span, temp;
    ambx := AmbientSpace(x!.geo);
    amby := AmbientSpace(y!.geo);
    typx := x!.type;
    typy := y!.type;

    ## affine span of x + A and y + B is 
    ##  x + <y-x, A,B>

    if ambx!.vectorspace = amby!.vectorspace then
      if typx = 1 then ux1 := x!.obj; ux2 := [];
      else ux1 := x!.obj[1]; ux2 := x!.obj[2];
      fi;
      if typy = 1  then uy1 := y!.obj; uy2 := [];
      else uy1 := x!.obj[1]; uy2 := y!.obj[2];
      fi;  
      span := MutableCopyMat(ux2);
      Append(span, uy2);
      Append(span, [uy1-ux1]); 
      span := MutableCopyMat(SemiEchelonMat(span).vectors);
      # if the span is [], then x=y, so return x.
      if Length(span) = 0 then
        return x;
      fi;
      # if the span is the whole space, return that.
      if Length(span) = ambx!.dimension + 1 then
        return ambx;
      fi;      
      TriangulizeMat(span);
      return Wrap(ambx, Rank(span)+1, 
       [VectorSpaceTransversalElement(ambx!.vectorspace,span,ux1), span]);
    else
      Error("Subspaces belong to different ambient spaces");
    fi;
    return;
  end );


InstallMethod( Meet, [IsSubspaceOfAffineSpace, IsSubspaceOfAffineSpace],
  function( x, y )
    local ag, ux1, uy1, ux2, uy2, typx, typy, int, 
          rep, t, f, vec, rk, trans;
    typx := x!.type;
    typy := y!.type; 
    ag := x!.geo;
    f := ag!.basefield; 
    vec := ag!.vectorspace;

  ## Cases for the intersection of x + A and y + B
  ## (i) A int B = 0 => (x+A) int (y+B) is a point or empty
  ## (ii) dim(A int B) = 1 => (x+A) int (y+B) is a point or empty
  ## (iii) dim(A int B) > 1 => (x+A) int (y+B) is subspace of 
  ## dimension dim(A int B), or empty
  ## (iv) A empty or B empty => equal or empty 

    if vec = y!.geo!.vectorspace then 
      
    ## redundant cases 
      if x = y then 
         return x;
      elif (typx = 1 or typy = 1) then
         return [];
      fi;

      ux1 := x!.obj[1]; ux2 := x!.obj[2];
      uy1 := y!.obj[1]; uy2 := y!.obj[2];
    ## parallel spaces
      if ux2 = uy2 then return []; fi;
    ## find intersection of two spaces
      int := SumIntersectionMat(ux2, uy2)[2];
      if not IsEmpty(int) and Rank(int) > 0 then 
          int := MutableCopyMat(int);
          TriangulizeMat(int);
          rk := Rank(int) + 1;        
      ## Now check to see if the affine intersection
      ## is empty. We will need a representative anyway.
          trans := VectorSpaceTransversal(vec, int);
          rep := 0;
          for t in trans do
            if Rank(Union([t-ux1], ux2)) = Rank(ux2) and
               Rank(Union([t-uy1], uy2)) = Rank(uy2) then
               rep := t; break;
            fi;
          od;
          if rep = 0 then 
             return []; 
          elif rk = 1 then  
             return Wrap( ag, rk, rep);
          fi;
          return Wrap( ag, rk, [rep, int]);
      else   ## case (i)
          rep := 0;
          for t in vec do
            if not IsZero(t) and
               Rank(Union([t-ux1], ux2)) = Rank(ux2) and
               Rank(Union([t-uy1], uy2)) = Rank(uy2) then
               rep := t; break;
            fi;
          od;
          if rep = 0 then 
             return [];
          else
             return Wrap( ag, 1, rep);
          fi;
      fi;
    else
      Error("Subspaces belong to different ambient spaces");
    fi;
    return;
  end );


InstallMethod( IsParallel, "for two affine subspaces of the same dimension",
           [ IsSubspaceOfAffineSpace, IsSubspaceOfAffineSpace ],
  function( a, b );
    if a!.type <> a!.type then
       Error("Subspaces must be of the same dimension");
    fi;
    if a!.geo <> a!.geo then
       Error("Ambient affine spaces must be the same");
    fi;
    if a!.type = 1 then
       return true;
    else
       return a!.obj[2] = b!.obj[2];
    fi;
  end );

InstallMethod( ProjectiveCompletion, [ IsAffineSpace ],
  function( as )
    # Returns an embedding of an affine space
    # into a projective space (its projective completion). 
    # For example, the point (x, y, z) goes to <(1, x, y, z)>    
    # An intertwiner is unnecessary, CollineationGroup(as) is 
    # subgroup of CollineationGroup(ps).

    local d, gf, ps, func, pre, map, morphism, vec, one, hom;
    d := as!.dimension;
    gf := as!.basefield;
    ps := ProjectiveSpace(d, gf);
    one := One(gf);    
    vec := as!.vectorspace;

    func := function( x )
      local repx, subspace, n, trans, new, i, j;
      repx := x!.obj;
      n := x!.type;
      if not x in as then 
         Error("Subspace is not an element of the domain (affine space)");
      fi;
      if n > 1 then      
         subspace := repx[2];
         trans := repx[1];
         # simply put all vectors together and put 0's in the first column
         new := NullMat(n, d+1, gf);
         new{[1..n-1]}{[2..d+1]} := subspace;
         new[n][1] := one;
         new[n]{[2..d+1]} := trans; 
      else
         new := Concatenation([one], repx); 
      fi;
      return VectorSpaceToElement(ps, new);
    end;
 
    pre := function( y )
      local n, repy, subspace, trans, new, zerov, hyp, elm;
      n := y!.type;
      repy := y!.obj;
      if not y in ps then 
         Error("Subspace is not an element of the range (projective space)");
      fi;
      if n > 1 then
         zerov := NullMat(1,d,gf);
         hyp := TransposedMat(Concatenation(zerov, IdentityMat(d, gf)));
         subspace := SumIntersectionMat(hyp, repy)[2];
         repy := SortedList(y!.obj);  
         # Make use of lexicographic ordering
         # Zero at front gives the parallel class       
         trans := repy[n];
      
         # curtail
         trans := trans{[2..d+1]};
         subspace := subspace{[1..n-1]}{[2..d+1]}; 
         new := VectorSpaceTransversalElement(vec, subspace, trans);
         elm := Wrap(as, n, [new, subspace]);
      else
         trans := repy{[2..d+1]};
         elm := Wrap(as, 1, trans);
      fi;
      return elm;
    end;

    map := GeometryMorphismByFunction(ElementsOfIncidenceStructure(as), 
                                      ElementsOfIncidenceStructure(ps), func, false, pre);
    SetIsInjective(map, true);  
    return map;
end );

#############################################################################
#
#  Methods for shadows and parallel classes
#
#  (We use the completion to the projective space)
#
#
#############################################################################


InstallMethod( ShadowOfElement, [IsAffineSpace, IsSubspaceOfAffineSpace, IsPosInt],
  function( as, v, j )   
    return Objectify(
      NewType( ElementsCollFamily, IsElementsOfIncidenceStructure and
                                   IsShadowSubspacesOfAffineSpace and
                                   IsShadowSubspacesOfAffineSpaceRep),
        rec( geometry := as, type := j, list := [v] ) );
  end );
  
InstallMethod( ShadowOfFlag, [IsAffineSpace, IsList, IsPosInt],
  function( as, flag, j )
        #   empty flag - return all subspaces of the right type
    if IsEmpty(flag) then
      return ElementsOfIncidenceStructure(as, j);
    fi;

    return Objectify(
      NewType( ElementsCollFamily, IsElementsOfIncidenceStructure and
                                   IsShadowSubspacesOfAffineSpace and
                                   IsShadowSubspacesOfAffineSpaceRep),
        rec( geometry := as, type := j, list := flag )
      );
  end);
  
InstallMethod( ParallelClass, "for an affine space and subspace",
          [IsAffineSpace, IsSubspaceOfAffineSpace], 
  function( as, v )
    if v!.type = 0 then
       Error("Subspace must be nontrivial");
    elif v!.type = 1 then
       return Points( as );     
    else
      return Objectify(
        NewType( ElementsCollFamily, IsElementsOfIncidenceStructure and
                                   IsParallelClassOfAffineSpace and
                                   IsParallelClassOfAffineSpaceRep),
                 rec( geometry := as, element := v ) );
    fi;      
  end );
  
InstallMethod( ParallelClass, "for an affine subspace", [ IsSubspaceOfAffineSpace ], 
  x -> ParallelClass( x!.geo, x ) );


InstallMethod( Iterator, "for a parallel class of an affine space",
        [IsParallelClassOfAffineSpace and IsParallelClassOfAffineSpaceRep ],
  function( pclass )
    local as, v, type, direction, vec, elms;
    as := pclass!.geometry;
    v := pclass!.element;
    type := v!.type;
    direction := v!.obj[2];
    vec := as!.vectorspace;
    
    ## Do the trivial cases:
    if type = 1 then 
       ## it already has an iterator...
       return Iterator( pclass );
    fi;
  
    elms := List(VectorSpaceTransversal(vec, direction), y -> Wrap(as, type, [y,direction]) );
    return IteratorList( elms ); 
  end );

InstallMethod( Size, [IsShadowSubspacesOfAffineSpace and
                      IsShadowSubspacesOfAffineSpaceRep ],
  function( vs )
    local ps, list, map, shad, j, as;
    as := vs!.geometry;
    j := vs!.type;
    map := ProjectiveCompletion(as);
    ps := Range(map)!.geometry;
    list := vs!.list;    
    if Size( list ) = 1 then
       shad := ShadowOfElement( ps, ImageElm(map, list[1]), j);
    else
       shad := ShadowOfFlag( ps, ImagesSet(map, list), j);
    fi;
    return Size( shad );
  end);


InstallMethod( Iterator, "for a shadow in an affine space",
     [IsShadowSubspacesOfAffineSpace and IsShadowSubspacesOfAffineSpaceRep ],
  function( vs )
    local as, j, ps, map, iter, list;
    as := vs!.geometry;
    j := vs!.type;
    map := ProjectiveCompletion(as);
    ps := Range(map)!.geometry;
    list := vs!.list;    
    
    if Size( list ) = 1 then
       iter := Iterator( ShadowOfElement( ps, ImageElm(map, list[1]), j) );
    else
       iter := Iterator( ShadowOfFlag( ps, ImagesSet(map, list), j) );
    fi;
       
    return IteratorByFunctions( rec(
      NextIterator := function(iter)
        local x;
        x := NextIterator(iter!.S);
        return PreImageElm(map, x);
      end,
      IsDoneIterator := iter -> IsDoneIterator(iter!.S),
      ShallowCopy := iter -> rec( S := ShallowCopy(iter!.S) ),
      S := iter )    
    );
  end);


#############################################################################
#
# Some view methods for shadows and parallel classes
#
#############################################################################


InstallMethod( ViewObj, [ IsShadowSubspacesOfAffineSpace and
                          IsShadowSubspacesOfAffineSpaceRep ],
  function( vs )
    Print("<shadow ",TypesOfElementsOfIncidenceStructurePlural(vs!.geometry)[vs!.type]," in ");
    ViewObj(vs!.geometry);
    Print(">");
  end );
  
InstallMethod( ViewObj, [ IsParallelClassOfAffineSpace and
                          IsParallelClassOfAffineSpaceRep ],
  function( vs )
    Print("<parallel class of ",
   TypesOfElementsOfIncidenceStructurePlural(vs!.geometry)[vs!.element!.type]," in ");
    ViewObj(vs!.geometry);
    Print(">");
  end );



  
#############################################################################
#
# Nice shorthand methods for shadows of elements
#
#############################################################################


InstallMethod( Points, [ IsSubspaceOfAffineSpace ],
  function( var )
    return ShadowOfElement(var!.geo, var, 1);
  end );

InstallMethod( Points, [ IsAffineSpace, IsSubspaceOfAffineSpace ],
  function( geo, var )
    return ShadowOfElement(geo, var, 1);
  end );

InstallMethod( Lines, [ IsSubspaceOfAffineSpace ],
  function( var )
    return ShadowOfElement(var!.geo, var, 2);
  end );

InstallMethod( Lines, [ IsAffineSpace, IsSubspaceOfAffineSpace ],
  function( geo, var )
    return ShadowOfElement(geo, var, 2);
  end );

InstallMethod( Planes, [ IsSubspaceOfAffineSpace ],
  function( var )
    return ShadowOfElement(var!.geo, var, 3);
  end );

InstallMethod( Planes, [ IsAffineSpace, IsSubspaceOfAffineSpace ],
  function( geo, var )
    return ShadowOfElement(geo, var, 3);
  end );

InstallMethod( Solids, [ IsSubspaceOfAffineSpace ],
  function( var )
    return ShadowOfElement(var!.geo, var, 4);
  end );

InstallMethod( Solids, [ IsAffineSpace, IsSubspaceOfAffineSpace ],
  function( geo, var )
    return ShadowOfElement(geo, var, 4);
  end );
