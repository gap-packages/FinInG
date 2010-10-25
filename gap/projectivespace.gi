#############################################################################
##
##  projectivespace.gi              FinInG package
##                                                              John Bamberg
## 								Anton Betten
##                                                              Jan De Beule
##                                                             Philippe Cara
## 		                                              Michel Lavrauw
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
##  Implementation stuff for projective spaces.
##
#############################################################################

#############################################################################
# Constructor methods:
#############################################################################

## To do
#  Use compressed matrices



InstallMethod( Wrap, "for a projective space and an object",
  [IsProjectiveSpace, IsPosInt, IsObject],
  function( geo, type, o )
    local w;
    w := rec( geo := geo, type := type, obj := o );
    Objectify( NewType( SoPSFamily, IsElementOfIncidenceStructure and
      IsElementOfIncidenceStructureRep and IsSubspaceOfProjectiveSpace ), w );
    return w;
  end );

InstallMethod( \^, [ IsSubspaceOfProjectiveSpace, IsUnwrapper ],
# If the object "v" to be unwrapped is a point of a vector space, then we do not want to use
# return v!.obj, but we want to return a list with one vector, i.e. [v!.obj]
	function( v, u )
	  if v!.type = 1 then return [v!.obj];
	  else return v!.obj;
	  fi;
	end );

InstallMethod( ProjectiveSpace, "for a proj dimension and a field",
  [ IsPosInt, IsField ],
  function( d, f )
    local geo, ty;
    geo := rec( dimension := d, basefield := f, 
                vectorspace := FullRowSpace(f, d+1) );
    ty := NewType( GeometriesFamily,
                  IsProjectiveSpace and IsProjectiveSpaceRep );
    Objectify( ty, geo );
    SetAmbientSpace(geo,geo);
    return geo;
  end );

InstallMethod( ProjectiveSpace, "for a proj dimension and a prime power",
  [ IsPosInt, IsPosInt ],
  function( d, q )
          return ProjectiveSpace(d, GF(q));
  end );

InstallOtherMethod(\QUO,  "quotients for projective spaces",
   [ IsProjectiveSpace and IsProjectiveSpaceRep, IsSubspaceOfProjectiveSpace],
  function( ps, v )
    if not v in ps then 
       Error( "Subspace does not belong to the projective space" );
    fi;
    return Range( NaturalProjectionBySubspace( ps, v ) )!.geometry;
  end );

InstallMethod( ProjectiveDimension, "for a projective space",
  [ IsProjectiveSpace and IsProjectiveSpaceRep ],
  function( ps )
    return ps!.dimension;
  end );

InstallMethod( ProjectiveDimension, "for a subspace of a projective space",
     [ IsSubspaceOfProjectiveSpace ],
  function( v )
    return v!.type - 1;
  end );


InstallMethod( ProjectiveDimension, [ IsEmpty ], function(x) return -1;end );

InstallMethod( Dimension, "for a projective space",
  [ IsProjectiveSpace and IsProjectiveSpaceRep ],
  function( ps )
    return ps!.dimension;
  end );

InstallMethod( Dimension, "for a subspace of a projective space",
     [ IsSubspaceOfProjectiveSpace ],
  function( v )
    return v!.type - 1;
  end );

InstallMethod( Dimension, [ IsEmpty ], function(x) return -1;end );

InstallMethod( UnderlyingVectorSpace, "for a projective space",
   [IsProjectiveSpace and IsProjectiveSpaceRep],
   function(ps)
   return ShallowCopy(ps!.vectorspace);
end);

InstallMethod( UnderlyingVectorSpace, "for a subspace of a projective space",
	[IsSubspaceOfProjectiveSpace],
	function(subspace)
	local vspace,W;
	vspace:=UnderlyingVectorSpace(subspace!.geo);
	W:=SubspaceNC(vspace,subspace!.obj);
	return W;
end);

InstallMethod( StandardFrame, "for a projective space", [IsProjectiveSpace], 
	# if the dimension of the projective space is n, then StandardFrame 
	# makes a list of points with coordinates 
	# (1,0,...0), (0,1,0,...,0), ..., (0,...,0,1) and (1,1,...,1) 
  function( pg )
	local bas, frame, unitpt;
	if not pg!.dimension > 0 then 
		Error("The argument needs to be a projective space of dimension at least 1!");
	else
	bas:=Basis(pg!.vectorspace);	
	frame:=List(BasisVectors(bas),v->VectorSpaceToElement(pg,v));
	unitpt:=VectorSpaceToElement(pg,Sum(BasisVectors(bas)));
	Add(frame,unitpt);
	return frame;
	fi;
  end );

InstallMethod( StandardFrame, "for a subspace of a projective space", 
	[IsSubspaceOfProjectiveSpace],
	# if the dimension of the subspace is d (needs to be at least 1), then this returns d+2
	# points of the subspace, the first d+1 are the points "basispoints"
	# the last point has as coordinates the sum of the basispoints.
  function( subspace )
	local list,v;
	if not Dimension(subspace) > 0 then 
		Error("The argument needs to be a projective space of dimension at least 1!");
	else
	list:=ShallowCopy(subspace!.obj);
        Add(list,Sum(subspace!.obj));
	return List(list,v->VectorSpaceToElement(subspace!.geo,v));
	fi;
  end );

InstallMethod( Coordinates, "for a point of a projective space",
	[IsSubspaceOfProjectiveSpace],
  function( point )
	if not Dimension(point)=0 then Error("The argument is not a projective point");
	else return ShallowCopy(point!.obj);
	fi;
  end );
  
InstallMethod( CoordinatesOfHyperplane, "for a hyperplane of a projective space",
	[IsSubspaceOfProjectiveSpace],
	function(hyp)
	local pg,perp;
	pg:=ShallowCopy(hyp!.geo);
	if not hyp!.type=Dimension(pg) then 
		Error("The argument is not a hyperplane");
	else 
		perp:=StandardDualityOfProjectiveSpace(pg);
		return Coordinates(hyp^perp);
	fi;
end );

InstallMethod( EquationOfHyperplane, "for a hyperplane of a projective space",
	[IsSubspaceOfProjectiveSpace],
	function(hyp)
	local pg,r,v,indets;
	pg:=AmbientGeometry(hyp);
	r:=PolynomialRing(pg!.basefield,pg!.dimension + 1);
	indets:=IndeterminatesOfPolynomialRing(r);
	v:=CoordinatesOfHyperplane(hyp);
	return Sum(List([1..Size(indets)],i->v[i]*indets[i]));
end );

InstallMethod( CollineationGroup, "for a full projective space",
  [ IsProjectiveSpace and IsProjectiveSpaceRep ],
  function( ps )
    local coll,d,f,frob,g,newgens,q,s,pow;
    f := ps!.basefield;
    q := Size(f);
    d := ProjectiveDimension(ps);
    g := GL(d+1,q);
    frob := FrobeniusAutomorphism(f);
    newgens := List(GeneratorsOfGroup(g),x->[x,frob^0]);
    Add(newgens,[One(g),frob]);
    newgens := ProjElsWithFrob(newgens);
    coll := GroupWithGenerators(newgens);
    pow := LogInt(q, Characteristic(f));
    s := pow * q^(d*(d+1)/2)*Product(List([2..d+1], i->q^i-1)); 
    if pow > 1 then 
       SetName( coll, Concatenation("PGammaL(",String(d+1),",",String(q),")") );
    else
       SetName( coll, Concatenation("PGL(",String(d+1),",",String(q),")") );
    fi;
    SetSize( coll, s );    
    return coll;
  end );

InstallMethod( HomographyGroup, "for a full projective space",
  [ IsProjectiveSpace ],
  function( ps )
    local gg,d,f,frob,g,newgens,q,s;
    f := ps!.basefield;
    q := Size(f);
    d := ProjectiveDimension(ps);
    g := GL(d+1,q);
    frob := FrobeniusAutomorphism(f);
    newgens := List(GeneratorsOfGroup(g),x->[x,frob^0]);
    newgens := ProjElsWithFrob(newgens);
    gg := GroupWithGenerators(newgens);
    s := q^(d*(d+1)/2)*Product(List([2..d+1], i->q^i-1)); 
    SetName( gg, Concatenation("PGL(",String(d+1),",",String(q),")") );
    SetSize( gg, s );
    return gg;
  end );

InstallMethod( SpecialHomographyGroup, "for a full projective space",
  [ IsProjectiveSpace ],
  function( ps )
    local gg,d,f,frob,g,newgens,q,s;
    f := ps!.basefield;
    q := Size(f);
    d := ProjectiveDimension(ps);
    g := SL(d+1,q);
    frob := FrobeniusAutomorphism(f);
    newgens := List(GeneratorsOfGroup(g),x->[x,frob^0]);
    newgens := ProjElsWithFrob(newgens);
    gg := GroupWithGenerators(newgens);
    s := q^(d*(d+1)/2)*Product(List([2..d+1], i->q^i-1)) / GCD_INT(q-1, d+1);
    SetName( gg, Concatenation("PSL(",String(d+1),",",String(q),")") );
    SetSize( gg, s );    
    return gg;
  end );

InstallMethod( RankAttr, "for a projective space",
  [ IsProjectiveSpace and IsProjectiveSpaceRep ],
  function( ps )
    return ps!.dimension;
  end );

InstallMethod( BaseField, "for a projective space", [IsProjectiveSpace],
  pg -> pg!.basefield );
  
InstallMethod( RepresentativesOfElements, "for a projective space", [IsProjectiveSpace],
  function( ps )
    local d, gf, id, elts;  
    d := ProjectiveDimension(ps);
    gf := BaseField(ps);
    id := IdentityMat(d+1,gf);
    elts := List([1..d], i -> VectorSpaceToElement(ps, id{[1..i]}));
    return elts;
  end );

InstallMethod( TypesOfElementsOfIncidenceStructure, "for a projective space", [IsProjectiveSpace],
  function( ps )
    local d,i,types;
    types := ["point"];
    d := ProjectiveDimension(ps);
    if d >= 2 then Add(types,"line"); fi;
    if d >= 3 then Add(types,"plane"); fi;
    if d >= 4 then Add(types,"solid"); fi;
    for i in [5..d] do
        Add(types,Concatenation("proj. ",String(i-1),"-space"));
    od;
    return types;
  end );

InstallMethod( TypesOfElementsOfIncidenceStructurePlural, "for a projective space",
  [IsProjectiveSpace],
  function( ps )
    local d,i,types;
    types := ["points"];
    d := ProjectiveDimension(ps);
    if d >= 2 then Add(types,"lines"); fi;
    if d >= 3 then Add(types,"planes"); fi;
    if d >= 4 then Add(types,"solids"); fi;
    for i in [5..d] do
        Add(types,Concatenation("proj. ",String(i-1),"-subspaces"));
    od;
    return types;
  end );

InstallMethod( \in, "for a subspace of a projective space and projective space",  
     [IsSubspaceOfProjectiveSpace, IsProjectiveSpace],
  function( x, dom )
    local ps;
    ps := x!.geo;
    return ps!.dimension = dom!.dimension and 
           ps!.basefield = dom!.basefield;
  end );

InstallMethod( \in, "for an element and set of elements",  
     [IsElementOfIncidenceStructure, IsElementsOfIncidenceStructure], 1*SUM_FLAGS+3,
  function( x, dom )
    return x in dom!.geometry and x!.type = dom!.type;
  end );

InstallMethod( \in, "for an element and domain",  
     [IsElementOfIncidenceStructure, IsAllElementsOfIncidenceStructure], 1*SUM_FLAGS+3,
  function( x, dom )
    return x in dom!.geometry;
  end );

InstallMethod(Size, "for subspaces of a projective space",
        [IsAllSubspacesOfProjectiveSpace],
        function( vs )
          return vs!.size;
        end);

InstallMethod( AsList, [IsAllSubspacesOfProjectiveSpace],
 function( vs )

  ## We use the package "orb" by Mueller, Neunhoeffer and Noeske,
  ## which is much quicker than using an iterator to get all of the
  ## projective subspaces of a certain dimension.

   local geo, g, p, o, type, sz;
   
   geo := vs!.geometry;
   g := ProjectivityGroup(geo);
   type := vs!.type; 
   sz := Size(vs);
   
   if type = 1 then
      o := MakeAllProjectivePoints(geo!.basefield, geo!.dimension);
      o := List(o, t -> Wrap(geo, type, t));;   
   else
     p := NextIterator(Iterator(vs));
     o := Orb(g, p, OnProjSubspaces, rec( hashlen:=Int(5/4*sz), 
                                          orbsizebound := sz ));
     Enumerate(o, sz);
   fi;
   return o;
 end );



######################################
#
# Put compressed matrices here....
#
#####################################


InstallMethod(Iterator,
  "for subspaces of a projective space",
        [IsAllSubspacesOfProjectiveSpace],
        function( vs )
          local ps, j, d, F;
          ps := vs!.geometry;
          j := vs!.type;
          d := ps!.dimension;
          F := ps!.basefield;
          return IteratorByFunctions( rec(
            NextIterator := function(iter)
              local mat;
              mat := NextIterator(iter!.S);
              mat := BasisVectors(Basis(mat));
              if j = 1 then
                mat := mat[1];
                ConvertToVectorRep(mat, F);
              else
                ConvertToMatrixRep(mat, F);
              fi;
                return Wrap(ps, j, mat);
            end,
            IsDoneIterator := function(iter)
              return IsDoneIterator(iter!.S);
            end,
            ShallowCopy := function(iter)
              return rec(
                S := ShallowCopy(iter!.S)
                );
            end,
            S := Iterator(Subspaces(ps!.vectorspace,j))
          ));
  end);

InstallMethod( ElementsOfIncidenceStructure, [IsProjectiveSpace, IsPosInt],
  function( ps, j )
    local r;
    r := Rank(ps);
    if j > r then
      Error("<geo> has no elements of type <j>");
    else
      return Objectify(
        NewType( ElementsCollFamily, IsElementsOfIncidenceStructure and
                                  IsAllSubspacesOfProjectiveSpace and
                                  IsAllSubspacesOfProjectiveSpaceRep),
          rec(
            geometry := ps,
            type := j,
            size := Size(Subspaces(ps!.vectorspace, j))
          )
       );
     fi;
  end);

InstallMethod( ElementsOfIncidenceStructure, [IsProjectiveSpace],
  function( ps )
    return Objectify(
      NewType( ElementsCollFamily, IsAllSubspacesOfProjectiveSpace ),
        rec( geometry := ps )
      );
  end);

InstallMethod( Size, [IsShadowSubspacesOfProjectiveSpace and
  IsShadowSubspacesOfProjectiveSpaceRep ],
  function( vs )
    return Size(Subspaces(vs!.factorspace,
      vs!.type - Size(vs!.inner)));
  end);

InstallMethod( Iterator, [IsShadowSubspacesOfProjectiveSpace and
  IsShadowSubspacesOfProjectiveSpaceRep ],
  function( vs )
    local ps, j, d, F;
    ps := vs!.geometry;
    j := vs!.type;
    d := ps!.dimension;
    F := ps!.basefield;
    return IteratorByFunctions( rec(
      NextIterator := function(iter)
        local mat;
        mat := NextIterator(iter!.S);
        mat := MutableCopyMat(Concatenation(
          BasisVectors(Basis(mat)),
          iter!.innermat
          ));
        TriangulizeMat(mat);
        if j = 1 then
          mat := mat[1];
          ConvertToVectorRep(mat, F);
        else
          ConvertToMatrixRep(mat, F);
        fi;
          return Wrap(ps, j, mat);
      end,
      IsDoneIterator := function(iter)
        return IsDoneIterator(iter!.S);
      end,
      ShallowCopy := function(iter)
        return rec(
          innermat := iter!.innermat,
          S := ShallowCopy(iter!.S)
          );
      end,
      innermat := vs!.inner,
      S := Iterator(Subspaces(vs!.factorspace,j-Size(vs!.inner)))
    ));
  end);

InstallMethod( ShadowOfElement, [IsProjectiveSpace, IsElementOfIncidenceStructure, IsPosInt],
  function( ps, v, j )
    local localinner, localouter, localfactorspace;
    if j < v!.type then
      localinner := [];
      localouter := v!.obj;
    elif j = v!.type then
      localinner := v!.obj;
      localouter := localinner;
    else
      localinner := v!.obj;
      localouter := BasisVectors(Basis(ps!.vectorspace));
    fi;
    
    if IsVector(localinner) and not IsMatrix(localinner) then
      localinner := [localinner]; fi;
    if IsVector(localouter) and not IsMatrix(localouter) then
      localouter := [localouter]; fi;
    localfactorspace := Subspace(ps!.vectorspace,
      BaseSteinitzVectors(localouter, localinner).factorspace);
    return Objectify(
      NewType( ElementsCollFamily, IsElementsOfIncidenceStructure and
                                IsShadowSubspacesOfProjectiveSpace and
                                IsShadowSubspacesOfProjectiveSpaceRep),
        rec(
          geometry := ps,
          type := j,
          inner := localinner,
          outer := localouter,
          factorspace := localfactorspace,
		  parentflag := v,
          size := Size(Subspaces(localfactorspace))
        )
      );
  end);
  

InstallMethod( ShadowOfFlag, [IsProjectiveSpace,
    IsList, IsPosInt],
  function( ps, flag, j )
    local localinner, localouter, localfactorspace, v;
    
    #empty flag - return all subspaces of the right type
    if IsEmpty(flag) then
      return ElementsOfIncidenceStructure(ps, j);
    fi;
    
    # find the element of highest type less than j, and the subspace
    # of lowest type more than j.
    for v in flag do
      if v!.type = j then
        localinner := v;
        localouter := v;
      elif v!.type < j then
        if not IsBound(localinner) or v!.type > localinner!.type then
          localinner := v;
        fi;
      else
        if not IsBound(localouter) or v!.type < localouter!.type then
          localouter := v;
        fi;
      fi;
    od;
    
    # convert the subspaces into their respective matrices
    if IsBound(localinner) then
      if localinner!.type = 1 then
        localinner := [localinner!.obj];
      else
        localinner := localinner!.obj;
      fi;
    else
      localinner := [];
    fi;
    
    if IsBound(localouter) then
      if localouter!.type = 1 then
        localouter := [localouter!.obj];
      else
        localouter := localouter!.obj;
      fi;
    else
      localouter := BasisVectors(Basis(ps!.vectorspace));
    fi;
    
    localfactorspace := Subspace(ps!.vectorspace,
      BaseSteinitzVectors(localouter, localinner).factorspace);
    return Objectify(
      NewType( ElementsCollFamily, IsElementsOfIncidenceStructure and
                                IsShadowSubspacesOfProjectiveSpace and
                                IsShadowSubspacesOfProjectiveSpaceRep),
        rec(
          geometry := ps,
          type := j,
          inner := localinner,
          outer := localouter,
          factorspace := localfactorspace,
		  parentflag := flag,
          size := Size(Subspaces(localfactorspace))
        )
      );
  end);

#############################################################################
# Baer sublines and Baer subplanes:
#############################################################################

InstallMethod( BaerSublineOnThreePoints, [IsSubspaceOfProjectiveSpace,
                IsSubspaceOfProjectiveSpace, IsSubspaceOfProjectiveSpace],
 function( x, y, z )
  local geo, gfq2, gfq, t, subline;
  geo := AmbientSpace(x!.geo);
  gfq2 := geo!.basefield;
  gfq := GF(Sqrt(Size(gfq2)));

  ## Write z as x + ty
  
  t := First(gfq2, u -> Rank([z!.obj, x!.obj + u * y!.obj]) = 1); 

  ## Then the subline is just the set of points
  ## of the form x + w (ty), w in GF(q) (together
  ## with x and y of course).
  
  subline := List(gfq, w -> VectorSpaceToElement(geo, x!.obj + w * t * y!.obj));
  Add( subline, y );
  return subline;
end);

InstallMethod( BaerSubplaneOnQuadrangle, [IsSubspaceOfProjectiveSpace, 
         IsSubspaceOfProjectiveSpace, IsSubspaceOfProjectiveSpace, IsSubspaceOfProjectiveSpace],
 function( w, x, y, z )
  local geo, gfq2, gfq, s, t, subplane, coeffs, ow, ox, oy;
  geo := AmbientSpace(w!.geo);
  gfq2 := geo!.basefield;
  gfq := GF(Sqrt(Size(gfq2)));

  ## Write z as element in <w, x, y>
  
  coeffs := SolutionMat([w!.obj,x!.obj,y!.obj], z!.obj);
  ow := coeffs[1] * w!.obj;  
  ox := coeffs[2] * x!.obj;  
  oy := coeffs[3] * y!.obj;  

  ## Then just write down the subplane
  
  subplane := List(gfq, t -> VectorSpaceToElement(geo, ox + t * oy));
  Add( subplane, VectorSpaceToElement(geo, oy) );
  for s in gfq do
      for t in gfq do
          Add( subplane, VectorSpaceToElement(geo, ow + s * ox + t * oy));
      od;
  od;
  return subplane;
end);



#############################################################################
# Display methods:
#############################################################################

InstallMethod( ViewObj, [ IsProjectiveSpace and IsProjectiveSpaceRep ],
  function( p )
    Print("PG(",p!.dimension,", ",Size(p!.basefield),")");
  end );

InstallMethod( PrintObj, [ IsProjectiveSpace and IsProjectiveSpaceRep ],
  function( p )
          Print("ProjectiveSpace(",p!.dimension,",",p!.basefield,")");
  end );

InstallMethod( Display, [ IsProjectiveSpace and IsProjectiveSpaceRep ],
  function( p )
    Print("ProjectiveSpace(",p!.dimension,",",p!.basefield,")\n");
    if HasDiagramOfGeometry( p ) then
       Display( DiagramOfGeometry( p ) );
    fi;
  end );

#############################################################################
# Basic methods and functions:
#############################################################################

## some of this function is based on the
## SemiEchelonMat function. we save time by assuming that the matrix of
## each subspace is already in semiechelon form.
## method only applies to projective and polar spaces (where to put it!)

InstallMethod( IsIncident,  [IsSubspaceOfProjectiveSpace,
  IsSubspaceOfProjectiveSpace],
  function( x, y )
    local ambx, amby, typx, typy, mat,
          zero,      # zero of the field of <mat>
          nrows,
          ncols,     # number of columns in <mat>
          vectors,   # list of basis vectors
          nvectors,
          i,         # loop over rows
          j,         # loop over columns
          z,         # a current element
          nzheads,   # list of non-zero heads
          row;       # the row of current interest


    ambx := x!.geo;
    amby := y!.geo;
    typx := x!.type;
    typy := y!.type;
    
    if ambx!.vectorspace = amby!.vectorspace then
    
        if typx >= typy then
          vectors := x!.obj;
          nvectors := typx;
          mat := MutableCopyMat(y!.obj);
          nrows := typy;
        else
          vectors := y!.obj;
          nvectors := typy;
          mat := MutableCopyMat(x!.obj);
          nrows := typx;
        fi;
      # subspaces of type 1 need to be nested to make them lists of vectors

      if nrows = 1 then mat := [mat]; fi;
      if nvectors = 1 then vectors := [vectors]; fi;

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

        # if the row is now not zero then y is not a subvariety of x
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

InstallMethod( Span, [IsSubspaceOfProjectiveSpace, IsSubspaceOfProjectiveSpace],
  function( x, y )  
    local ux, uy, ambx, amby, typx, typy, span, temp, F;
    ambx := AmbientSpace(x!.geo);
    amby := AmbientSpace(y!.geo);
    typx := x!.type;
    typy := y!.type;
    F := ambx!.basefield;

    if ambx!.vectorspace = amby!.vectorspace then
      ux := ShallowCopy(x!.obj); 
      uy := ShallowCopy(y!.obj);
        
      if typx = 1 then ux := [ux]; fi;
      if typy = 1 then uy := [uy]; fi;

      span := MutableCopyMat(ux);
      Append(span,uy);
      span := MutableCopyMat(SemiEchelonMat(span).vectors);
      # if the span is the whole space, return that.
      if Length(span) = ambx!.dimension + 1 then
        return ambx;
      fi;
      
      TriangulizeMat(span);
      if Length(span) > 1 then
         ConvertToMatrixRep(span, F);
      fi;
	  # The next line generated an error
	  # temp := Wrap(ambx, Rank(span), span);
	  # So we replaced it by:
	  temp:=VectorSpaceToElement(ambx,span); 
	  return temp;
    else
      Error("Subspaces belong to different ambient spaces");
    fi;
    return;
  end );

InstallMethod( Span, [ IsHomogeneousList and IsSubspaceOfProjectiveSpaceCollection ],
  function( l )  
      # we trust that every member of the list
      # comes from a common geometry
    local unwrapped, r, unr, amb, span, temp, x, F;
	# first we check that all items in the list belong to the same ambient space
	if not Size(AsSet(List(l,x->AmbientSpace(x!.geo)!.dimension)))=1 and
			Size(AsSet(List(l,x->AmbientSpace(x!.geo)!.basefield)))=1 then 
	 Error("The elements in the list do not have a common ambient space");
	else
    x := l[1];
    amb := AmbientSpace(x!.geo);
    F := amb!.basefield;
    unwrapped := [];
    for r in l do
        unr := r!.obj;
        if r!.type = 1 then unr := [unr]; fi;
        Append(unwrapped, unr);
    od;

    span := MutableCopyMat(unwrapped);
    span := MutableCopyMat(SemiEchelonMat(span).vectors);

    if Length(span) = amb!.dimension + 1 then
       return amb;
    fi;
      
    TriangulizeMat(span);
    if Length(span) > 1 then
       ConvertToMatrixRep(span, F);
    fi;
	temp:=VectorSpaceToElement(amb,span);
    #temp := Wrap(amb, Rank(span), span);
    #if IsClassicalPolarSpace(x!.geo) and temp in x!.geo then
    #   return Wrap( x!.geo, Rank(span), span);
    #else
    #   return temp;
    #fi;
	fi;
    return temp;
  end );


InstallMethod( Meet, [IsSubspaceOfProjectiveSpace, IsSubspaceOfProjectiveSpace],
  function( x, y )
    local ux, uy, typx, typy, int, f, rk;
    typx := x!.type;
    typy := y!.type;
  
    if x!.geo!.vectorspace = y!.geo!.vectorspace then 
      ux := Unwrap(x); 
      uy := Unwrap(y);
        
      if typx = 1 then ux := [ux]; fi;
      if typy = 1 then uy := [uy]; fi;
      f := x!.geo!.basefield; 

      int := SumIntersectionMat(ux, uy)[2];

      if not IsEmpty(int) and Rank(int) > 0 then 
          int := MutableCopyMat(int);
          TriangulizeMat(int);
          rk := Rank(int);
          if rk = 1 then int := int[1]; fi;

          # if one of our varieties is in a polar space, we
          # can say that the meet is in the polar space.
      
          if IsClassicalPolarSpace(x!.geo) then
             return Wrap( x!.geo, rk, int);
          elif IsClassicalPolarSpace(y!.geo) then
             return Wrap( y!.geo, rk, int);
          else
             return Wrap( AmbientSpace(x!.geo), rk, int);
          fi;
      else 
          return [];
      fi;
    else
      Error("Subspaces belong to different ambient spaces");
    fi;
    return;
  end );

InstallMethod( Meet, [ IsHomogeneousList and IsSubspaceOfProjectiveSpaceCollection],
  function( l )  
      # we trust that every member of the list
      # comes from a common geometry.
      # We use recursion for this routine.
      # Not ideal, but there is no "SumIntersectionMat" for lists
    local int, iter;
    if not IsEmpty(l) then
       iter := Iterator(l);
       int := NextIterator(iter);
       repeat
         int := Meet(int, NextIterator(iter));
       until int = [] or IsDoneIterator(iter);
       return int;
    else return l;
    fi;
  end );
  
  
InstallMethod( Random, "for a collection of subspaces of a vector space",
                       [ IsSubspacesVectorSpace ],
	# chooses a random element out of the collection of subspaces of given dimension of a vectorspace
  function( subs )
    local d, vspace, V, W, w;

    ## the underlying vector space
    vspace := subs!.structure;
	
	if not IsInt(subs!.dimension) then
		Error("The subspaces of the collection need to have the same dimension");
	fi;

    ## the common dimension of elements of subs
    d := subs!.dimension;

    V:=[];
    repeat
      w := Random( vspace );
      Add(V, w);
      W := SubspaceNC( vspace, V );
    until Dimension(W) = d;

    return W;
  end );

InstallMethod( RandomSubspace,"for a vectorspace and a dimension",
                        [IsVectorSpace,IsInt],
        function(vspace,d)
                local list,W,w;

        if d>Dimension(vspace) then
                Error("The dimension of the subspace is larger than that of the vectorspace");
        fi;
		if not IsPosInt(d) then
				Error("The dimension of the subspace must be at least 1!");
		fi;

        list:=[];
        repeat
                w := Random( vspace );
                Add(list, w);
                W := SubspaceNC( vspace, list );
        until Dimension(W) = d;
        return W;

        end );  


InstallMethod( RandomSubspace,"for a projective space and a dimension",
                        [IsProjectiveSpace,IsInt],
        function(pg,d)
                local vspace,list,W,w;
        
        if d>ProjectiveDimension(pg) then
                Error("The dimension of the subspace is larger that of the projective space");
        fi;
		if IsNegInt(d) then
				Error("The dimension of the subspace must be at least 0!");
		fi;

        vspace:=pg!.vectorspace;
		W:=RandomSubspace(vspace,d+1);
        return(VectorSpaceToElement(pg,AsList(Basis(W))));

        end );

InstallMethod( RandomSubspace,"for a subspace of a projective space and a dimension",
                        [IsSubspaceOfProjectiveSpace,IsInt],
        function(subspace,d)
                local vspace,list,W,w;
        
        if d>ProjectiveDimension(subspace) then
                Error("The dimension of the random subspace is too large");
        fi;
		if IsNegInt(d) then
				Error("The dimension of the random subspace must be at least 0!");
		fi;
        vspace:=UnderlyingVectorSpace(subspace);
		W:=RandomSubspace(vspace,d+1);
        return(VectorSpaceToElement(subspace!.geo,AsList(Basis(W))));

        end );  

		
InstallMethod( Random, "for a collection of subspaces of a projective space",
                       [ IsAllSubspacesOfProjectiveSpace ],
        # chooses a random element out of the collection of subspaces of given
        # dimension of a projective space
  function( subs )
    	local d, pg, vspace, W, w;
	## the underlying projective space
	pg := subs!.geometry;
	vspace:=pg!.vectorspace;
	if not IsInt(subs!.type) then
          Error("The subspaces of the collection need to have the same dimension");
        fi;
	## the common type of elements of subs
	d := subs!.type;        
	W:=RandomSubspace(vspace,d);
        return(VectorSpaceToElement(pg,AsList(Basis(W))));
  end );
  

# HIER VERDER WERKEN !!!!!!

InstallMethod( Random, "for a collection of subspaces of a subspace of a projective space",
                       [ IsShadowSubspacesOfProjectiveSpace ],
        # chooses a random element out of the collection of subspaces of given
        # dimension of a subspace of a projective space
  function( subs )
    	local d, pg, subspace, vspace, W, w;
	## the underlying projective space
	pg := subs!.geometry;
	subspace:=subs!.parentflag;
	vspace:=UnderlyingVectorSpace(subspace);
	if not IsInt(subs!.type) then
          Error("The subspaces of the collection need to have the same dimension");
        fi;
	## the common type of elements of subs
	d := subs!.type;        
	W:=RandomSubspace(vspace,d);
        return(VectorSpaceToElement(pg,AsList(Basis(W))));
  end );


#####################################
#
#  VectorSpaceToElement methods
#
#####################################

## Things to check for (dodgy input)
## ---------------------------------
## - dimension
## - field
## - compress the matrix at the end
## - rank of matrix
## - an empty list

## Much of the following will need to change in the new
## version of GAP, with the new Row and Matrix types.

## Should we have methods for the new types given by the cvec package?
## Currently we don't load the cvec package.


InstallMethod( VectorSpaceToElement, "for a Plist",
  [IsProjectiveSpace, IsPlistRep],
  function( geom, v )
    local  x, n, i;
      ## when v is empty... 
      
      if IsEmpty(v) then
        return [];
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
         return [];
      fi;
      x := x{[1..n-i]};
      if Length(x)=ProjectiveDimension(geom)+1 then
         return geom;
      fi;
      
      ## It is possible that (a) the user has entered a
      ## matrix with one row, or that (b) the user has
      ## entered a matrix with rank 1 (thus at this stage
      ## we will have a matrix with one row).
      
      ## We must also compress our vector/matrix.
      
      if Length(x) = 1 then
         ConvertToVectorRep(x, geom!.basefield);
         return Wrap(geom, 1, x[1]);
      else
         ConvertToMatrixRep(x, geom!.basefield);
         return Wrap(geom, Length(x), x);
      fi;
 end );


InstallMethod( VectorSpaceToElement, "for a compressed GF(2)-matrix",
  [IsProjectiveSpace, IsGF2MatrixRep],
  function( geom, v )
    local  x, n, i;

    ## when v is empty... 
    if IsEmpty(v) then
      return [];
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
       return [];
    fi;
    x := x{[1..n-i]};
    if Length(x)=ProjectiveDimension(geom)+1 then
       return geom;
    fi;
  
    ## It is possible that (a) the user has entered a
    ## matrix with one row, or that (b) the user has
    ## entered a matrix with rank 1 (thus at this stage
    ## we will have a matrix with one row).
  
    ## We must also compress our vector/matrix.
  
    if Length(x) = 1 then
       ConvertToVectorRep(x, geom!.basefield);
       return Wrap(geom, 1, x[1]);
    else
       ConvertToMatrixRep(x, geom!.basefield);
       return Wrap(geom, Length(x), x);
    fi;
  end );
  
InstallMethod( VectorSpaceToElement, "for a compressed basis of a vector subspace",
  [IsProjectiveSpace, Is8BitMatrixRep],
  function( geom, v )
  local  x, n, i;

  ## when v is empty... 
  if IsEmpty(v) then
    return [];
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
     return [];
  fi;
  x := x{[1..n-i]};
  if Length(x)=ProjectiveDimension(geom)+1 then
     return geom;
  fi;

  ## It is possible that (a) the user has entered a
  ## matrix with one row, or that (b) the user has
  ## entered a matrix with rank 1 (thus at this stage
  ## we will have a matrix with one row).

  ## We must also compress our vector/matrix.

  if Length(x) = 1 then
     ConvertToVectorRep(x, geom!.basefield);
     return Wrap(geom, 1, x[1]);
  else
     ConvertToMatrixRep(x, geom!.basefield);
     return Wrap(geom, Length(x), x);
  fi;
end );

InstallMethod( VectorSpaceToElement, "for a row vector",
  [IsProjectiveSpace, IsRowVector],
  function( geom, vec )
    local  c;
      if IsZero(vec) then
         return [];
      fi;

      c := PositionNonZero( vec );
      if c <= Length( vec )  then
         vec := Inverse( vec[c] ) * vec;
      fi;
      return Wrap(geom, 1, vec);
  end );
  
InstallMethod( VectorSpaceToElement, "for a 8-bit vector",
  [IsProjectiveSpace, Is8BitVectorRep],
  function( geom, vec )
    local  c, gf;
      if IsZero(vec) then
         return [];
      fi;
      gf := BaseField(vec);
      c := PositionNonZero( vec );
      if c <= Length( vec )  then
         vec := Inverse( vec[c] ) * vec;
         ConvertToVectorRep( vec, gf );
      fi;
      return Wrap(geom, 1, vec);
  end );

InstallOtherMethod( Dimension,
   "for projective spaces",
   [IsProjectiveSpace],
   x->Rank(x)
);

