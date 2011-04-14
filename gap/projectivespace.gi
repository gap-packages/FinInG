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
 # This is an internal subroutine which is not expected to be used by the user;
 # they would be using VectorSpaceToElement.
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
# e.g. if p is a point of a projective space
# gap> p^_; 
# will return a list, with the coordinate vector of the point p
	function( v, u )
	  if v!.type = 1 then return [v!.obj];
	  else return v!.obj;
	  fi;
	end );

InstallMethod( ProjectiveSpace, "for a proj dimension and a field",
  [ IsInt, IsField ],
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

  
InstallMethod( \=, "for two projective spaces",
	[IsProjectiveSpace, IsProjectiveSpace],
	function(pg1,pg2);
	return UnderlyingVectorSpace(pg1) = UnderlyingVectorSpace(pg2);
end );

InstallMethod( ProjectiveSpace, "for a proj dimension and a prime power",
  [ IsInt, IsPosInt ],
  function( d, q )
          return ProjectiveSpace(d, GF(q));
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


#InstallMethod( ProjectiveDimension, [ IsEmpty ], function(x) return -1;end );

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

#InstallMethod( Dimension, [ IsEmpty ], function(x) return -1;end );

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
	if d <= -1 then Error("The dimension of the projective spaces needs to be at least 0");
	fi;
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
	if d <= -1 then Error("The dimension of the projective spaces needs to be at least 0");
	fi;
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
	if d <= -1 then Error("The dimension of the projective spaces needs to be at least 0");
	fi;
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

InstallMethod( Rank, "for a projective space",
  [ IsProjectiveSpace and IsProjectiveSpaceRep ],
  function( ps )
    return ps!.dimension;
  end );

InstallMethod( BaseField, "for a projective space", [IsProjectiveSpace],
  pg -> pg!.basefield );

InstallMethod( BaseField, "for an element of a projective space", [IsSubspaceOfProjectiveSpace],
  sub -> AmbientSpace(sub)!.basefield );
  
InstallMethod( RepresentativesOfElements, "for a projective space", [IsProjectiveSpace],
 # returns the canonical maximal flag
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
	# 1*SUM_FLAGS+3 increases the ranking for this method
     [IsElementOfIncidenceStructure, IsElementsOfIncidenceStructure], 1*SUM_FLAGS+3,
  function( x, dom )
    return x in dom!.geometry and x!.type = dom!.type;
  end );

InstallMethod( \in, "for an element and domain",  
	# 1*SUM_FLAGS+3 increases the ranking for this method
     [IsElementOfIncidenceStructure, IsAllElementsOfIncidenceStructure], 1*SUM_FLAGS+3,
  function( x, dom )
    return x in dom!.geometry;
  end );

InstallMethod( ElementsOfIncidenceStructure, [IsProjectiveSpace, IsPosInt],
  function( ps, j )
    local r;
    r := Rank(ps);
    if j > r then
      Error("<geo> has no elements of type <j>");
    else
      return Objectify(
        NewType( ElementsCollFamily, IsSubspacesOfProjectiveSpace and IsSubspacesOfProjectiveSpaceRep ),
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
      NewType( ElementsCollFamily, IsAllSubspacesOfProjectiveSpace and IsAllSubspacesOfProjectiveSpaceRep ),
        rec( geometry := ps )
      );
  end);
  
InstallMethod( AmbientSpace, [IsSubspaceOfProjectiveSpace],
	function(subspace)
		return subspace!.geo;
	end );
  
InstallMethod( AsList, [IsSubspacesOfProjectiveSpace],
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
 
 
 # One of the best features of all of the orb package is the FindSuborbits command
 # Here's an example
 #
# gap> pg:=PG(3,4);
#PG(3, 4)
#gap> lines:=AsList(Lines(pg));
#<closed orbit, 357 points>
#gap> g:=ProjectivityGroup(pg);
#PGL(4,4)
#gap> h:=SylowSubgroup(g,5);
#<projective semilinear group of size 25>
#gap> FindSuborbits(lines,GeneratorsOfGroup(h));
##I  Have suborbits, compiling result record...
#rec( o := <closed orbit, 357 points>, nrsuborbits := 21,
# reps := [ 1, 2, 3, 4, 5, 7, 9, 10, 12, 16, 18, 25, 26, 28, 36, 39, 56, 62,
#     124, 276, 324 ],
# words := [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
# lens := [ 25, 25, 25, 25, 5, 25, 25, 25, 25, 25, 5, 25, 25, 25, 5, 25, 5,
#     5, 5, 1, 1 ],
# suborbnr := [ 1, 2, 3, 4, 5, 3, 6, 6, 7, 8, 2, 9, 8, 4, 1, 10, 7, 11, 2, 6,
#     6, 8, 1, 7, 12, 13, 3, 14, 8, 12, 13, 10, 14, 12, 7, 15, 13, 1, 16, 9,
#     4, 8, 7, 2, 10, 12, 10, 1, 4, 10, 3, 8, 14, 7, 7, 17, 6, 1, 14, 9, 10,
#     18, 4, 9, 1, 3, 14, 12, 6, 1, 9, 8, 17, 8, 2, 13, 2, 4, 6, 16, 13, 13,
#     3, 3, 1, 10, 1, 14, 3, 12, 14, 14, 7, 10, 1, 14, 1, 4, 13, 2, 16, 2,
#     14, 16, 4, 9, 13, 12, 3, 14, 10, 6, 15, 12, 1, 16, 4, 6, 6, 8, 17, 12,
#     4, 19, 3, 13, 10, 9, 9, 16, 16, 7, 9, 4, 7, 1, 5, 3, 10, 19, 12, 13, 9,
#     2, 6, 10, 6, 16, 2, 2, 3, 6, 10, 4, 4, 16, 8, 14, 6, 13, 9, 12, 12, 16,
#     15, 13, 3, 9, 3, 10, 2, 1, 4, 7, 10, 8, 8, 14, 10, 11, 7, 9, 1, 8, 7,
#     2, 1, 12, 17, 4, 6, 15, 16, 16, 7, 14, 13, 14, 3, 8, 18, 12, 7, 16, 14,
#     6, 14, 7, 16, 13, 1, 9, 13, 1, 14, 18, 16, 16, 1, 17, 13, 6, 10, 16,
#     10, 6, 10, 7, 3, 18, 13, 15, 2, 3, 7, 8, 1, 4, 2, 2, 13, 7, 4, 8, 8, 2,
#     13, 14, 1, 10, 6, 10, 19, 6, 12, 12, 7, 13, 9, 2, 2, 7, 19, 2, 16, 14,
#     3, 13, 10, 5, 14, 12, 8, 8, 9, 20, 13, 14, 9, 12, 6, 9, 12, 13, 7, 12,
#     6, 11, 16, 4, 5, 2, 12, 10, 2, 12, 9, 9, 14, 14, 3, 11, 9, 8, 3, 8, 4,
#     16, 8, 1, 2, 12, 5, 4, 8, 11, 4, 3, 6, 6, 9, 3, 3, 21, 9, 4, 2, 8, 16,
#     16, 12, 3, 7, 7, 9, 6, 4, 8, 9, 14, 2, 3, 16, 7, 16, 1, 13, 4, 16, 4,
#     13, 10, 18, 12, 1, 10, 19 ],
# suborbs := [ [ 1, 15, 23, 38, 48, 58, 65, 70, 85, 87, 95, 97, 115, 136,
#         172, 183, 187, 211, 214, 219, 237, 249, 310, 346, 355 ],
#     [ 2, 11, 19, 44, 75, 77, 100, 102, 144, 149, 150, 171, 186, 233, 239,
#         240, 246, 260, 261, 264, 292, 295, 311, 327, 341 ],
#     [ 3, 6, 27, 51, 66, 83, 84, 89, 109, 125, 138, 151, 167, 169, 199, 229,
#         234, 267, 301, 305, 318, 322, 323, 332, 342 ],
#     [ 4, 14, 41, 49, 63, 78, 98, 105, 117, 123, 134, 154, 155, 173, 190,
#         238, 243, 290, 307, 314, 317, 326, 337, 348, 350 ],
#     [ 5, 137, 270, 291, 313 ],
#     [ 7, 8, 20, 21, 57, 69, 79, 112, 118, 119, 145, 147, 152, 159, 191,
#         206, 222, 226, 251, 254, 281, 287, 319, 320, 336 ],
#     [ 9, 17, 24, 35, 43, 54, 55, 93, 132, 135, 174, 181, 185, 195, 203,
#         208, 228, 235, 242, 257, 262, 285, 333, 334, 344 ],
#     [ 10, 13, 22, 29, 42, 52, 72, 74, 120, 157, 176, 177, 184, 200, 236,
#         244, 245, 273, 274, 304, 306, 309, 315, 328, 338 ],
#     [ 12, 40, 60, 64, 71, 106, 128, 129, 133, 143, 161, 168, 182, 212, 259,
#         275, 279, 282, 297, 298, 303, 321, 325, 335, 339 ],
#     [ 16, 32, 45, 47, 50, 61, 86, 94, 111, 127, 139, 146, 153, 170, 175,
#         179, 223, 225, 227, 250, 252, 269, 294, 352, 356 ],
#     [ 18, 180, 288, 302, 316 ],
#     [ 25, 30, 34, 46, 68, 90, 108, 114, 122, 141, 162, 163, 188, 202, 255,
#         256, 272, 280, 283, 286, 293, 296, 312, 331, 354 ],
#     [ 26, 31, 37, 76, 81, 82, 99, 107, 126, 142, 160, 166, 197, 210, 213,
#         221, 231, 241, 247, 258, 268, 277, 284, 347, 351 ],
#     [ 28, 33, 53, 59, 67, 88, 91, 92, 96, 103, 110, 158, 178, 196, 198,
#         205, 207, 215, 248, 266, 271, 278, 299, 300, 340 ],
#     [ 36, 113, 165, 192, 232 ],
#     [ 39, 80, 101, 104, 116, 130, 131, 148, 156, 164, 193, 194, 204, 209,
#         217, 218, 224, 265, 289, 308, 329, 330, 343, 345, 349 ],
#     [ 56, 73, 121, 189, 220 ], [ 62, 201, 216, 230, 353 ],
#     [ 124, 140, 253, 263, 357 ], [ 276 ], [ 324 ] ],
# conjsuborbit := [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
#     0, 0 ], issuborbitrecord := true )




######################################
#
# Put compressed matrices here....
#
#####################################


InstallMethod(Iterator,
  "for subspaces of a projective space",
        [IsSubspacesOfProjectiveSpace],
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
			  return VectorSpaceToElement(ps,mat);	         
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



InstallMethod( Size, [IsShadowSubspacesOfProjectiveSpace and
  IsShadowSubspacesOfProjectiveSpaceRep ],
  function( vs )
    return Size(Subspaces(vs!.factorspace,
      vs!.type - Size(vs!.inner)));
  end);


InstallMethod( ShadowOfElement, [IsProjectiveSpace, IsElementOfIncidenceStructure, IsPosInt],
# returns the shadow of an element v as a record containing the projective space (geometry), 
# the type j of the elements (type), the element v (parentflag), and some extra information
# useful to compute with the shadows, e.g. iterator
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
# returns the shadow of a flag as a record containing the projective space (geometry), 
# the type j of the elements (type), the flag (parentflag), and some extra information
# useful to compute with the shadows, e.g. iterator

  function( ps, flag, j )
    local localinner, localouter, localfactorspace, v, smallertypes, biggertypes, ceiling, floor;
    
    #empty flag - return all subspaces of the right type
    if IsEmpty(flag) then
      return ElementsOfIncidenceStructure(ps, j);
    fi;
    
    # find the element in the flag of highest type less than j, and the subspace
    # in the flag of lowest type more than j.
	
	#listoftypes:=List(flag,x->x!.type);
	smallertypes:=Filtered([1..Size(flag)],t->flag[t]!.type<=j);
	biggertypes:=Filtered([1..Size(flag)],t->flag[t]!.type>=j);
	if smallertypes=[] then localinner := [];
		ceiling:=Minimum(biggertypes);
		localouter:=flag[ceiling];
	elif biggertypes=[] then localouter:=BasisVectors(Basis(ps!.vectorspace));
		floor:=Maximum(smallertypes);
		localinner:=flag[floor];
	else
		floor:=Maximum(smallertypes);
		ceiling:=Minimum(biggertypes);
		localinner:=flag[floor];
		localouter:=flag[ceiling];
	fi;
      if not smallertypes = [] then
		if localinner!.type = 1 then
		localinner:=[localinner!.obj];
		else
		localinner:=localinner!.obj;
		fi;
	  fi;
      if not biggertypes = [] then
		if localouter!.type = 1 then
        localouter := [localouter!.obj];
        else
        localouter := localouter!.obj;
        fi;
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
		return VectorSpaceToElement(ps,mat);
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




#############################################################################
# Display methods:
#############################################################################

InstallMethod( ViewObj, [ IsProjectiveSpace and IsProjectiveSpaceRep ],
  function( p )
    Print("ProjectiveSpace(",p!.dimension,", ",Size(p!.basefield),")");
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


InstallMethod( IsIncident,  [IsSubspaceOfProjectiveSpace,
        IsProjectiveSpace],
        # returns true if the subspace is contained in the projective space
        function(x,y)
                return x in y;
        end );

InstallMethod( IsIncident,  [IsProjectiveSpace,
        IsSubspaceOfProjectiveSpace],
        # returns true if the subspace is contained in the projective space
        function(x,y)
                return y in x;
        end );


InstallMethod( IsIncident,  [IsSubspaceOfProjectiveSpace,
## some of this function is based on the
## SemiEchelonMat function. we save time by assuming that the matrix of
## each subspace is already in semiechelon form.
## method only applies to projective and polar spaces
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
      Error( "The subspaces belong to different ambient spaces" );
    fi;
    return false;
  end );

InstallMethod( Span, [IsProjectiveSpace, IsSubspaceOfProjectiveSpace],
        function(x,y)
        if y in x then return x;
	fi;
end );

InstallMethod( Span, [IsSubspaceOfProjectiveSpace, IsProjectiveSpace],
        function(x,y)
        if x in y then return y;
        fi;
end );

#InstallMethod( Span, [IsSubspaceOfProjectiveSpace, IsSubspaceOfProjectiveSpace],
#  function( x, y )  
#    local ux, uy, ambx, amby, typx, typy, span, F;
#    ambx := AmbientSpace(x!.geo);
#    amby := AmbientSpace(y!.geo);
#    typx := x!.type;
#    typy := y!.type;
#    F := ambx!.basefield;

#    if ambx!.vectorspace = amby!.vectorspace then
#      ux := ShallowCopy(x!.obj); 
#      uy := ShallowCopy(y!.obj);
#        
#      if typx = 1 then ux := [ux]; fi;
#      if typy = 1 then uy := [uy]; fi;

#      span := MutableCopyMat(ux);
#      Append(span,uy);
#      span := MutableCopyMat(SemiEchelonMat(span).vectors);
#      # if the span is the whole space, return that.
#      if Length(span) = ambx!.dimension + 1 then
#        return ambx;
#      fi;
#	  return VectorSpaceToElement(ambx,span); 
#    else
#      Error("The subspaces belong to different ambient spaces");
#    fi;
#    return;
#  end );

InstallMethod( Span, [IsSubspaceOfProjectiveSpace,
IsSubspaceOfProjectiveSpace],
 function( x, y )
   ## This method is quicker than the old one
   local ux, uy, typx, typy, span, vec;
   typx := x!.type;
   typy := y!.type;
   vec := x!.geo!.vectorspace;
   if vec = y!.geo!.vectorspace then
     ux := Unwrap(x);
     uy := Unwrap(y);

     if typx = 1 then ux := [ux]; fi;
     if typy = 1 then uy := [uy]; fi;

     span := SumIntersectionMat(ux, uy)[1];

     if Length(span) < vec!.DimensionOfVectors then
        return VectorSpaceToElement( AmbientSpace(x!.geo), span);
     else
        return AmbientSpace(x!.geo);
     fi;
   else
     Error("Subspaces belong to different ambient spaces");
   fi;
 end );



InstallMethod( Span, [ IsHomogeneousList and IsSubspaceOfProjectiveSpaceCollection ],
  function( l )  
    local unwrapped, r, unr, amb, span, temp, x, F, list;
	# first we check that all items in the list belong to the same ambient space
	if not Size(AsDuplicateFreeList(List(l,x->AmbientSpace(x))))=1 then 
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
      
	  return VectorSpaceToElement(amb,span);
	fi;
  end );


InstallMethod( Span, [ IsList ],
  function( l )  
    local pg,list,x;
	# This method is added to allow the list ("l") to contain the projective space 
	# or the empty subspace. If this method is selected, it follows that the list must
	# contain the whole projective space or the empty set. 
	# First we remove the emptysubspace from the list, then we check if the list
	# contains the whold projective space. If it does, return that, if it doesn't
	# return the span of the remaining elements of the list, which will then select
	# the previous method for Span
  if Length(l)=0 then return EmptySubspace;
  elif Length(l)=1 then return l;
  else
	list:=Filtered(l,x->not IsEmptySubspace(x));
	if not Size(AsDuplicateFreeList(List(list,x->AmbientSpace(x))))=1 then 
	 Error("The elements in the list do not have a common ambient space");
	else
	  pg:=AmbientSpace(list[1]);
	  if pg in list then return pg;
	  else
		return Span(list);
	  fi;
	fi;
  fi;
  end );



InstallMethod( Meet, [IsProjectiveSpace, IsSubspaceOfProjectiveSpace],
        function(x,y)
        if y in x then return y;
	fi;
end );

InstallMethod( Meet, [IsSubspaceOfProjectiveSpace, IsProjectiveSpace],
        function(x,y)
        if x in y then return x;
        fi;
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

      if not int=[] then 

          # if one of our varieties is in a polar space, we
          # can say that the meet is in the polar space.
      
          if IsClassicalPolarSpace(x!.geo) then
             return VectorSpaceToElement( x!.geo, int);
          elif IsClassicalPolarSpace(y!.geo) then
             return VectorSpaceToElement( y!.geo, int);
          else
             return VectorSpaceToElement( AmbientSpace(x), int);
          fi;
      else 
          return EmptySubspace;
      fi;
    else
      Error("Subspaces belong to different ambient spaces");
    fi;
    return;
  end );

InstallMethod( Meet, [ IsHomogeneousList and IsSubspaceOfProjectiveSpaceCollection],
 function( l )  
     local int, iter;
	 # first we check if all subspaces have the same ambient geometry
  if not Size(AsDuplicateFreeList(List(l,x->AmbientSpace(x))))=1 then 
    Error("The elements in the list do not have a common ambient space");
  else
      # We use recursion for this routine.
      # Not ideal, but there is no "SumIntersectionMat" for lists

    if not IsEmpty(l) then
	  if Length(l)=1 then return l;
	  else
       iter := Iterator(l);
       int := NextIterator(iter);
       repeat
         int := Meet(int, NextIterator(iter));
       until int = [] or IsDoneIterator(iter);
       return int;
	  fi;
    else return EmptySubspace;
    fi;
  fi;
 end );

InstallMethod( Meet, [ IsList ],
  function( l )  
    local pg,checklist,list,x;
	# This method is added to allow the list ("l") to contain the projective space 
	# or the empty subspace. If this method is selected, it follows that the list must
	# contain the whole projective space or the empty set. 
	if IsEmpty(l) then return EmptySubspace;
	else
	  if Length(l)=1 then return l;
	  else

    	# First we check that the non emptysubspace elements belong to the same ambient space
	  checklist:=Filtered(l,x->not IsEmptySubspace(x) and not IsProjectiveSpace(x));
	  if not Size(AsDuplicateFreeList(List(checklist,x->AmbientSpace(x))))=1 then 
	   Error("The elements in the list do not have a common ambient space");
	  else	
	  # First we remove the whole space from the list, then we check if the list
	  # contains the whold projective space. If it does, return that, if it doesn't
	  # return the span of the remaining elements of the list, which will then select
	  # the previous method for Span
	    if EmptySubspace in l then return EmptySubspace;
	    else
	      pg:=AmbientSpace(checklist[1]); # the first element in l could be the emptysubspace,
	      # so we choose the first element of the checklist
	      list:=Filtered(l,x->not x = pg);
		  return Meet(list);
	    fi;
	  fi;
	fi;
  fi;
end );



#####
##
## Methods for random selection of elements
##
####
  
  
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

InstallMethod( RandomSubspace, "for a projective space",
					[IsProjectiveSpace],
		function(pg)
			local list,i;
			list:=[0..Dimension(pg)-1];
			i:=Random(list);
			return RandomSubspace(pg,i);
		end );
			
		
InstallMethod( Random, "for a collection of subspaces of a projective space",
                       [ IsSubspacesOfProjectiveSpace ],
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
        return EmptySubspace;
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
      return EmptySubspace;
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
    return EmptySubspace;
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
 function( geom, v )
   local  x, n, i;
     ## when v is empty...

     if IsEmpty(v) then
       return EmptySubspace;
     fi;
     x := ShallowCopy(v);

     ## dimension should be correct

     if Length(v) <> geom!.dimension + 1 then
        Error("Dimensions are incompatible");
     fi;

     ## We must also compress our vector.

     ConvertToVectorRep(x, geom!.basefield);
     return Wrap(geom, 1, x);
end );

InstallMethod( VectorSpaceToElement, "for an 8-bit vector",
 [IsProjectiveSpace, Is8BitVectorRep],
 function( geom, v )
   local  x, n, i;
     ## when v is empty...

     if IsEmpty(v) then
       return EmptySubspace;
     fi;
     x := ShallowCopy(v);

     ## dimension should be correct

     if Length(v) <> geom!.dimension + 1 then
        Error("Dimensions are incompatible");
     fi;

     ## We must also compress our vector.

     ConvertToVectorRep(x, geom!.basefield);
     return Wrap(geom, 1, x);
end );


#############################################################################
# Baer sublines and Baer subplanes:
# These objects are particular cases of subgeometries, and should be returned
# as embeddings. To construct subgeometries on a general frame, we should
# use the unique projectivity mapping the standard frame to this frame, and
# then construct the subgeometry as the image of the canonical subgeometry
# (this is the one that contains the standard frame)
#
# The general functions could be: 
# CanonicalSubgeometry(projectivespace,primepower)
# SubgeometryByFrame(projectivespace,primepower,frame)
# The function
# ProjectivityByFrame(frame) (DONE, see "ProjectivityByImageOfStandardFrameNC")
# ProjectivityByTwoFrames(frame1,frame2)
#############################################################################

InstallMethod( BaerSublineOnThreePoints, [IsSubspaceOfProjectiveSpace,
                IsSubspaceOfProjectiveSpace, IsSubspaceOfProjectiveSpace],
# UNCHECKED
 function( x, y, z )
  # returns the Baersubline determined by three collinear points x,y,z
  local geo, gfq2, gfq, t, subline;
  
  geo := AmbientSpace(x);
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
# UNCHECKED
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
