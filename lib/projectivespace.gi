#############################################################################
##
##  projectivespace.gi        FinInG package
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
##  Implementation stuff for projective spaces.
##
#############################################################################

#############################################################################
# Low level help methods:
#############################################################################

# CHECKED 6/09/11 jdb
#############################################################################
#O  Wrap( <geo>, <type>, <o> )
# This is an internal subroutine which is not expected to be used by the user;
# they would be using VectorSpaceToElement. Recall that Wrap is declared in 
# geometry.gd. 
##
InstallMethod( Wrap, 
	"for a projective space and an object",
	[IsProjectiveSpace, IsPosInt, IsObject],
	function( geo, type, o )
		local w;
		w := rec( geo := geo, type := type, obj := o );
		Objectify( NewType( SoPSFamily, IsElementOfIncidenceStructure and
			IsElementOfIncidenceStructureRep and IsSubspaceOfProjectiveSpace ), w );
		return w;
	end );

# CHECKED 6/09/11 jdb
#jdb 30/10/15: This method is not used anymore after commenting out the Unwrapper stuff in geometry.gd
#see the comment there.
#############################################################################
#O  \^( <v>, <u> )
# If the object "v" to be unwrapped is a point of a vector space, then we do not want to use
# return v!.obj, but we want to return a list with one vector, i.e. [v!.obj]
# e.g. if p is a point of a projective space
# gap> p^_; 
# will return a list, with the coordinate vector of the point p
##
#InstallMethod( \^,
#	"for a subspace of a projective space and an unwrapper",
#	[ IsSubspaceOfProjectiveSpace, IsUnwrapper ],
#	function( v, u )
#		if v!.type = 1 then return [v!.obj];
#		else return v!.obj;
#		fi;
#	end );

#############################################################################
# Constructor methods and some operations/attributes for projective spaces.
#############################################################################

# CHECKED 6/09/11 jdb
#############################################################################
#O  ProjectiveSpace( <d>, <f> )
# returns PG(d,f), f a finite field.
##
InstallMethod( ProjectiveSpace, "for a proj dimension and a field",
  [ IsInt, IsField ],
  function( d, f )
    local geo, ty;
    geo := rec( dimension := d, basefield := f, 
                vectorspace := FullRowSpace(f, d+1) );
    if d = 2 then
        ty := NewType( GeometriesFamily,
                  IsProjectiveSpace and IsProjectiveSpaceRep and IsDesarguesianPlane);
    else
        ty := NewType( GeometriesFamily,
                  IsProjectiveSpace and IsProjectiveSpaceRep );
    fi;
    Objectify( ty, geo );
    SetAmbientSpace(geo,geo);
    if d=2 then
        SetOrder(geo,[Size(f),Size(f)]);
    fi;
    return geo;
  end );
  
# CHECKED 6/09/11 jdb
#############################################################################
#O  ProjectiveSpace( <d>, <q> )
# returns PG(d,q). 
##
InstallMethod( ProjectiveSpace, "for a proj dimension and a prime power",
  [ IsInt, IsPosInt ],
  function( d, q )
          return ProjectiveSpace(d, GF(q));
  end );
  
#############################################################################
# Display methods:
#############################################################################

InstallMethod( ViewObj, [ IsProjectiveSpace and IsProjectiveSpaceRep ],
  function( p )
    Print("ProjectiveSpace(",p!.dimension,", ",Size(p!.basefield),")");
  end );

InstallMethod( ViewString, 
	"for a projective space",
	[ IsProjectiveSpace and IsProjectiveSpaceRep ],
	function( p )
		return Concatenation("ProjectiveSpace(",String(p!.dimension),", ",String(Size(p!.basefield)),")");
	end );

InstallMethod( PrintObj, [ IsProjectiveSpace and IsProjectiveSpaceRep ],
  function( p )
          Print("ProjectiveSpace(",p!.dimension,",",p!.basefield,")");
  end );

InstallMethod( Display, [ IsProjectiveSpace and IsProjectiveSpaceRep ],
  function( p )
    Print("ProjectiveSpace(",p!.dimension,",",p!.basefield,")\n");
    #if HasDiagramOfGeometry( p ) then      
    #   Display( DiagramOfGeometry( p ) );
    #fi;
  end );

# CHECKED 6/09/11 jdb
#############################################################################
#O  \=( <pg1>, <pg2> )
##
InstallMethod( \=, 
	"for two projective spaces",
	[IsProjectiveSpace, IsProjectiveSpace],
	function(pg1,pg2);
		return UnderlyingVectorSpace(pg1) = UnderlyingVectorSpace(pg2);
	end );

# CHECKED 8/09/11 jdb
#############################################################################
#O  Rank( <ps> )
# returns the projective dimension of <ps>
##
InstallMethod( Rank, 
	"for a projective space",
	[ IsProjectiveSpace and IsProjectiveSpaceRep ],
	ps -> ps!.dimension
	);

#############################################################################
#O  BaseField( <sub> )
# returns the basefield of an element of a projective space
##
InstallMethod( BaseField, 
	"for an element of a projective space", 
	[IsSubspaceOfProjectiveSpace],
	sub -> AmbientSpace(sub)!.basefield );

# CHECKED 6/09/11 jdb
#############################################################################
#A  StandardFrame( <ps> )
# if the dimension of the projective space is n, then StandardFrame 
# makes a list of points with coordinates 
# (1,0,...0), (0,1,0,...,0), ..., (0,...,0,1) and (1,1,...,1) 
##
InstallMethod( StandardFrame, 
	"for a projective space", 
	[IsProjectiveSpace], 
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

# CHECKED 11/09/11 jdb
#############################################################################
#A  RepresentativesOfElements( <ps> )
# Returns the canonical maximal flag for the projective space <ps>
##
InstallMethod( RepresentativesOfElements, 
	"for a projective space", [IsProjectiveSpace],
	# returns the canonical maximal flag
	function( ps )
		local d, gf, id, elts;  
		d := ProjectiveDimension(ps);
		gf := BaseField(ps);
		id := IdentityMat(d+1,gf);
		elts := List([1..d], i -> VectorSpaceToElement(ps, id{[1..i]}));
		return elts;
	end );

# CHECKED 18/4/2011 jdb
#############################################################################
#O  Hyperplanes( <ps> )
# returns Hyperplanes(ps,ps!.dimension), <ps> a projective space
## 
InstallMethod( Hyperplanes,
	"for a projective space",
	[ IsProjectiveSpace ],
	function( ps )
		return ElementsOfIncidenceStructure(ps, ps!.dimension);
	end);

# CHECKED 11/09/11 jdb
#############################################################################
#A  TypesOfElementsOfIncidenceStructure( <ps> )
# returns the names of the types of the elements of the projective space <ps>
# the is a helper operation.
## 
InstallMethod( TypesOfElementsOfIncidenceStructure, 
	"for a projective space", [IsProjectiveSpace],
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

# CHECKED 11/09/11 jdb
#############################################################################
#A  TypesOfElementsOfIncidenceStructurePlural( <ps> )
# retunrs the plural of the names of the types of the elements of the 
# projective space <ps>. This is a helper operation.
## 
InstallMethod( TypesOfElementsOfIncidenceStructurePlural, 
	"for a projective space",
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

# CHECKED 11/09/11 jdb
#############################################################################
#O  ElementsOfIncidenceStructure( <ps>, <j> )
# returns the elements of the projective space <ps> of type <j>
## 
InstallMethod( ElementsOfIncidenceStructure, 
	"for a projective space and an integer",
	[IsProjectiveSpace, IsPosInt],
	function( ps, j )
		local r;
		r := Rank(ps);
		if j > r then
			Error("<ps> has no elements of type <j>");
		else
			return Objectify(
			NewType( ElementsCollFamily, IsSubspacesOfProjectiveSpace and IsSubspacesOfProjectiveSpaceRep ),
				rec( geometry := ps,
					type := j,
					size := Size(Subspaces(ps!.vectorspace, j))
					)
					);
		fi;
	end);

# CHECKED 11/09/11 jdb
#############################################################################
#O  ElementsOfIncidenceStructure( <ps> )
# returns all the elements of the projective space <ps> 
## 
InstallMethod( ElementsOfIncidenceStructure, 
	"for a projective space",
	[IsProjectiveSpace],
	function( ps )
		return Objectify(
			NewType( ElementsCollFamily, IsAllSubspacesOfProjectiveSpace and IsAllSubspacesOfProjectiveSpaceRep ),
				rec( geometry := ps,
					type := "all") #added this field in analogy with the collection that contains all subspaces of a vector space. 14/9/2011 jdb.
				);
	end);

# CHECKED 14/09/11 jdb
#############################################################################
#O  \=( <x>, <y> )
# returns true if the collections <x> and <y> of all subspaces of a projective
# space are the same.
## 
InstallMethod( \=,
  "for set of all subspaces of a projective space",
  [ IsAllSubspacesOfProjectiveSpace, IsAllSubspacesOfProjectiveSpace ],
  function(x,y)
  return ((x!.geometry!.dimension = y!.geometry!.dimension) and (x!.geometry!.basefield =
  y!.geometry!.basefield));
end );


# CHECKED 11/09/11 jdb
#############################################################################
#O  Size( <subs>) 
# returns the number of elements in the collection <subs>.
##
InstallMethod( Size,
	"for subspaces of a projective space",
	[IsSubspacesOfProjectiveSpace and IsSubspacesOfProjectiveSpaceRep],
	function(subs);
		return ShallowCopy(subs!.size);
	end);

#############################################################################
# Constructor methods and some operations/attributes for subspaces of 
# projective spaces.
#############################################################################

#############################################################################
#  VectorSpaceToElement methods
#############################################################################

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
## Currently we don't load the cvec package. Since 18/3/14 we do. The next 
## method also inserts a method for cmat.

# added 20/3/14
#############################################################################
#O  VectorSpaceToElement( <geom>, <v> ) returns the elements in <geom> determined
# by the vectorspace <v>. Several checks are built in. 
# This method unpacks the cmat, and uses the other VectorSpaceToElement method for PlistRep.
# this is maybe not too efficient, but row selection like x{list} (list is a list of positions)
# seems to fail, although according to the documentation, it should work. 
# In the future this could be made better.
##
InstallMethod( VectorSpaceToElement,
	"for a projective space and a CMatRep",
	[IsProjectiveSpace, IsCMatRep],
	function( geom, v )
	return VectorSpaceToElement(geom, Unpack(v));
	end );

# CHECKED 20/09/11
# changed 19/01/16 (jdb): by a change of IsPlistRep, this method gets also
# called when using a row vector, causing a problem with TriangulizeMat.
# a solution was to add IsMatrix.
#############################################################################
#O  VectorSpaceToElement( <geom>, <v> ) returns the elements in <geom> determined
# by the vectorspace <v>. Several checks are built in. 
##
InstallMethod( VectorSpaceToElement, 
	"for a projective space and a Plist",
	[IsProjectiveSpace, IsPlistRep and IsMatrix],
	function( geom, v )
		local  x, n, i, y; 
		## when v is empty... 
        if IsEmpty(v) then
			Error("<v> does not represent any element");
		fi; 		
		#x := EchelonMat(v).vectors;
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
		if Length(x)=ProjectiveDimension(geom)+1 then
			return geom;
		fi;

		## It is possible that (a) the user has entered a
		## matrix with one row, or that (b) the user has
		## entered a matrix with rank 1 (thus at this stage
		## we will have a matrix with one row).
        ## We must also compress our vector/matrix.
		y := NewMatrix(IsCMatRep,geom!.basefield,Length(x[1]),x);
		#NewMatrix is currently undocumented in cvec, but creates a CMat object from a list of lists, using CMat which uses then a list of cvec vectors.
		if Length(y) = 1 then
			return Wrap(geom, 1, y[1]);
			#x := x[1];
			#ConvertToVectorRep(x, geom!.basefield); # the extra basefield is necessary.
			#return Wrap(geom, 1, x);
			#return Wrap(geom, 1, CVec(Unpack(x), geom!.basefield) ); # changed to cvec 18/3/2014.
		else
			#ConvertToMatrixRep(x, geom!.basefield);
			#return Wrap(geom, Length(x), x);
			return Wrap(geom, Length(y), y);
		fi;
	end );

# CHECKED 20/09/11
#############################################################################
#O  VectorSpaceToElement( <geom>, <v> ) returns the elements in <geom> determined
# by the vectorspace <v>. Several checks are built in. 
##
InstallMethod( VectorSpaceToElement, 
	"for a projective space and a compressed GF(2)-matrix",
	[IsProjectiveSpace, IsGF2MatrixRep],
	function( geom, v )
		local  x, n, i, y;
		## when v is empty... 
		if IsEmpty(v) then
			Error("<v> does not represent any element");
		fi;
		x := MutableCopyMat(v);
		TriangulizeMat(x); 
		#x := EchelonMat(v).vectors;
		## dimension should be correct
		if Length(v[1]) <> geom!.dimension + 1 then
			Error("Dimensions are incompatible");
		fi;
		#if Length(x) = 0 then
		#	return EmptySubspace(geom);
		#fi;
		#if Length(x)=ProjectiveDimension(geom)+1 then
		#	return geom;
		#fi;
		
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
		if Length(x)=ProjectiveDimension(geom)+1 then
			return geom;
		fi;

		
		## It is possible that (a) the user has entered a
		## matrix with one row, or that (b) the user has
		## entered a matrix with rank 1 (thus at this stage
		## we will have a matrix with one row).
	    ## We must also compress our vector/matrix.
		y := NewMatrix(IsCMatRep,geom!.basefield,Length(x[1]),x);
		if Length(y) = 1 then
			return Wrap(geom, 1, y[1]);
			#x := x[1];
			#ConvertToVectorRep(x, geom!.basefield); # the extra basefield is necessary.
			#return Wrap(geom, 1, x);
			#return Wrap(geom, 1, CVec(Unpack(x), geom!.basefield) ); # changed to cvec 18/3/2014.
		else
			#ConvertToMatrixRep(x, geom!.basefield);
			return Wrap(geom, Length(y), y);
		fi;
	end );
  
# CHECKED 20/09/11
#############################################################################
#O  VectorSpaceToElement( <geom>, <v> ) returns the elements in <geom> determined
# by the vectorspace <v>. Several checks are built in. 
##
InstallMethod( VectorSpaceToElement, 
	"for a compressed basis of a vector subspace",
	[IsProjectiveSpace, Is8BitMatrixRep],
	function( geom, v )
		local  x, n, i, y;
		## when v is empty... 
		if IsEmpty(v) then
			Error("<v> does not represent any element");
		fi;
		#x := EchelonMat(v).vectors;
		x := MutableCopyMat(v);
		TriangulizeMat(x); 
		
		## dimension should be correct
		if Length(v[1]) <> geom!.dimension + 1 then
			Error("Dimensions are incompatible");
		fi;	
		
		#if Length(x) = 0 then
		#	return EmptySubspace(geom);
		#fi;
		#if Length(x)=ProjectiveDimension(geom)+1 then
		#	return geom;
		#fi;
		
		n := Length(x);
		i := 0;
		while i < n and ForAll(x[n-i], IsZero) do
			i := i+1; 
		od;
		if i = n then
			return EmptySubspace(geom);
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
		y := NewMatrix(IsCMatRep,geom!.basefield,Length(x[1]),x);
		if Length(y) = 1 then
			return Wrap(geom, 1, y[1]);
			#x := x[1];
			#ConvertToVectorRep(x, geom!.basefield); # the extra basefield is necessary.
			#return Wrap(geom, 1, x);
			#return Wrap(geom, 1, CVec(Unpack(x), geom!.basefield) ); # changed to cvec 18/3/2014.
		else
			#ConvertToMatrixRep(x, geom!.basefield);
			return Wrap(geom, Length(y), y);
		fi;
		end );
  
### The next mathod constructs an element using a cvec. 

# ADDED 20/3/2014 jdb
#############################################################################
#O  VectorSpaceToElement( <geom>, <v> ) returns the elements in <geom> determined
# by the rowvector <v>. Several checks are built in.
##
InstallMethod( VectorSpaceToElement,
	"for a row vector",
	[IsProjectiveSpace, IsCVecRep],
	function( geom, v )
		local  x, y;
		## when v is empty... does this ever occur for a row vector? No. jdb 21/09/2011
		#if IsEmpty(v) then
		#	Error("<v> does not represent any element");
		#fi;
		x := ShallowCopy(v);
		## dimension should be correct
		if Length(v) <> geom!.dimension + 1 then
			Error("Dimensions are incompatible");
		fi;
		## We must also compress our vector.
		#ConvertToVectorRep(x, geom!.basefield);
		## bad characters, such as jdb, checked this with input zero vector...
		if IsZero(x) then
			return EmptySubspace(geom);
		else
			MultVector(x,Inverse( x[PositionNonZero(x)] ));
			return Wrap(geom, 1, x);
		fi;
	end );

# CHECKED 11/04/15 jdb
# CHANGED 19/9/2011 jdb + ml
# CHECKED 21/09/2011 jdb
#############################################################################
#O  VectorSpaceToElement( <geom>, <v> ) returns the elements in <geom> determined
# by the rowvector <v>. Several checks are built in.
##
InstallMethod( VectorSpaceToElement,
	"for a row vector",
	[IsProjectiveSpace, IsRowVector],
	function( geom, v )
		local  x, y;
		## when v is empty... does this ever occur for a row vector? No. jdb 21/09/2011
		#if IsEmpty(v) then
		#	Error("<v> does not represent any element");
		#fi;
		x := ShallowCopy(v);
		## dimension should be correct
		if Length(v) <> geom!.dimension + 1 then
			Error("Dimensions are incompatible");
		fi;
		## We must also compress our vector.
		#ConvertToVectorRep(x, geom!.basefield);
		## bad characters, such as jdb, checked this with input zero vector...
		if IsZero(x) then
			return EmptySubspace(geom);
		else
			MultVector(x,Inverse( x[PositionNonZero(x)] ));
			y := NewMatrix(IsCMatRep,geom!.basefield,Length(x),[x]);
			#ConvertToVectorRep(x, geom!.basefield);
			return Wrap(geom, 1, y[1]);
		fi;
	end );

# CHECKED 11/04/15 jdb
# CHANGED 19/9/2011 jdb + ml
#############################################################################
#O  VectorSpaceToElement( <geom>, <v> ) returns the elements in <geom> determined
# by the rowvector <v>. Several checks are built in.
##
InstallMethod( VectorSpaceToElement, 
	"for a projective space and an 8-bit vector",
	[IsProjectiveSpace, Is8BitVectorRep],
	function( geom, v )
		local  x, n, i, y;
		## when v is empty...
		if IsEmpty(v) then
			return EmptySubspace(geom);
		fi;
		x := ShallowCopy(v);
		## dimension should be correct
		if Length(v) <> geom!.dimension + 1 then
			Error("Dimensions are incompatible");
		fi;
		## We must also compress our vector.
		#ConvertToVectorRep(x, geom!.basefield);
		## bad characters, such as jdb, checked this with input zero vector...
		if IsZero(x) then
			return EmptySubspace(geom);
		else
			MultVector(x,Inverse( x[PositionNonZero(x)] ));
			y := NewMatrix(IsCMatRep,geom!.basefield,Length(x),[x]);
			#ConvertToVectorRep(x, geom!.basefield);
			return Wrap(geom, 1, y[1]);
		fi;
	end );

#############################################################################
#  attributes/operations for subspaces
#############################################################################

# CHECKED 14/09/11 jdb
#############################################################################
#O  UnderlyingVectorSpace( <subspace> ) returns the underlying vectorspace of
# <subspace>, i.e. the vectorspace determining <subspace>
##
InstallMethod( UnderlyingVectorSpace, 
	"for a subspace of a projective space",
	[IsSubspaceOfProjectiveSpace],
	function(subspace)
		local vspace,W;
		vspace:=UnderlyingVectorSpace(subspace!.geo);
		if subspace!.type = 1 then
			W:=SubspaceNC(vspace,[Unpack(subspace!.obj)]); #possibly unpack here to avoid bloody seg fault.
		else
			W:=SubspaceNC(vspace,Unpack(subspace!.obj));
		fi;
		return W;
	end);

# CHECKED 8/09/11 jdb
#############################################################################
#O  ProjectiveDimension( <v> ) returns the projective dimension of <v>
##
InstallMethod( ProjectiveDimension, 
	"for a subspace of a projective space",
	[ IsSubspaceOfProjectiveSpace ],
	function( v )
		return v!.type - 1;
	end );

#InstallMethod( ProjectiveDimension, [ IsEmpty ], function(x) return -1;end );

# CHECKED 8/09/11 jdb
#############################################################################
#O  Dimension( <v> ) returns the projective dimension of <v>
##
InstallMethod( Dimension, 
	"for a subspace of a projective space",
    [ IsSubspaceOfProjectiveSpace ],
	function( v )
		return v!.type - 1;
	end );

#InstallMethod( Dimension, [ IsEmpty ], function(x) return -1;end );

# CHECKED 8/09/11 jdb
# 31/5/2020: still ok, but undocumentend!
#############################################################################
#O  StandardFrame( <subspace> ) returns a standard frame for <subspace>
##
InstallMethod( StandardFrame, 
	"for a subspace of a projective space", 
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

# CHECKED 8/09/11 jdb
#############################################################################
#O  Coordinates( <point> ) returns the coordinates of the projective point <point>
##
InstallMethod( Coordinates, 
	"for a point of a projective space",
	[IsSubspaceOfProjectiveSpace],
	function( point )
		if not Dimension(point)=0 then 
			Error("The argument is not a projective point");
		else 
			return ShallowCopy(Unpack(point!.obj));
		fi;
	end );
  
# obsolete
#############################################################################
#O  CoordinatesOfHyperplane( <hyp> ) returns the coordinates of the hyperplane
# <hyp>
##
#InstallMethod( CoordinatesOfHyperplane, 
#	"for a hyperplane of a projective space",
#	[IsSubspaceOfProjectiveSpace],
#	function(hyp)
#		local pg;
#		pg:=ShallowCopy(hyp!.geo);
#		if not hyp!.type=Dimension(pg) then 
#			Error("The argument is not a hyperplane");
#		else 
#			#perp:=StandardDualityOfProjectiveSpace(pg);
#			return Coordinates(VectorSpaceToElement(pg,NullspaceMat(TransposedMat(hyp!.obj))));
#		fi;
#	end );

# came from varieties.gi
#############################################################################
#O  DualCoordinatesOfHyperplane( <hyp> )
# returns the dual coordinate of a hyperplane in a projective space.
##
InstallMethod( DualCoordinatesOfHyperplane,
	"for a subspace of a projective space",
		[IsSubspaceOfProjectiveSpace],
		function(hyp)
			local mat,a,x;
			if not Dimension(hyp)=Dimension(hyp!.geo)-1 then
				Error("The argument is not a hyperplane");
			else
				mat:=hyp!.obj;
				a:=NullspaceMat(TransposedMat(mat));
				x := Unpack(a[1]);
                MultVector(x,Inverse( x[PositionNonZero(x)] ));
                return x;
			fi;
	end );

# came from varieties.gi
#############################################################################
#O  HyperplaneByDualCoordinates( <pg>,<vector> )
# returns the hyperplanes by given dual coordinates.
##
InstallMethod( HyperplaneByDualCoordinates,
	"for a projective space and a list with coordinates",
	[IsProjectiveSpace,IsList],
	function(pg,a)
		local mat,list;
		if not Size(a)=Dimension(pg)+1 or not ForAll(a,x->x in pg!.basefield) then
			Error("The dual coordinates are not compatible with the projective space");
		else
			mat:=[a];
			list:=NullspaceMat(TransposedMat(mat));
			return VectorSpaceToElement(pg,list);
		fi;
	end );

# CHECKED 8/09/11 jdb
#############################################################################
#O  EquationOfHyperplane( <hyp> ) returns the euqation of the hyperplane
# <hyp>
##
InstallMethod( EquationOfHyperplane, 
	"for a hyperplane of a projective space",
	[IsSubspaceOfProjectiveSpace],
	function(hyp)
		local pg,r,v,indets;
		pg:=AmbientGeometry(hyp);
		r:=PolynomialRing(pg!.basefield,pg!.dimension + 1);
		indets:=IndeterminatesOfPolynomialRing(r);
		v:=DualCoordinatesOfHyperplane(hyp);
		return Sum(List([1..Size(indets)],i->v[i]*indets[i]));
	end );
	
# CHECKED 11/09/11 jdb
# Commented out 28/11/11 jdb + pc, according to new regulations
#############################################################################
#O  AmbientSpace( <subspace> ) returns the ambient space of <subspace>
##
#InstallMethod( AmbientSpace, [IsSubspaceOfProjectiveSpace],
#	function(subspace)
#		return subspace!.geo;
#	end );

#############################################################################
#  Span/Meet for empty subspaces
#############################################################################

# CHECKED 8/09/11 jdb
#############################################################################
#O  Span( <x>, <y> ) returns the span of <x> and <y> 
##
InstallMethod( Span, 
	"for the empty subspace and a projective space", 
	[ IsEmptySubspace, IsProjectiveSpace ],
	function( x, y )
		if x!.geo!.vectorspace = y!.vectorspace then
			return y;
		else
			Error( "The subspace <x> has a different ambient space than <y>" );
		fi;
	end );

# CHECKED 8/09/11 jdb
#############################################################################
#O  Span( <x>, <y> ) returns the span of <x> and <y> 
##
InstallMethod( Span, 
	"for the empty subspace and a projective space", 
	[ IsProjectiveSpace, IsEmptySubspace ],
	function( x, y )
		if x!.vectorspace = y!.geo!.vectorspace then
			return x;
		else
			Error( "The subspace <x> has a different ambient space than <y>" );
		fi;
	end );

# CHECKED 8/09/11 jdb
#############################################################################
#O  Meet( <x>, <y> ) returns the intersection of <x> and <y> 
##
InstallMethod( Meet, 
	"for the empty subspace and a projective subspace", 
	[ IsEmptySubspace, IsProjectiveSpace ],
	function( x, y )
		if x!.geo!.vectorspace = y!.vectorspace then
			return x;
		else
			Error( "The subspace <x> has a different ambient space than <y>" );
		fi;
	end );

# CHECKED 8/09/11 jdb
#############################################################################
#O  Meet( <x>, <y> ) returns the intersection of <x> and <y> 
##
InstallMethod( Meet, 
	"for the empty subspace and a projective subspace", 
	[ IsProjectiveSpace, IsEmptySubspace ],
	function( x, y )
		if x!.vectorspace = y!.geo!.vectorspace then
			return y;
		else
			Error( "The subspace <x> has a different ambient space than <y>" );
		fi;
	end );

# CHECKED 8/09/11 jdb
# 25/3/14 It turns out that there is a problem when we do not Unpack 
# cvec/cmat with Size(Subspaces(localfactorspace)). As there is never an action
# on shadow of flag objects, it is not unreasonable to store the matrices as
# GAP matrices rather than cvec/cmats.
#############################################################################
#O ShadowOfElement(<ps>, <v>, <j> ). Recall that for every particular Lie 
# geometry a method for ShadowOfElement  must be installed. 
##
InstallMethod( ShadowOfElement, 
	"for a projective space, an element, and an integer",
	[IsProjectiveSpace, IsSubspaceOfProjectiveSpace, IsPosInt],
	# returns the shadow of an element v as a record containing the projective space (geometry), 
	# the type j of the elements (type), the element v (parentflag), and some extra information
	# useful to compute with the shadows, e.g. iterator
	function( ps, v, j )
		local localinner, localouter, localfactorspace;
        if not AmbientSpace(v) = ps then
            Error("<v> is not a subspace of <ps>");
        fi;
        if j > ps!.dimension then
            Error("<ps> has no elements of type <j>");
        elif j < v!.type then
			localinner := [];
			localouter := Unpack(v!.obj);
		elif j = v!.type then
			localinner := Unpack(v!.obj);
			localouter := localinner;
		else
			localinner := Unpack(v!.obj);
			localouter := BasisVectors(Basis(ps!.vectorspace));
		fi;
    	if IsVector(localinner) and not IsMatrix(localinner) then
			localinner := [localinner]; 
		fi;
		if IsVector(localouter) and not IsMatrix(localouter) then
			localouter := [localouter]; 
		fi;
		localfactorspace := Subspace(ps!.vectorspace,
		BaseSteinitzVectors(localouter, localinner).factorspace);
		return Objectify( NewType( ElementsCollFamily, IsElementsOfIncidenceStructure and
							IsShadowSubspacesOfProjectiveSpace and
							IsShadowSubspacesOfProjectiveSpaceRep),
							rec( geometry := ps,
									type := j,
									inner := localinner,
									outer := localouter,
									factorspace := localfactorspace,
									parentflag := FlagOfIncidenceStructure(ps,[v]),
									size := Size(Subspaces(localfactorspace))
								)
						);
	end);

# CHECKED 11/09/11 jdb
#############################################################################
#O  Size( <vs>) 
# returns the number of elements in the shadow collection <vs>.
##
InstallMethod( Size, 
	"for shadow subspaces of a projective space",
	[IsShadowSubspacesOfProjectiveSpace and IsShadowSubspacesOfProjectiveSpaceRep ],
	function( vs )
		return Size(Subspaces(vs!.factorspace,
		vs!.type - Size(vs!.inner)));
	end);


#############################################################################
# Constructors for groups of projective spaces.
#############################################################################

# CHECKED 10/09/2011 jdb # changed ml 02/11/12
#############################################################################
#A  CollineationGroup( <ps> )
# returns the collineation group of the projective space <ps>
##
InstallMethod( CollineationGroup, 
	"for a full projective space",
	[ IsProjectiveSpace and IsProjectiveSpaceRep ],
	function( ps )
		local coll,d,f,frob,g,newgens,q,s,pow;
		f := ps!.basefield;
		q := Size(f);
		d := ProjectiveDimension(ps);
		if d <= -1 then 
			Error("The dimension of the projective spaces needs to be at least 0");
		fi;
		g := GL(d+1,f);
		frob := FrobeniusAutomorphism(f);
		newgens := List(GeneratorsOfGroup(g),x->[x,frob^0]);
		if not IsOne(frob) then
			Add(newgens,[One(g),frob]); #somehow we forgot that this is trivial if IsOne(frob)
		fi; 
		newgens := ProjElsWithFrob(newgens);
		coll := GroupWithGenerators(newgens);
		pow := LogInt(q, Characteristic(f));
		s := pow * q^(d*(d+1)/2)*Product(List([2..d+1], i->q^i-1)); 
		if pow > 1 then 
			SetName( coll, Concatenation("The FinInG collineation group PGammaL(",String(d+1),",",String(q),")") );
		else
			SetName( coll, Concatenation("The FinInG collineation group PGL(",String(d+1),",",String(q),")") );
			# Remark that in the prime case, PGL is returned as a FinInG collineation group with associated automorphism F^0.
		fi;	
		SetSize( coll, s );
        # only for making generalised polygons section more generic:
        if d = 2 then
            SetCollineationAction(coll,OnProjSubspaces);
        fi;
		return coll;
	end );

# CHECKED 10/09/2011 jdb, changed ML 02/11/2012, changed JDB 05/11/2012
#############################################################################
#A  ProjectivityGroup( <ps> )
# returns the group of projectivities of the projective space <ps>
##
InstallMethod( ProjectivityGroup, 
	"for a projective space",
	[ IsProjectiveSpace ],
	function( ps )
		local gg,d,f,frob,g,newgens,q,s;
		f := ps!.basefield;
		q := Size(f);
		d := ProjectiveDimension(ps);
		if d <= -1 then 
			Error("The dimension of the projective spaces needs to be at least 0");
		fi;
		g := GL(d+1,f);
		frob := FrobeniusAutomorphism(f); #needs this, jdb 05/11/2012, see two lines further.
		#newgens:=GeneratorsOfGroup(g); # this replaces the next line (ml 02/11/12)
		newgens := List(GeneratorsOfGroup(g),x->[x,frob^0]); # I changed back, and uncommented previous line (jdb 05/11/12)
		#newgens := ProjEls(newgens); # this replaces the next line (ml 02/11/12)
		newgens := ProjElsWithFrob(newgens);  # I changed back, and uncommented previous line (jdb 05/11/12)
		gg := GroupWithGenerators(newgens);
		s := q^(d*(d+1)/2)*Product(List([2..d+1], i->q^i-1)); 
		SetName( gg, Concatenation("The FinInG projectivity group PGL(",String(d+1),",",String(q),")") );
		SetSize( gg, s );
		return gg;
	end );

# CHECKED 10/09/2011 jdb, changed ML 02/11/2012, changed JDB 05/11/2012
#############################################################################
#A  SpecialProjectivityGroup( <ps> )
# returns the special projectivity group of the projective space <ps>
##
InstallMethod( SpecialProjectivityGroup, 
	"for a full projective space",
	[ IsProjectiveSpace ],
	function( ps )
		local gg,d,f,frob,g,newgens,q,s;
		f := ps!.basefield;
		q := Size(f);
		d := ProjectiveDimension(ps);
		if d <= -1 then 
			Error("The dimension of the projective spaces needs to be at least 0");
		fi;
		g := SL(d+1,q);
		frob := FrobeniusAutomorphism(f); #needs this, jdb 05/11/2012, see two lines further.
		#newgens:=GeneratorsOfGroup(g);  # this replaces the next line (ml 02/11/12)
		newgens := List(GeneratorsOfGroup(g),x->[x,frob^0]); # I changed back, and uncommented previous line (jdb 05/11/12)
		#newgens:=ProjEls(newgens);  # this replaces the next line (ml 02/11/12)
		newgens := ProjElsWithFrob(newgens); # I changed back, and uncommented previous line (jdb 05/11/12)
		gg := GroupWithGenerators(newgens);
		s := q^(d*(d+1)/2)*Product(List([2..d+1], i->q^i-1)) / GCD_INT(q-1, d+1);
		SetName( gg, Concatenation("The FinInG PSL group PSL(",String(d+1),",",String(q),")") );
		SetSize( gg, s );    
		return gg;
	end );

#############################################################################
# Action functions intended for the user.
#############################################################################

# CHECKED 10/09/2011 jdb, to be reconsidered. See to do in beginning of file.
# CHANGED 19/09/2011 jdb + ml
#############################################################################
#F  OnProjSubspaces( <var>, <el> )
# computes <var>^<el>, where <var> is an element of a projective space, and 
# <el> a projective semilinear element. Important: we are allowed to use Wrap
# rather than VectorSpaceToElement, since the OnProjSubspacesWithFrob and OnProjPointsWithFrob
# deal with making the representation of <var>^<el> canonical.
##
InstallGlobalFunction( OnProjSubspaces,
  function( var, el )
    local amb,geo,newvar,newel;
    geo := var!.geo;   
    if var!.type = 1 then
        newvar := OnProjPointsWithFrob(var!.obj,el);
    else
        newvar := OnProjSubspacesWithFrob(var!.obj,el);
    fi;
    newel := Wrap(AmbientSpace(geo),var!.type,newvar);
    if newel in geo then
        return Wrap(geo,var!.type,newvar);
    else
        return newel;
    fi;
  end );

# CHECKED, but I am unhappy with the too general filter IsElementOfIncidenceStructure
# I left it, but will reconsider it when dealing with polar spaces.
# 11/09/11 jdb
#############################################################################
#O  /^( <x>, <em> )
# computes <var>^<el>, where <var> is an element of a incidence structure, and 
# <em> a projective semilinear element.
##
InstallOtherMethod( \^, 
	"for an element of an incidence structure and a projective semilinear element",
	[IsElementOfIncidenceStructure, IsProjGrpElWithFrob],
	function(x, em)
		return OnProjSubspaces(x,em);
	end );
	
#############################################################################
#O  /^( <x>, <em> )
# computes <var>^<el>, where <var> is an element of a incidence structure, and 
# <em> a projective semilinear with projective space isomorhpism element.
##
InstallOtherMethod( \^, 
	"for an element of an incidence structure and a projective semilinear element",
	[IsElementOfIncidenceStructure, IsProjGrpElWithFrobWithPSIsom],
	function(x, em)
		return OnProjSubspacesExtended(x,em);
	end );

# CHECKED 11/09/11 jdb
#############################################################################
#F  OnSetsProjSubspaces( <var>, el )
# computes <x>^<el>, for all x in <var> 
##
InstallGlobalFunction( OnSetsProjSubspaces,
  function( var, el )
    return Set( var, i -> OnProjSubspaces( i, el ) );
  end );

#############################################################################
# Iterator and Enumerating
#############################################################################

# CHECKED 11/09/11 jdb
# cvec change: only necessary to convert the output of MakeAllProjectivePoints. (19/3/14).
# This is done now (ml 31/03/14)
# 15/2/2016: jdb: changed return o; -> return AsList(o).
#############################################################################
#O  AsList( <vs>) 
# returns a list of all elements in <vs>, which is a collection of subspaces of
# a projective space of a given type. This methods uses functionality from the 
# orb package.
##
InstallMethod( AsList, 
	"for subspaces of a projective space",
	[IsSubspacesOfProjectiveSpace],
	function( vs )
	## We use the package "orb" by Mueller, Neunhoeffer and Noeske,
	## which is much quicker than using an iterator to get all of the
	## projective subspaces of a certain dimension.
	local geo, g, p, o, type, sz, bf, d;
	geo := vs!.geometry;
	g := ProjectivityGroup(geo);
	type := vs!.type; 
	sz := Size(vs);
	if type = 1 then
		bf := geo!.basefield;
		d := geo!.dimension;
		o := MakeAllProjectivePoints(bf, d);
		o := List(o, t -> Wrap(geo, type, t ) );;  
		# o := List(o, t -> Wrap(geo, type, CVec(t, bf) ) );;   
	else
		p := NextIterator(Iterator(vs));
		o := Orb(g, p, OnProjSubspaces, rec( hashlen:=Int(5/4*sz), 
                                          orbsizebound := sz ));
		Enumerate(o, sz);
	fi;
    #return o; #see also AsList in polarspace.gi for explanation.
    return AsList(o);
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
# If you mean with "here" the Iterator, then no worries since it uses VectorSpaceToElement, which uses now cvec/cmat.
#
#####################################

# CHECKED 11/09/11 jdb
#############################################################################
#O  Iterator( <vs>) 
# returns an iterator for <vs>, a collection of subspaces of a projective space.
##
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

#############################################################################
# Methods to create flags.
#############################################################################

# CHECKED 16/12/2014 (added check that all elements belong to ps) jdb
#############################################################################
#O  FlagOfIncidenceStructure( <ps>, <els> )
# returns the flag of the projective space <ps> with elements in <els>.
# the method checks whether the input really determines a flag.
# The use of ambient space in the first test makes sure that we can also
# use subspaces of polar spaces, as long as their ambient projective space
# equals ps.
##
InstallMethod( FlagOfIncidenceStructure,
	"for a projective space and list of subspaces of the projective space",
	[ IsProjectiveSpace, IsSubspaceOfProjectiveSpaceCollection ],
	function(ps,els)
		local list,i,test,type,flag;
		list := Set(ShallowCopy(els));
		if Length(list) > Rank(ps) then
		  Error("A flag can contain at most Rank(<ps>) elements");
		fi;
        test := List(list,x->AmbientSpace(x));
        if not ForAll(test,x->x=ps) then
            Error("not all elements have <ps> as ambient space");
        fi;
        #if test[1] <> ps then
        #    Error("<els> is not a list of elements with ambient projective space <ps>");
        #fi;
        test := Set(List([1..Length(list)-1],i -> IsIncident(list[i],list[i+1])));
		if (test <> [ true ] and test <> []) then
		  Error("<els> do not determine a flag");
		fi;
		flag := rec(geo := ps, types := List(list,x->x!.type), els := list, vectorspace := ps!.vectorspace );
		ObjectifyWithAttributes(flag, IsFlagOfPSType, IsEmptyFlag, false, RankAttr, Size(list) );
		return flag;
	end);

# CHECKED 18/4/2011 jdb
#############################################################################
#O  FlagOfIncidenceStructure( <ps>, <els> )
# returns the empty flag of the projective space <ps>.
##
InstallMethod( FlagOfIncidenceStructure,
	"for a projective space and an empty list",
	[ IsProjectiveSpace, IsList and IsEmpty ],
	function(ps,els)
		local flag;
		flag := rec(geo := ps, types := [], els := [], vectorspace := ps!.vectorspace );
		ObjectifyWithAttributes(flag, IsFlagOfPSType, IsEmptyFlag, true, RankAttr, 0 );
		return flag;
	end);


# ADDED 26/3/2014 jdb
#############################################################################
#O  UnderlyingVectorSpace( <flag> )
# returns the UnderlyingVectorSpace of <flag>
##
InstallMethod( UnderlyingVectorSpace,
	"for a flag of a projective space",
	[ IsFlagOfProjectiveSpace and IsFlagOfIncidenceStructureRep ],
	function(flag)
		return flag!.vectorspace;
	end);


#############################################################################
# View/Print/Display methods for flags
#############################################################################

#seems not necessary thanks to general method in geometry.gi 
#InstallMethod( ViewObj, "for a flag of a projective space",
#	[ IsFlagOfProjectiveSpace and IsFlagOfIncidenceStructureRep ],
#	function( flag )
#		Print("<a flag of ProjectiveSpace(",flag!.geo!.dimension,", ",Size(flag!.geo!.basefield),")>");
#	end );

InstallMethod( PrintObj, "for a flag of a projective space",
	[ IsFlagOfProjectiveSpace and IsFlagOfIncidenceStructureRep ],
	function( flag )
		PrintObj(flag!.els);
	end );

#sligthly adapted: make sure to use ViewString for the projective space, which could also be e.g. a subgeometry.
#by using ViewString, this method becomes applicable for all projective spaces, including spaces like subgeometries.
InstallMethod( Display, "for a flag of a projective space",
	[ IsFlagOfProjectiveSpace and IsFlagOfIncidenceStructureRep  ],
	function( flag )
		if IsEmptyFlag(flag) then
            Print(Concatenation("<empty flag of ",ViewString(flag!.geo)," >\n"));
        else
			Print(Concatenation("<a flag of ",ViewString(flag!.geo)," )> with elements of types ",String(flag!.types),"\n" ));
			Print("respectively spanned by\n");
			Display(flag!.els);
		fi;
	end );

# CHECKED 18/4/2011 jdb
# 25/3/14 It turns out that there is a problem when we do not Unpack 
# cvec/cmat with Size(Subspaces(localfactorspace)). As there is never an action
# on shadow of flag objects, it is not unreasonable to store the matrices as
# GAP matrices rather than cvec/cmats.
#############################################################################
#O  ShadowOfFlag( <ps>, <flag>, <j> )
# returns the shadow elements of <flag>, i.e. the elements of <ps> of type <j> 
# incident with all elements of <flag>.
# returns the shadow of a flag as a record containing the projective space (geometry), 
# the type j of the elements (type), the flag (parentflag), and some extra information
# useful to compute with the shadows, e.g. iterator
##
InstallMethod( ShadowOfFlag, 
	"for a projective space, a flag and an integer",
	[IsProjectiveSpace, IsFlagOfProjectiveSpace, IsPosInt],
	function( ps, flag, j )
    local localinner, localouter, localfactorspace, v, smallertypes, biggertypes, ceiling, floor;
    if j > ps!.dimension then
        Error("<ps> has no elements of type <j>");
    fi;
    #empty flag - return all subspaces of the right type
    if IsEmptyFlag(flag) then
      return ElementsOfIncidenceStructure(ps, j);
    fi;
    
    # find the element in the flag of highest type less than j, and the subspace
    # in the flag of lowest type more than j.
	
	#listoftypes:=List(flag,x->x!.type);
	smallertypes:=Filtered(flag!.types,t->t <= j);
	biggertypes:=Filtered(flag!.types,t->t >= j);
	if smallertypes=[] then 
		localinner := [];
		ceiling:=Minimum(biggertypes);
		localouter:=flag!.els[Position(flag!.types,ceiling)];
	elif biggertypes=[] then 
		localouter:=BasisVectors(Basis(ps!.vectorspace));
		floor:=Maximum(smallertypes);
		localinner:=flag!.els[Position(flag!.types,floor)];
	else
		floor:=Maximum(smallertypes);
		ceiling:=Minimum(biggertypes);
		localinner:=flag!.els[Position(flag!.types,floor)];
		localouter:=flag!.els[Position(flag!.types,ceiling)];
	fi;
	if not smallertypes = [] then
		if localinner!.type = 1 then
			localinner:=[Unpack(localinner!.obj)]; #here is the cmat change
		else
			localinner:=Unpack(localinner!.obj);
		fi;
	fi;
    if not biggertypes = [] then
		if localouter!.type = 1 then
			localouter := [Unpack(localouter!.obj)];
        else
			localouter := Unpack(localouter!.obj);
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
          size := Size(Subspaces(localfactorspace)) #this is wrong!!! But since we do some work again in the iterator, it's never used!
        )
      );
	end);

# CHECKED 11/09/11 jdb
#############################################################################
#O  Iterator( <vs>) 
# returns an iterator for <vs>, a collection of shadowsubspaces of a projective space.
##
InstallMethod( Iterator, 
	"for shadows subspaces of a projective space",
	[IsShadowSubspacesOfProjectiveSpace and IsShadowSubspacesOfProjectiveSpaceRep ],
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
# Methods for incidence.
# Recall that: - we have a generic method to check set theoretic containment
#                for two *elements* of a Lie geometry, and empty subspaces.
#              - IsIncident is symmetrized set theoretic containment
#              - we can extend the \in method (if desired) for the particular
#                Lie geometry, such that we can get true if we ask if an 
#                element is contained in the complete space, or if we consider
#                the whole space and the empty subspce.
#              - \* is a different notation for IsIncident, declared and
#                implement in geometry.g* 
#############################################################################

# CHECKED 7/09/11 jdb
#############################################################################
#O  \in( <x>, <y> )
# set theoretic containment for a projective space and a subspace. 
##
InstallOtherMethod( \in, 
	"for a projective space and any of its subspaces", 
	[ IsProjectiveSpace, IsSubspaceOfProjectiveSpace ],
	function( x, y )
		if x = y!.geo then
			return false;
		else
			Error( "<x> is different from the ambient space of <y>" );
		fi;
	end );

# CHECKED 11/09/11 jdb
#############################################################################
#O  \in( <x>, <y> )
# returns false if and only if (and this is checked) <y> is the empty subspace
# in the projective space <x>.
##
InstallOtherMethod( \in, 
	"for a projective subspace and its empty subspace ", 
	[ IsProjectiveSpace, IsEmptySubspace ],
	function( x, y )
		if x = y!.geo then
			return false;
		else
			Error( "<x> is different from the ambient space of <y>" );
		fi;
	end );

# CHECKED 19/02/14 jdb
#############################################################################
#O  \in( <x>, <dom> )
# returns true if and only if the ambient space of x is equal to dom.
# note that when <x> is an element of a projective space, if would be
# sufficient to define ps as x!geo. But for <x> an element of e.g. a quadric
# and dom the ambient space of the quadric, this would result in checking
# whether a quadric = a projective space, and for this check no method is
# implemented for \= (doing so would be ridiculous). However, to make sure
# that e.g. checking wheter a point of a quadric lies in the ambient space
# of a projective space, this method will be called. So we have to use 
# the AmbientSpace of the element here.
##
InstallMethod( \in, 
	"for a subspace of a projective space and projective space",  
    [IsSubspaceOfProjectiveSpace, IsProjectiveSpace],
	function( x, dom )
		local ps;
		ps := AmbientSpace(x);
        if dom = ps then
          return ps!.dimension = dom!.dimension and
			ps!.basefield = dom!.basefield;
        else
          Error( "<dom> is different from the ambient space of <x>" );
        fi;
	end );

# CHECKED 11/09/11 jdb
#############################################################################
#O  \in( <x>, <dom> )
# returns true if and only if (and this is checked) <x> is the empty subspace
# in the projective space <dom>.
##
InstallMethod( \in, 
	"for a subspace of a projective space and projective space",  
    [IsProjectiveSpace, IsSubspaceOfProjectiveSpace],
	function( dom, x )
		local ps;
		ps := x!.geo;
		return ps!.dimension = dom!.dimension and 
			ps!.basefield = dom!.basefield;
	end );


# CHECKED 14/09/11 jdb
#############################################################################
#O  IsIncident( <x>, <y> )
# returns true if and only if <x> is incident with <y>. Relies on set theoretic
# containment, which is implemented genericly for elements of Lie geometries.
##
InstallMethod( IsIncident, 
	"for two subspaces of the same projective space",
	[IsSubspaceOfProjectiveSpace, IsSubspaceOfProjectiveSpace],
	# returns true if the subspace is contained in the projective space
    function(x,y)
		return x in y or y in x;
	end );

#InstallMethod( IsIncident,  [IsProjectiveSpace,
#        IsSubspaceOfProjectiveSpace],
#        # returns true if the subspace is contained in the projective space
#        function(x,y)
#                return y in x;
#        end );

#InstallMethod( IsIncident,  [IsSubspaceOfProjectiveSpace,
#        IsProjectiveSpace],
#        # returns true if the subspace is contained in the projective space
#        function(x,y)
#                return x in y;
#        end );

#InstallMethod( IsIncident,  [IsProjectiveSpace,
#        IsSubspaceOfProjectiveSpace],
#        # returns true if the subspace is contained in the projective space
#        function(x,y)
#                return y in x;
#        end );


# I will change "drastically" here.
# this method is converted into a generic method to test set theoretic containment
# for elements of Lie geometries. Put in the file liegeometry.gi 

#InstallMethod( IsIncident,  [IsSubspaceOfProjectiveSpace,
## some of this function is based on the
## SemiEchelonMat function. we save time by assuming that the matrix of
## each subspace is already in semiechelon form.
## method only applies to projective and polar spaces
#  IsSubspaceOfProjectiveSpace],
#  function( x, y )
#    local ambx, amby, typx, typy, mat,
#          zero,      # zero of the field of <mat>
#          nrows,
#          ncols,     # number of columns in <mat>
#          vectors,   # list of basis vectors
#          nvectors,
#          i,         # loop over rows
#          j,         # loop over columns
#          z,         # a current element
#          nzheads,   # list of non-zero heads
#          row;       # the row of current interest


#    ambx := x!.geo;
#    amby := y!.geo;
#    typx := x!.type;
#    typy := y!.type;
#    
#    if ambx!.vectorspace = amby!.vectorspace then
    
#        if typx >= typy then
#          vectors := x!.obj;
#          nvectors := typx;
#          mat := MutableCopyMat(y!.obj);
#          nrows := typy;
#        else
#          vectors := y!.obj;
#          nvectors := typy;
#          mat := MutableCopyMat(x!.obj);
#          nrows := typx;
#        fi;
      # subspaces of type 1 need to be nested to make them lists of vectors

#      if nrows = 1 then mat := [mat]; fi;
#      if nvectors = 1 then vectors := [vectors]; fi;

#      ncols:= amby!.dimension + 1;
#      zero:= Zero( mat[1][1] );

      # here we are going to treat "vectors" as a list of basis vectors. first
      # figure out which column is the first nonzero column for each row
#      nzheads := [];
#      for i in [ 1 .. nvectors ] do
#        row := vectors[i];
#        j := PositionNot( row, zero );
#        Add(nzheads,j);
#      od;

      # now try to reduce each row of "mat" with the basis vectors
#      for i in [ 1 .. nrows ] do
#        row := mat[i];
#        for j in [ 1 .. Length(nzheads) ] do
#            z := row[nzheads[j]];
#            if z <> zero then
#              AddRowVector( row, vectors[ j ], - z );
#            fi;
#        od;

        # if the row is now not zero then y is not a subvariety of x
#        j := PositionNot( row, zero );
#        if j <= ncols then
#                return false;
#        fi;

#      od;
      
#      return true;
#    else
#      Error( "The subspaces belong to different ambient spaces" );
#    fi;
#    return false;
#  end );

#############################################################################
# Span/Meet methods in many flavours. 
#############################################################################

# CHECKED 11/09/11 jdb
#############################################################################
#O  Span( <x>, <y> )
# returns <x> if and only if <y> is a subspace in the projective space <x>
##
InstallMethod( Span, 
	"for a projective space and a subspace",
	[IsProjectiveSpace, IsSubspaceOfProjectiveSpace],
    function(x,y)
		if y in x then return x; fi;
	end );

# CHECKED 11/09/11 jdb
#############################################################################
#O  Span( <x>, <y> )
# returns <y> if and only if <x> is a subspace in the projective space <y>
##
InstallMethod( Span, "for a subspace of a projective space and a projective space",
	[IsSubspaceOfProjectiveSpace, IsProjectiveSpace],
	function(x,y)
	if x in y then return y; fi;
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
#      span := MutableCopyMat(EchelonMat(span).vectors);
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

# CHECKED 11/09/11 jdb
#############################################################################
#O  Span( <x>, <y> )
# returns <x,y>, <x> and <y> two subspaces of a projective space.
# cvec note (19/3/14: SumIntersectionMat seems to be incompatible with
# cvecs. So I will Unpack, do what I have to do, and get what I want
# since VectorSpaceToElement is helping here.
##
InstallMethod( Span, 
	"for two subspaces of a projective space",
	[IsSubspaceOfProjectiveSpace, IsSubspaceOfProjectiveSpace],
	function( x, y )
	## This method is quicker than the old one
	local ux, uy, typx, typy, span, vec;
	typx := x!.type;
	typy := y!.type;
	vec := x!.geo!.vectorspace;
	if vec = y!.geo!.vectorspace then
		ux := Unpack(Unwrap(x)); #here is the cvec/cmat unpack :-)
		uy := Unpack(Unwrap(y));
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

# ADDED 30/11/2011 jdb
#############################################################################
#O  Span( <x>, <y> )
# returns <x,y>, <x> and <y> two subspaces of a projective space.
##
InstallMethod( Span, 
	"for two subspaces of a projective space and a boolean",
	[IsSubspaceOfProjectiveSpace, IsSubspaceOfProjectiveSpace, IsBool],
	function( x, y, b )
		return Span(x,y);
	end );

# CHECKED 20/09/11 
#############################################################################
#O  Span( <l> )
# returns the span of the projective subspaces in <l>.
# cvec note (19/3/14: Unpack for this kind of work, in combination with VectorSpaceToElement
# seems to be succesful. So the changes here are obvious:
##
InstallMethod( Span,
    "for a homogeneous list of subspaces of a projective space",
	[ IsHomogeneousList and IsSubspaceOfProjectiveSpaceCollection ],
	function( l )  
		local unwrapped, r, unr, amb, span, temp, x, F, list;  
		# first we check that all items in the list belong to the same ambient space
		if Length(l)=0 then 
			return [];
		elif not Size(AsDuplicateFreeList(List(l,x->AmbientSpace(x))))=1 then 
			Error("The elements in the list do not have a common ambient space");
		else
			x := l[1];
			amb := AmbientSpace(x!.geo);
			F := amb!.basefield;
			unwrapped := [];
			for r in l do
				unr := Unpack(r!.obj); #here is the change.
				if r!.type = 1 then unr := [unr]; fi;
				Append(unwrapped, unr);
			od;
			span := MutableCopyMat(unwrapped);
			# span := MutableCopyMat(EchelonMat(span).vectors); #not necessary anyway, since VectorSpaceToElement is used.
#   JB: Yes it is necessary for the following part!!!
#			if Length(span) = amb!.dimension + 1 then
#				return amb;
#			fi;
			return VectorSpaceToElement(amb,span);
		fi;
	end );

# CHECKED 11/09/11 jdb
#############################################################################
#O  Span( <l> )
# returns the span of the projective subspaces in <l>.
##
InstallMethod( Span,
	"for a list",
	[ IsList ],
	function( l )
		local list, listels, listgeos, x;
        if Length(l) = 0 then
            return [];
        else
            list := Filtered(l,x->not IsEmptySubspace(x));
            listels := Filtered(list,x->IsSubspaceOfProjectiveSpace(x));
            if Length(listels) = Length(list) then
                if not Size(AsDuplicateFreeList(List(listels,x->AmbientGeometry(x))))=1 then
                    Error("The elements in the list do not have a common ambient space");
                else
                    return Span(listels); #now we may assume that listels is homogeneous.
                fi;
            else
                listgeos := Filtered(l,x->IsProjectiveSpace(x));
                if Length(listels) + Length(listgeos) <> Length(list) then
                    Error( " <list> does not contain only subspaces and projective spaces");
                fi;
                if not Length(DuplicateFreeList(listgeos))=1 then
                    Error( "<list> should not contain different projective space");
                fi;
                if ForAll(listels,x->x in listgeos[1]) then
                    return listgeos[1];
                else
                    Error( "not all subspaces in <l> belong to the same geometry");
                fi;
            fi;
        fi;
    end );

# ADDED 30/11/2011 jdb
# this is a "helper" operation. We do not expect the user to use this variant
# when he knows that list is a list of projective subspaces. In this case, we will
# not document it. When dealing with a list of subspaces polar spaces, the user
# could get into this without realising. The result is of course then just Span(l).
#############################################################################
#O  Span( <x>, <y> )
# returns <x,y>, <x> and <y> two subspaces of a projective space.
##
InstallMethod( Span, 
	"for a list and a boolean",
	[IsList, IsBool],
	function( l, b )
		return Span(l);
	end );

# CHECKED 14/09/11 jdb
#############################################################################
#O  Meet( <x>, <y> )
# returns <y> if and only if <y> is a subspace in the projective space <x>
##
InstallMethod( Meet, 
	"for a projective space and a subspace of a projective space",
	[IsProjectiveSpace, IsSubspaceOfProjectiveSpace],
    function(x,y)
		if y in x then return y;
	fi;
end );

# CHECKED 14/09/11 jdb
#############################################################################
#O  Meet( <x>, <y> )
# returns <y> if and only if <y> is a subspace in the projective space <x>
##
InstallMethod( Meet, 
	"for a subspace of a projective space and a projective space",
	[IsSubspaceOfProjectiveSpace, IsProjectiveSpace],
	function(x,y)
		if x in y then return x;
	fi;
end );

# CHECKED 14/09/2011 jdb.
# CHANGED 30/11/2011 jdb. it is not possible to compare e.g. two quadrics, or
# just any two lie geometries using \=. So if <x> and <y> belong to two different
# Lie geometries with the same ambient projective space, it is not possible to decide
# if the two geometries equal. So it makes no sense to construct the result in any other
# space then the ambient space.
# cvec note (19/3/14): SumIntersectionMat seems to be incompatible with
# cvecs. So I will Unpack, do what I have to do, and get what I want
# since VectorSpaceToElement is helping here.
#############################################################################
#O  Meet( <x>, <y> )
# returns the intersection of <x> and <y>, two subspaces of a projective space.
##
InstallMethod( Meet,
	"for two subspaces of a projective space",
	[IsSubspaceOfProjectiveSpace, IsSubspaceOfProjectiveSpace],
	function( x, y )
		local ux, uy, typx, typy, int, f, rk;
		typx := x!.type;
		typy := y!.type;
		if x!.geo!.vectorspace = y!.geo!.vectorspace then 
			ux := Unpack(Unwrap(x)); #here is the change. 
			uy := Unpack(Unwrap(y));
			if typx = 1 then ux := [ux]; fi;
			if typy = 1 then uy := [uy]; fi;
			f := x!.geo!.basefield; 
			int := SumIntersectionMat(ux, uy)[2];
			if not int=[] then 
				return VectorSpaceToElement( AmbientSpace(x), int);
			else 
				return EmptySubspace(AmbientSpace(x));
			fi;
		else
			Error("Subspaces belong to different ambient spaces");
		fi;
  end );

# CHECKED 14/09/2011 jdb.
#############################################################################
#O  Meet( <l> )
# returns the intersection the subspaces of a projective space in the list <l>
##
InstallMethod( Meet,
	"for a homogeneous list that is a collection of subspaces of a projective space",
	[ IsHomogeneousList and IsSubspaceOfProjectiveSpaceCollection],
	function( l )  
		local int, iter, ps,em;
		# first we check if all subspaces have the same ambient geometry
		ps := AsDuplicateFreeList(List(l,x->AmbientSpace(x)));
		if not Size(ps)=1 then 
			Error("The elements in the list do not have a common ambient space");
		else
		# We use recursion for this routine.
		# Not ideal, but there is no "SumIntersectionMat" for lists
			ps := ps[1];
			em := EmptySubspace(ps);
			if not IsEmpty(l) then
				if Length(l)=1 then return l;
				else
					iter := Iterator(l);
					int := NextIterator(iter);
					repeat
						int := Meet(int, NextIterator(iter));
					until int = em or IsDoneIterator(iter);
					return int;
				fi;
			else return []; #I think this will never happen, since IsSubspaceOfProjectiveSpaceCollection([]) is false.
			fi;
		fi;
	end );

# CHECKED 14/09/2011 jdb.
#############################################################################
#O  Meet( <l> )
# returns the intersection the objects list <l>
##
InstallMethod( Meet,
	"for a list",
	[ IsList ],
	function( l )  
		local pg,checklist,list,x;
		# This method is added to allow the list ("l") to contain the projective space 
		# or the empty subspace. If this method is selected, it follows that the list must
		# contain the whole projective space or the empty set. 
		if IsEmpty(l) then return [];
		else
			if Length(l)=1 then return l[1];
			else
				# First we check that the non emptysubspace elements belong to the same ambient space
				checklist:=Filtered(l,x->not IsEmptySubspace(x) and not IsProjectiveSpace(x));
				if not Size(AsDuplicateFreeList(List(checklist,x->AmbientSpace(x))))=1 then 
					Error("The elements in the list do not have a common ambient space");
				else	
					if EmptySubspace(AmbientSpace(l[1])) in l then return EmptySubspace(AmbientSpace(l[1]));
					else
						pg:=AmbientSpace(checklist[1]); 
						# the first element in l could be the emptysubspace,
						# so we choose the first element of the checklist
						list:=Filtered(l,x->not x = pg);
						return Meet(list);
					fi;
				fi;
			fi;
		fi;
	end );

#############################################################################
## Methods for random selection of elements
#############################################################################  

# CHECKED 14/09/2011 jdb.
#############################################################################
#O  RandomSubspace( <pg>, <d> )
# returns a random subspace of projective dimension <d> in the projective space
# <pg>
##
InstallMethod( RandomSubspace,
	"for a projective space and a projective dimension",
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

# CHECKED 14/09/2011 jdb.
#############################################################################
#O  RandomSubspace( <subspace>, <d> )
# returns a random subspace of projective dimension <d> contained in the given
# projective subspace <subspace>
##
InstallMethod( RandomSubspace,
	"for a subspace of a projective space and a dimension",
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

# CHECKED 14/09/2011 jdb.
#############################################################################
#O  RandomSubspace( <pg> )
# returns a random subspace of random projective dimension in the given projective
# space <pg>
##
InstallMethod( RandomSubspace, 
	"for a projective space",
	[IsProjectiveSpace],
	function(pg)
		local list,i;
		list:=[0..Dimension(pg)-1];
		i:=Random(list);
		return RandomSubspace(pg,i);
	end );
			
		
# CHECKED 14/09/2011 jdb.
#############################################################################
#O  Random( <subs> )
# returns a random subspace out of the collection of subspaces of given dimension
# of a projective space.
##
InstallMethod( Random, 
	"for a collection of subspaces of a projective space",
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
  
# CHECKED 14/09/2011 jdb.
# Fixed a bug 6/02/2013 jdb. So do not trust me completely when I writed CHECKED :-(
#############################################################################
#O  Random( <subs> )
# returns a random subspace out of the collection of all subspaces of 
# a projective space.
##
InstallMethod( Random, 
	"for a collection of all subspaces of a projective space",
	[ IsAllSubspacesOfProjectiveSpace ],
    # chooses a random element out of the collection of all subspaces of a projective space
	function( subs )
	    return RandomSubspace(subs!.geometry);
	end );

# CHECKED 14/09/2011 jdb. 
# but I am unhappy with this method, since it is not possible to select e.g. 
# a random line through a point now.
#
# This should be ok now. The method Random was wrong for shadows. Fixed 30/10/2012 ml.
# The variable "x" was undefined. Fixed 07/11/2012 jb.
#############################################################################
#O  Random( <subs> )
# returns a random element out of the collection of subspaces of given
# dimension contained in a subspace of a projective space
##
#InstallMethod( Random, 
#	"for a collection of subspaces of a subspace of a projective space",
#	[ IsShadowSubspacesOfProjectiveSpace ],
    # chooses a random element out of the collection of subspaces of given
    # dimension of a subspace of a projective space
#	function( shad )
#		local d, pg, x, vspace, W;
		## the underlying projective space
#		pg := shad!.geometry;
#		x:=shad!.parentflag;
#		vspace:=UnderlyingVectorSpace(x);
#		if not IsInt(shad!.type) then
#			Error("The subspaces of the collection need to have the same dimension");
#        fi;
		## the common type of elements of shads
#		d := shad!.type;
		# now we need to distinguish two cases	
#		if d>Dimension(vspace) then
		# in this case the shadow consists of subspaces through x
#		  repeat W:=Span(RandomSubspace(pg,d-Dimension(vspace)-1),x);
#		  until Dimension(W)=d-1;
#		else
		# in this case we can just take a random subspace in x
#		  W:=RandomSubspace(x,d-1); #here was 'pg' instead of 'x', which is a bug! 
#        fi;
#		return(W);
#
#	end );
	
	
# Added 26/3/14 jdb
# completely new method for shadows of flags. Note that the above method
# only worked for shadows of one element. The linear algebra for the new method
# is actually done in the creation of the shadow. The Iterator method
# for shadows was inpiring for this method.
#############################################################################
#O  Random( <subs> )
# returns a random element out of the collection of subspaces of given
# dimension contained in a subspace of a projective space
##
InstallMethod( Random, 
	"for a collection of subspaces of a subspace of a projective space",
	[ IsShadowSubspacesOfProjectiveSpace ],
    # chooses a random element out of the collection of subspaces of given
    # dimension of a subspace of a projective space
	function( shad )
		local rand, mat, vs, j, pg;
		## the underlying projective space
		pg := shad!.geometry;
		j := shad!.type;
		rand := BasisVectors(Basis(Random(Subspaces(shad!.factorspace,j-Size(shad!.inner)))));
		mat := shad!.inner;
		vs := Concatenation(rand,mat);
		return VectorSpaceToElement(pg,vs);
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

InstallMethod( BaerSublineOnThreePoints,
	"for three points of a projective space",
	[IsSubspaceOfProjectiveSpace, IsSubspaceOfProjectiveSpace, IsSubspaceOfProjectiveSpace],
	# UNCHECKED
	function( x, y, z )
  # returns the Baersubline determined by three collinear points x,y,z
	local geo, gfq2, gfq, t, subline;
	if Length(DuplicateFreeList(List([x,y,z],t->AmbientSpace(t)))) <> 1 then
		Error( "<x>, <y>, <z> must be points of the same projective space" );
	fi;
	geo := AmbientSpace(x);
	gfq2 := geo!.basefield;
	if IsOddInt(DegreeOverPrimeField(gfq2)) then
		Error( "the order of the basefield must be a square" );
	fi;
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

#############################################################################
# The useful non-user operation. This code comes from John and was found 
# affinespace.gi. As I can use it already here, I put it here not to violate 
# the modularity of Fining. 
#############################################################################

#############################################################################
#O  ComplementSpace( <space>, <mat> )
#  Taken from the code for BaseSteinitzVectors.
#  This operation computes a list of vectors of <space>,
#  in a deterministic way, such that they form a complement
#  in <space> of the subspace spanned by <mat>.
##
InstallMethod( ComplementSpace, 
	"for a vector space and a maxtrix",	
	[IsVectorSpace, IsFFECollColl],
	function( space, mat )
    	
    local  z, l, b, i, j, k, stop, v, dim, bas;
    bas := MutableCopyMat( BasisVectors( Basis(space) ));
    z := Zero( bas[1][1] );
    if NrRows( mat ) > 0  then
        mat := MutableCopyMat( mat );
        TriangulizeMat( mat );
    fi;
    dim := Length( bas[1] );
    l := Length( bas ) - NrRows( mat );
    b := [  ];
    i := 1;
    j := 1;
    while Length( b ) < l  do
        stop := false;
        repeat
            if j <= dim and (NrRows( mat ) < i or mat[i,j] = z)  then
                v := PositionProperty( bas, k -> k[j] <> z );
                if v <> fail  then
                    v := bas[v];
                    v := 1 / v[j] * v;
                    Add( b, v );
                fi;
            else
                stop := true;
                if i <= NrRows( mat )  then
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
  
#############################################################################
# Interesting subgroups of projectivities.
#############################################################################

#############################################################################
# Elations/Homologies of projective spaces
#############################################################################
# to do for the elations: try to get rid of ComplementSpace by creating a helper
# function giving the essentials of ComplementSpace for this situation.

#############################################################################
#O  ElationOfProjectiveSpace( <sub>, <point1>, <point2> )
#  return the uniquely defined elation with axis sub, mapping point1 on point2
##
InstallMethod( ElationOfProjectiveSpace,
	"for a hyperplane and two points of the same projective space",
	[ IsSubspaceOfProjectiveSpace, IsSubspaceOfProjectiveSpace, IsSubspaceOfProjectiveSpace ],
	function(sub,point1,point2)
	local en,e0,ei,mat,vssub,n,f,c,M,el,centre,p2vect,ps;
	ps := AmbientSpace(sub);
	n := Dimension(ps);
	if not Size(AsDuplicateFreeList([AmbientSpace(sub),AmbientSpace(point1),AmbientSpace(point2)]))=1 then 
		Error("The elements <sub>, <point1>, and <point2> do not have a common ambient space");
	elif Dimension(sub) <> n-1 or Dimension(point1) <> 0 or Dimension(point2) <> 0 then
		Error("<sub> must be a hyperplane, <point1> and <point2> must be points");
	elif point1 in sub or point2 in sub then
		Error("The points <point1> and <point2> must not be incident with <sub>");
	fi;
	centre := Meet(sub,Span(point1,point2));
	mat := UnderlyingObject(sub);
	f := BaseField(sub);
	vssub := VectorSpace(f,mat);
	e0 := Unpack(UnderlyingObject(centre));
	ei := BasisVectors(Basis(ComplementSpace(vssub,[e0])));
	en := UnderlyingObject(point1);
	M := Concatenation([e0],ei,[en]);
	el := IdentityMat(n+1,f);
	p2vect := UnderlyingObject(point2)*M^-1;
	el[n+1,1] := p2vect[1]/p2vect[n+1];
	el := M^(-1)*el*M;
	return CollineationOfProjectiveSpace(el,f);
end );

#############################################################################
#O  ProjectiveElationGroup( <sub>, <centre> )
#  returns group of elations with axis sub and centre centre
##
InstallMethod( ProjectiveElationGroup,
	"for a hyperplane and a point of a projective space",
	[ IsSubspaceOfProjectiveSpace, IsSubspaceOfProjectiveSpace ],
	function(sub,centre)
	local en,e0,ei,mat,vssub,n,f,c,M,el,ps,gens,fbas,x,group;
	ps := AmbientSpace(sub);
	n := Dimension(ps);
	if not (ps=AmbientSpace(centre)) then 
		Error("The elements <sub> and <centre> do not have a common ambient space");
	elif (Dimension(sub) <> n-1) or (Dimension(centre) <> 0) then
		Error("<sub> must be a hyperplane, <centre> must be a point");
	elif not centre in sub then
		Error("The point <centre> and <sub> must be incident");
	fi;
	mat := UnderlyingObject(sub);
	f := BaseField(sub);
	vssub := VectorSpace(f,mat);
	e0 := Unpack(UnderlyingObject(centre));
	ei := BasisVectors(Basis(ComplementSpace(vssub,[e0])));
	en := BasisVectors(Basis(ComplementSpace(f^(n+1),mat)))[1];
	M := Concatenation([e0],ei,[en]);
	el := IdentityMat(n+1,f);
	gens := [];
	fbas := BasisVectors(Basis(f));
	for x in fbas do
		el[n+1,1] := x;
		Add(gens,ShallowCopy(M^(-1)*el*M));
	od;
	#group := SubgroupNC(ProjectivityGroup(ps),List(gens,x->CollineationOfProjectiveSpace(x,f)));
	group := Group(List(gens,x->CollineationOfProjectiveSpace(x,f)));
	SetOrder(group,Size(f));
	return group;
end );

# cmat change 20/3/14. some arithmetic is not yet possible with cmats. 
#############################################################################
#O  ProjectiveElationGroup( <sub> )
#  returns group of elations with axis sub 
##
InstallMethod( ProjectiveElationGroup,
	"for a hyperplane of a projective space",
	[ IsSubspaceOfProjectiveSpace ],
	function(sub)
	local en,mat,vssub,n,f,c,M,el,ps,gens,fbas,x,group,i;
	ps := AmbientSpace(sub);
	n := Dimension(ps);
	if (Dimension(sub) <> n-1) then
		Error("<sub> must be a hyperplane");
	fi;	
	mat := Unpack(UnderlyingObject(sub));
	f := BaseField(sub);
	vssub := VectorSpace(f,mat);
	#e0 := UnderlyingObject(centre);
	#ei := BasisVectors(Basis(ComplementSpace(vssub,[e0])));
	en := BasisVectors(Basis(ComplementSpace(f^(n+1),mat)))[1];
	M := Concatenation(mat,[en]);
	el := ShallowCopy(IdentityMat(n+1,f));
	gens := [];
	fbas := BasisVectors(Basis(f));
	for x in fbas do
		for i in [1..n] do
			el[n+1,i] := x;
			Add(gens,ShallowCopy(M^(-1)*el*M));
		od;
	od;
	#group := SubgroupNC(ProjectivityGroup(ps),List(gens,x->CollineationOfProjectiveSpace(x,f)));
	group := Group(List(gens,x->CollineationOfProjectiveSpace(x,f)));
	SetOrder(group,Size(f)^n);
	return group;
end );

#############################################################################
#O  HomologyOfProjectiveSpace( <sub>, <centre>, <point1>, <point2> )
#  return the uniquely defined homology with axis sub and centre centre, mapping point1 on point2
##
InstallMethod( HomologyOfProjectiveSpace,
	"for a hyperplane and three points of the same projective space",
	[ IsSubspaceOfProjectiveSpace, IsSubspaceOfProjectiveSpace, IsSubspaceOfProjectiveSpace, IsSubspaceOfProjectiveSpace ],
	function(sub,centre,point1,point2)
	local en,e0,ei,mat,vssub,n,f,c,M,el,p1vect,p2vect,ps;
	ps := AmbientSpace(sub);
	n := Dimension(ps);
	if not Size(AsDuplicateFreeList([AmbientSpace(sub),AmbientSpace(centre),AmbientSpace(point1),AmbientSpace(point2)]))=1 then 
		Error("The elements <sub>, <centre>, <point1>, and <point2> do not have a common ambient space");
	elif Dimension(sub) <> n-1 or Dimension(centre) <> 0 or Dimension(point1) <> 0 or Dimension(point2) <> 0 then
		Error("<sub> must be a hyperplane, <centre>, <point1> and <point2> must be points");
	elif centre in sub or point1 in sub or point2 in sub then
		Error("The points <centre>, <point1> and <point2> must not be incident with <sub>");
	elif centre=point1 or centre=point2 then
		Error("<centre> is fixed and must be different from <point1> and <point2>");
	elif Dimension(Span([centre,point1,point2])) <> 1 then
		Error("<centre>, <point1>, and <point2> must span a line");
	fi;
	mat := UnderlyingObject(sub);
	f := BaseField(sub);
	vssub := VectorSpace(f,mat);
	e0 := UnderlyingObject(Meet(Span(centre,point1),sub));
	ei := BasisVectors(Basis(ComplementSpace(vssub,[e0])));
	en := UnderlyingObject(centre);
	M := Concatenation([e0],ei,[en]);
	el := IdentityMat(n+1,f);
	p1vect := UnderlyingObject(point1)*M^-1;
	p2vect := UnderlyingObject(point2)*M^-1;
	el[n+1,n+1] := (p2vect[n+1]*p1vect[1])/(p2vect[1]*p1vect[n+1]);
	el := M^(-1)*el*M;
	return CollineationOfProjectiveSpace(el,f);
end );

## cmat change 20/3/14. some arithmetic is not yet possible with cmats. 
#############################################################################
#O  ProjectiveHomologyGroup( <sub>, <centre> )
#  returns group of homologies with axis sub and centre centre
##
InstallMethod( ProjectiveHomologyGroup,
	"for a hyperplane and a point of a projective space",
	[ IsSubspaceOfProjectiveSpace, IsSubspaceOfProjectiveSpace ],
	function(sub,centre)
	local n,M,el,ps,x,group,f,q;
	ps := AmbientSpace(sub);
	n := Dimension(ps);
	if not (ps=AmbientSpace(centre)) then 
		Error("The elements <sub> and <centre> do not have a common ambient space");
	elif (Dimension(sub) <> n-1) or (Dimension(centre) <> 0) then
		Error("<sub> must be a hyperplane, <centre> must be a point");
	elif centre in sub then
		Error("The point <centre> and <sub> must not be incident");
	fi;
	M := Concatenation(Unpack(UnderlyingObject(sub)),[Unpack(UnderlyingObject(centre))]);
	f := BaseField(sub);
	q := Size(f);
	el := IdentityMat(n+1,f);
	el[n+1,n+1] := Z(q);
	return Group(CollineationOfProjectiveSpace(M^(-1)*el*M,f));
end );

#############################################################################
#O  SingerCycleCollineation( <dim>, <q> )
#  returns a matrix which is a Singer cycle, of GF(q)^dim
##

InstallMethod( SingerCycleMat, [ IsInt, IsInt],
	function(n, q)
	local basis, omega, mat;
	basis := Basis(AsVectorSpace(GF(q),GF(q^(n+1))));
	omega := Z(q^(n+1));
	# companion matrix
	mat := List(BasisVectors(basis), t -> Coefficients(basis, t*omega));
	return mat;
end);

#############################################################################
#O  SingerCycleCollineation( <dim>, <q> )
#  returns a Singer cycle of PG(<dim>,<q>)
##

InstallMethod( SingerCycleCollineation, [IsInt, IsInt],
	function(n, q)
	return CollineationOfProjectiveSpace(SingerCycleMat(n,q), GF(q));
end);

#############################################################################
#O  IncidenceGraph( <gp> )
# Note that computing the collineation group of a projective space is zero
# computation time. So useless to print the warning here if the group is not
# yet computed.
###
InstallMethod( IncidenceGraph,
    "for a projective space",
    [ IsProjectiveSpace ],
    function( ps )
        local elements, graph, adj, coll, sz;
		if IsBound(ps!.IncidenceGraphAttr) then
            return ps!.IncidenceGraphAttr;
        fi;
		coll := CollineationGroup(ps);
		elements := Concatenation(List([1..Rank(ps)], i -> List(AsList(ElementsOfIncidenceStructure(ps,i)))));
        adj := function(x,y)
            if x!.type <> y!.type then
                return IsIncident(x,y);
            else
                return false;
            fi;
        end;
        graph := Graph(coll,elements,OnProjSubspaces,adj,true);
        Setter( IncidenceGraphAttr )( ps, graph );
        return graph;
    end );

# Added 18/09/18 jdb.
#############################################################################
#O  EvaluateForm( <form>, <el1>, <el2> )
# returns the "action" of <form> (sesquilinear) on the subspaces <el1> and <el2>.
# All it does is unpack the underlyingobject and call the existing method
# for EvaluateForm and matrices/vectors (and counts on checking of input
# of the existing method.
#
InstallMethod( EvaluateForm,
    "for a sesquilinear form and two elements of a Lie geometry",
    [IsSesquilinearForm, IsElementOfLieGeometry, IsElementOfLieGeometry],
    function(form, el1, el2)
    local list;
    list := Set([AmbientSpace(el1)!.vectorspace,AmbientSpace(el2)!.vectorspace]);
    if Size(list) <> 1 or not form!.vectorspace in list then
        Error("underlying vectorspaces of <form>, <el1> and <el2> do not match");
    fi;
    return EvaluateForm(form,Unpack(UnderlyingObject(el1)),Unpack(UnderlyingObject(el2)));
    end );

# Added 18/09/18 jdb.
#############################################################################
#O  EvaluateForm( <form>, <el1> )
# returns the "action" of <form> (quadratic) on the subspace <el1>.
# All it does is unpack the underlyingobject and call the existing method
# for EvaluateForm and matrices/vectors (and counts on checking of input
# of the existing method.
#
InstallMethod( EvaluateForm,
    "for a quadratic form and two elements of a Lie geometry",
    [IsQuadraticForm, IsElementOfLieGeometry ],
    function(form, el1 )
    if not form!.vectorspace = AmbientSpace(el1)!.vectorspace then
        Error("underlying vectorspaces of <form> and <el1> do not match");
    fi;
    return EvaluateForm(form,Unpack(UnderlyingObject(el1)));
    end );

# Added 18/09/18 jdb.
#############################################################################
#O  \^( <el>, <form> )
# returns the "action" of <form> (quadratic) on the subspace <el1>.
# All it does is call EvaluateForm
#
InstallMethod( \^,
    "for an element of a Lie geometry and a quadratic form",
    [IsElementOfLieGeometry, IsQuadraticForm ],
    function(el1, form)
        return EvaluateForm(form,el1);
    end );

# Added 18/09/18 jdb.
#############################################################################
#O  \^( <pair>, <form> )
# returns the "action" of <form> (quadratic) on the subspace <el1>.
# All it does is call EvaluateForm
#
InstallMethod( \^,
    "for two elements of a Lie geometry and a sesquilinear form",
    [IsSubspaceOfProjectiveSpaceCollection, IsSesquilinearForm ],
    function(pair, form)
        if Size(pair) <> 2 then
            Error("The first argument must be a pair of subspaces of a projective space");
        fi;
        return EvaluateForm(form,pair[1],pair[2]);
    end );
