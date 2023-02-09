#############################################################################
##
##  affinespace.gi              FinInG package
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
##  Implementation stuff for affine spaces
##
#############################################################################

#############################################################################
#
# Construction of affine spaces
#
#############################################################################

# CHECKED 12/03/12 jdb
#############################################################################
#O  AffineSpace( <d>, <f> )
# returns AG( <d>, <f> )
##
InstallMethod( AffineSpace, 
	"for a dimension and a field",
 	[ IsPosInt, IsField ],
	function( d, f )
		local geo;
		geo := rec( dimension := d, basefield := f, 
                vectorspace := FullRowSpace(f, d) );
		Objectify( NewType( GeometriesFamily,
                        IsAffineSpace and IsAffineSpaceRep ), geo );
		SetAmbientSpace(geo,geo);
		SetRankAttr(geo,d); #this makes Rank applicable without adding more code in this file
		return geo;
	end );

# CHECKED 12/03/12 jdb
#############################################################################
#O  AffineSpace( <d>, <q> )
# returns AG( <d>, GF(<q>) )
##
InstallMethod( AffineSpace, 
	"for a dimension and a prime power",
	[ IsPosInt, IsPosInt ],
	function( d, q )
		return AffineSpace(d, GF(q));
	end );

############################################################################
#
# ViewObj/PrintObj for affine spaces
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

# ADDED 16/12/14 jdb, this is needed to make one check in ShadowOfElement
#############################################################################
#O  \=( <ag1>, <ag2> )
# Code taken from \= for projective spaces.
##
InstallMethod( \=, 
	"for two projective spaces",
	[IsAffineSpace, IsAffineSpace],
	function(ag1,ag2);
		return UnderlyingVectorSpace(ag1) = UnderlyingVectorSpace(ag2);
	end );

#############################################################################
#
# Attributes of affine spaces
#
#############################################################################

# CHECKED 20/03/11 jdb
#############################################################################
#A  Dimension( <as> )
# returns the projective dimension of <ps>
##
InstallMethod( Dimension, 
	"for an affine space",
	[ IsAffineSpace and IsAffineSpaceRep ],
	as -> as!.dimension
	);

# CHECKED 20/03/12 jdb
#############################################################################
#O  UnderlyingVectorSpace( <as> )
#returns the projective dimension of <ps>
##
InstallMethod( UnderlyingVectorSpace, 
	"for an affine space",
	[ IsAffineSpace and IsAffineSpaceRep ],
	as -> as!.vectorspace
	);
	
#############################################################################
#O  AmbientSpace( <subspace> ) returns the ambient space of <subspace>
##
InstallMethod( AmbientSpace,
	"for a subspace of an affine space",
	[IsSubspaceOfAffineSpace],
	function(subspace)
		return subspace!.geo;
	end );

# CHECKED 20/03/12 jdb
#############################################################################
#O  BaseField( <as> )
# returns the basefield of <as>
##
InstallMethod( BaseField, 
	"for an affine space", 
	[IsAffineSpace and IsAffineSpaceRep],
	ag -> ag!.basefield );

#############################################################################
#O  BaseField( <sub> )
# returns the basefield of an element of a projective space
##
InstallMethod( BaseField, 
	"for an element of a projective space", 
	[IsSubspaceOfAffineSpace],
	sub -> AmbientSpace(sub)!.basefield );

# CHECKED 12/03/12 jdb
#############################################################################
#O  TypesOfElementsOfIncidenceStructure( <ps> )
# returns the names of the types of the elements of the projective space <ps>
# the is a helper operation.
##
InstallMethod( TypesOfElementsOfIncidenceStructure, 
	"for an affine space", 
	[IsAffineSpace],
	function( as )
		local d,i,types;
		types := ["point"];
		d := Rank(as); #this line replaces next line, since next line assumes IsAffineSpaceRep
	#d := ps!.dimension;
		if d >= 2 then Add(types,"line"); fi;
		if d >= 3 then Add(types,"plane"); fi;
		if d >= 4 then Add(types,"solid"); fi;
		for i in [5..d] do
			Add(types,Concatenation("affine subspace of dim. ",String(i-1)));
		od;
		return types;
	end );

# CHECKED 12/03/12 jdb
#############################################################################
#O  TypesOfElementsOfIncidenceStructurePlural( <ps> )
# returns the plural of the names of the types of the elements of the projective space <ps>
# the is a helper operation.
##
InstallMethod( TypesOfElementsOfIncidenceStructurePlural, 
	"for an affine space",
	[IsAffineSpace],
	function( as )
		local d,i,types;
		types := ["points"];
    		d := Rank(as); #this line replaces next line, since next line assumes IsAffineSpaceRep
	#d := ps!.dimension;
		if d >= 2 then Add(types,"lines"); fi;
		if d >= 3 then Add(types,"planes"); fi;
		if d >= 4 then Add(types,"solids"); fi;
		for i in [5..d] do
			Add(types,Concatenation("affine. subspaces of dim. ",String(i-1)));
		od;
		return types;
	end );

#############################################################################
# helper methods to construct affine subspacs.
#############################################################################

#############################################################################
#O  VectorSpaceTransversalElement( <space>, <subspace>, <v> )
# This is an internal subroutine which is not expected to be used by the user;
# Returns a canonical vector from <v>+<subspace>
##
InstallMethod( VectorSpaceTransversalElement,
	"for a vector space, a matric, a vector",
	[IsVectorSpace, IsFFECollColl, IsVector],
	function(space, subspace, v)
		local basis;
		basis := SemiEchelonBasis( Subspace(space, subspace) );
		return SiftedVector(basis, v);
	end );

#############################################################################
#O  VectorSpaceTransversal( <space>, <subspace> )
# This is an internal subroutine which is not expected to be used by the user;
# Returns a typed object containing a record with two components
# Note: IsVectorSpaceTransversal is a subfilter of IsSubspacesOfVectorSpace
##
InstallMethod( VectorSpaceTransversal, 
	"for a vector space, a matrix",
	[IsVectorSpace, IsFFECollColl],
	function(space, subspace)
		return Objectify(
               NewType( CollectionsFamily( FamilyObj( space ) ),
                    IsVectorSpaceTransversal and IsVectorSpaceTransversalRep),
           rec( vectorspace := space, subspace := subspace ) );    
	end );

#############################################################################
#O  ViewObj( <trans> )
InstallMethod( ViewObj, 
	"for a vector space transversal",
	[ IsVectorSpaceTransversal and IsVectorSpaceTransversalRep ],
	function( trans )
		Print("<vector space transversal of ", trans!.vectorspace,">");
	end );
	
#############################################################################
#O  PrintObj( <trans> )
InstallMethod( PrintObj, 
	"for a vector space transversal",
	[ IsVectorSpaceTransversal and IsVectorSpaceTransversalRep ],
	function( trans )
		Print("<vector space transversal of ", trans!.vectorspace," subspace spanned by ",trans!.subspace, ">");
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
# The treatment of the matrix representing the subspace at infinity, is similar to 
# the treatment in the methods for VectorSpaceToElement in projectivespace.gi
#
# 24/3/2014
# cvec/cmat changes lead to the methods for 
# [ IsAffineSpace, IsCVecRep ]
# [ IsAffineSpace, IsCVecRep, IsCMatRep ]
#
# more methods might be necessary in the future.
# Note that an affine point is represented by just one vector, while an affine 
# space by an object [v,m], where v is vector and m is a matrix. Currently we
# will change this into a cvec/cmat. 
#
#############################################################################

# CHECKED 12/03/12 jdb
# But I am unhappy with this code. It seems obsolete if you compare this with
# the method installed in geometry.gi for Wrap.
#############################################################################
#O  Wrap( <geo>, <type>, <o> )
# This is an internal subroutine which is not expected to be used by the user;
# they would be using AffineSubspace. Recall that Wrap is declared in 
# geometry.gd. 
##
InstallMethod( Wrap, 
	"for an affine space and an object",
	[IsAffineSpace, IsPosInt, IsObject],
	function( geo, type, o )
		local w;
		w := rec( geo := geo, type := type, obj := o );
		Objectify( NewType( SoASFamily, IsElementOfIncidenceStructure and
			IsElementOfIncidenceStructureRep and IsSubspaceOfAffineSpace ), w );
		return w;
	end );

#############################################################################
#
# ViewObj/PrintObjDisplay of subspaces of affine spaces
#
#############################################################################

InstallMethod( ViewObj, 
	[ IsSubspacesOfAffineSpace and IsSubspacesOfAffineSpaceRep ],
	function( vs )
		Print("<",TypesOfElementsOfIncidenceStructurePlural(vs!.geometry)[vs!.type]," of ");
		ViewObj(vs!.geometry);
		Print(">");
	end );

InstallMethod( PrintObj, 
	[ IsSubspacesOfAffineSpace and IsAllSubspacesOfProjectiveSpaceRep ],
	function( vs )
		Print("Elements( ",vs!.geometry," , ",vs!.type,")");
	end );

# special method to view an affine subspace as it is usually thought of

InstallMethod( Display, 
	[ IsSubspaceOfAffineSpace ],
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

#############################################################################
#
# User methods to construct subspaces of affine spaces.
#
#############################################################################

# CHECKED 12/03/12 jdb
# but I am unhappy with the fact that an empty v just returns [].
# 24/3/2014. I changed it now, entering [] gives an error.
# cvec/cmat change.
# changed 19/01/16 (jdb): by a change of IsPlistRep, this method gets also
# called when using a row vector, causing a problem with TriangulizeMat.
# a solution was to add IsMatrix.
#############################################################################
#O  AffineSubspace( <geom>, <v>, <m> )
# returns the subspace of <geom>, with representative <v> and subspace at infinity
# determined by <m>. 
##
InstallMethod( AffineSubspace, 
	"for a row vector and Plist",
    [IsAffineSpace, IsRowVector, IsPlistRep and IsMatrix],
	function( geom, v, m )
		local  x, n, i, gf, v2;
		gf := geom!.basefield;
		## when v is empty... 
		#if IsEmpty(v) then
		#	return [];
		#fi;
        ## dimension should be correct
        x := MutableCopyMat(m);
		if Length(v) <> geom!.dimension or Length(v) <> Length(x[1]) then
			Error("Dimensions are incompatible");
		fi;
		TriangulizeMat(x);

		## Remove zero rows. It is possible the the user
		## has inputted a matrix which does not have full rank
        n := Length(x);
		i := 0;
		while i < n and ForAll(x[n-i], IsZero) do
			i := i+1; 
		od;
		#if i = n then #this case corresponds simple with the "empty subspace at infinity", so a point must be returned.
		#	return [];
		#fi;
		x := x{[1..n-i]};
		if Length(x) = geom!.dimension then
			return geom;
		fi;   
        ## It is possible that (a) the user has entered a
		## matrix with one row, or that (b) the user has
		## entered a matrix with rank 1 (thus at this stage
		## we will have a matrix with one row).
        ## We must also compress our vector/matrix.
        ##note that IsZero([[]])=IsZero([])=true.
        if IsZero(x) then
		## return an affine point
			v2 := NewMatrix(IsCMatRep,gf,geom!.dimension,[v])[1];
			#ConvertToVectorRep(v2, gf); #useless now.
			return Wrap(geom, 1, v2);
		else
		## find transversal element
			v2 := VectorSpaceTransversalElement( geom!.vectorspace, x, v );
			v2 := NewMatrix(IsCMatRep,gf,geom!.dimension,[v2])[1];
			#ConvertToVectorRep(x, gf);
			#ConvertToMatrixRep(x, gf); #should have been ConvertToMatrixRep(v2,gf) ?
			x := NewMatrix(IsCMatRep,gf,geom!.dimension,x);
			return Wrap(geom, Length(x)+1, [v2,x]);
		fi;
	end );

# CHECKED 24/3/3014 jdb
#############################################################################
#O  AffineSubspace( <geom>, <v>  )
# returns the point in the affine space <geom> determined by <v> 
##
InstallMethod( AffineSubspace, 
	"for a row vector",
    [IsAffineSpace, IsRowVector],
	function( geom, v )
        if Length(v) <> geom!.dimension then
			Error("Dimensions are incompatible");
		fi;
		return Wrap(geom, 1, NewMatrix(IsCMatRep,geom!.basefield,geom!.dimension,[v])[1] );
	end );
	
# CHECKED 24/3/3014 jdb
#############################################################################
#O  AffineSubspace( <geom>, <v>  )
# returns the point in the affine space <geom> determined by <v> 
##
InstallMethod( AffineSubspace, 
	"for a row vector",
    [IsAffineSpace, IsCVecRep],
	function( geom, v )
        if Length(v) <> geom!.dimension then
			Error("Dimensions are incompatible");
		fi;
		return Wrap(geom, 1, v);
	end );

# CHECKED 24/3/3014 jdb
#############################################################################
#O  AffineSubspace( <geom>, <v>, <m> )
# returns the subspace of <geom>, with representative <v> and subspace at infinity
# determined by <m>. 
##
InstallMethod( AffineSubspace, 
	"for a row vector and 8-bit matrix",
    [IsAffineSpace, IsRowVector, Is8BitMatrixRep],
	function( geom, v, m )
  ## We have simply copied the code that was for IsPlistRep
		local  x, n, i, gf, v2;
		gf := geom!.basefield;
        x := MutableCopyMat(m);
		if Length(v) <> geom!.dimension or Length(v) <> Length(x[1]) then
			Error("Dimensions are incompatible");
		fi;
		TriangulizeMat(x);
        n := Length(x);
		i := 0;
		while i < n and ForAll(x[n-i], IsZero) do
			i := i+1; 
		od;
		x := x{[1..n-i]};
		if Length(x) = geom!.dimension then
			return geom;
		fi;   
        if IsZero(x) then
			v2 := NewMatrix(IsCMatRep,gf,geom!.dimension,[v])[1];
			return Wrap(geom, 1, v2);
		else
			v2 := VectorSpaceTransversalElement( geom!.vectorspace, x, v );
			v2 := NewMatrix(IsCMatRep,gf,geom!.dimension,[v2])[1];
			x := NewMatrix(IsCMatRep,gf,geom!.dimension,x);
			return Wrap(geom, Length(x)+1, [v2,x]);
		fi;
	end ); 
  
# CHECKED 24/3/3014 jdb
#############################################################################
#O  AffineSubspace( <geom>, <v>, <m> )
# returns the subspace of <geom>, with representative <v> and subspace at infinity
# determined by <m>. 
##
InstallMethod( AffineSubspace, 
	"for a row vector and 8-bit matrix",
    [IsAffineSpace, IsRowVector, IsGF2MatrixRep],
	function( geom, v, m )
    ## We have simply copied the code that was for IsPlistRep
		local  x, n, i, gf, v2;
		gf := geom!.basefield;
        x := MutableCopyMat(m);
		if Length(v) <> geom!.dimension or Length(v) <> Length(x[1]) then
			Error("Dimensions are incompatible");
		fi;
		TriangulizeMat(x);
        n := Length(x);
		i := 0;
		while i < n and ForAll(x[n-i], IsZero) do
			i := i+1; 
		od;
		x := x{[1..n-i]};
		if Length(x) = geom!.dimension then
			return geom;
		fi;   
        if IsZero(x) then
			v2 := NewMatrix(IsCMatRep,gf,geom!.dimension,[v])[1];
			return Wrap(geom, 1, v2);
		else
			v2 := VectorSpaceTransversalElement( geom!.vectorspace, x, v );
			v2 := NewMatrix(IsCMatRep,gf,geom!.dimension,[v2])[1];
			x := NewMatrix(IsCMatRep,gf,geom!.dimension,x);
			return Wrap(geom, Length(x)+1, [v2,x]);
		fi;
  end ); 
  
# ADDED 24/3/3014 jdb
#############################################################################
#O  AffineSubspace( <geom>, <v>, <m> )
# returns the subspace of <geom>, with representative <v> and subspace at infinity
# determined by <m>. 
##
InstallMethod( AffineSubspace, 
	"for a row vector and 8-bit matrix",
    [IsAffineSpace, IsCVecRep, IsCMatRep],
	function( geom, v, m )
		local  x, n, i, gf, v2;
		gf := geom!.basefield;
        x := MutableCopyMat(Unpack(m)); #cmat might give trouble with {}
		if Length(v) <> geom!.dimension or Length(v) <> Length(x[1]) then
			Error("Dimensions are incompatible");
		fi;
		TriangulizeMat(x);
        n := Length(x);
		i := 0;
		while i < n and ForAll(x[n-i], IsZero) do
			i := i+1; 
		od;
		x := x{[1..n-i]};
		if Length(x) = geom!.dimension then
			return geom;
		fi;   
        if IsZero(x) then
			v2 := NewMatrix(IsCMatRep,gf,geom!.dimension,[v])[1];
			return Wrap(geom, 1, v2);
		else
			v2 := VectorSpaceTransversalElement( geom!.vectorspace, x, v );
			v2 := NewMatrix(IsCMatRep,gf,geom!.dimension,[v2])[1];
			x := NewMatrix(IsCMatRep,gf,geom!.dimension,x);
			return Wrap(geom, Length(x)+1, [v2,x]);
		fi;
  end ); 

# added 31/7/2014 for reasons of consistency jdb
#############################################################################
#O  ObjecToElement( <geom>, <obj> )
# returns the subspace of <geom>, with representative <v> and subspace at infinity
# determined by <m> if and only if <obj> is the list [v,m].
##
InstallMethod( ObjectToElement,
	"for an affine space and an object",
	[ IsAffineSpace, IsList],
	function(as, obj)
		if Length(obj) = 2 then
			return AffineSubspace(as, obj[1], obj[2]);
		else
			Error("<obj> does not determine a subspace of <as>");
		fi;
	end );
			
# added 31/7/2014 for reasons of consistency jdb
#############################################################################
#O  ObjecToElement( <geom>, <type>, <obj> )
# returns the subspace of <geom>, with representative <v> and subspace at infinity
# determined by <m> if and only if <obj> is the list [v,m].
##
InstallMethod( ObjectToElement,
	"for an affine space and an object",
	[ IsAffineSpace, IsPosInt, IsList],
	function(as, t, obj)
		local el;
		if Length(obj) = 2 then
			el :=  AffineSubspace(as, obj[1], obj[2]);
		else
			Error("<obj> does not determine a subspace of <as>");
		fi;
		if el!.type <> t then
			Error("<obj> does not determine a subspace of <as> of given type <t>");
		else
			return el;
		fi;
	end );

# CHECKED 13/03/12 jdb
#############################################################################
#O  RandomSubspace( <as>, <d> )
# returns an affine subspace of dimension <d> in the affine space <as>
##
InstallMethod( RandomSubspace, 
	"for an affine space and a dimension",
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
		
# CHECKED 13/03/12 jdb
#############################################################################
#O  Random( <subs> )
# returns a random element in the collection <subs>
##
InstallMethod( Random, 
	"for a collection of subspaces of an affine space",
    [ IsSubspacesOfAffineSpace ],
    # chooses a random element out of the collection of subspaces of given
    # dimension of an affine space
	function( subs )
		local x;
		x := RandomSubspace( subs!.geometry, subs!.type );
		return x;
	end );
		
#############################################################################
#
#  ElementsOfIncidenceStructure, enumerators, iterators
#
#############################################################################

# CHECKED 13/03/12 jdb
#############################################################################
#O  ElementsOfIncidenceStructure( <as> )
# returns the collection of all the elements of the affine space <as> 
## 
InstallMethod( ElementsOfIncidenceStructure, 
	"for an affine space",
	[IsAffineSpace],
	function( as )
		return Objectify( NewType( ElementsCollFamily, IsAllElementsOfIncidenceStructure ),
			rec( geometry := as ) );
	end );

# CHECKED 20/03/12 jdb
#############################################################################
#O  ElementsOfIncidenceStructure( <as>, <j> )
# returns the collection of all the elements of type <j> of the affine space <as> 
## 
InstallMethod( ElementsOfIncidenceStructure, 
	"for an affine space and an integer",
	[ IsAffineSpace, IsPosInt],
	function( as, j )
		local r;
		r := Rank(as);
		if j > r then
			Error("<as> has no elements of type <j>");
		else
			return Objectify( NewType( ElementsCollFamily, IsElementsOfIncidenceStructure and
                                IsSubspacesOfAffineSpace and IsSubspacesOfAffineSpaceRep ),
								#IsAllSubspacesOfAffineSpace and IsAllSubspacesOfAffineSpaceRep),
			rec( geometry := as, type := j, size := Size(Subspaces(as!.vectorspace, j-1)) * 
                  Size(as!.basefield)^(as!.dimension - j + 1) ) );
		fi;
	end );

#############################################################################
# User friendly named operations for points, lines, planes, solids
# for affine spaces. These operations are not checking if the affine space
# really contains the elements of the asked type, since ElementsOfIncidenceStructure 
# does. 
#############################################################################

# CHECKED 13/03/12 jdb
#############################################################################
#O  Points( <as> )
# returns ElementsOfIncidenceStructure(as,1), <as> a an affine space
## 
InstallMethod( Points, 
	"for an affine space",
	[IsAffineSpace],
	function( as )
		return ElementsOfIncidenceStructure(as, 1);
	end);

# CHECKED 13/03/12 jdb
#############################################################################
#O  Lines( <as> )
# returns ElementsOfIncidenceStructure(as,2), <as> a an affine space
## 
InstallMethod( Lines, 
	"for an affine space",
	[IsAffineSpace],
	function( as )
		return ElementsOfIncidenceStructure(as, 2);
	end);

# CHECKED 13/03/12 jdb
#############################################################################
#O  Planes( <as> )
# returns ElementsOfIncidenceStructure(as,3), <as> a an affine space
## 
InstallMethod( Planes, 
	"for an affine space",
	[IsAffineSpace],
	function( as )
		return ElementsOfIncidenceStructure(as, 3);
	end);

# CHECKED 13/03/12 jdb
#############################################################################
#O  Solids( <as> )
# returns ElementsOfIncidenceStructure(as,4), <as> a an affine space
## 
InstallMethod( Solids, 
	"for an affine space",
	[IsAffineSpace],
	function( as )
		return ElementsOfIncidenceStructure(as, 4);
	end);

# CHECKED 20/03/12 jdb
#############################################################################
#O  Solids( <as> )
# returns ElementsOfIncidenceStructure(as,1), <as> a an affine space
## 
InstallMethod( Hyperplanes, 
	"for an affine space",
	[IsAffineSpace],
	function( as )
		return ElementsOfIncidenceStructure(as, as!.dimension);
	end);

# CHECKED 13/03/12 jdb
#############################################################################
#O  Size( <vs> )
# returns the number of elements in the collection <vs>
## 
InstallMethod(Size, 
	"for a collection of subspaces of an affine space",
    [IsSubspacesOfAffineSpace],
	function( vs ) 
		return vs!.size; 
	end );
    
#############################################################################
# Methods to create flags.
#############################################################################

#############################################################################
#O  FlagOfIncidenceStructure( <as>, <els> )
# returns the flag of the projective space <ps> with elements in <els>.
# the method checks whether the input really determines a flag.
##
InstallMethod( FlagOfIncidenceStructure,
	"for an affine space and list of subspaces of the affine space",
	[ IsAffineSpace, IsSubspaceOfAffineSpaceCollection ],
	function(as,els)
		local list,i,test,type,flag;
		list := Set(ShallowCopy(els));
		if Length(list) > Rank(as) then
		  Error("A flag ca at most Rank(<as>) elements");
		fi;
        test := List(list,x->AmbientGeometry(x));
        if not ForAll(test,x->x=as) then
            Error("not all elements have <as> as ambient geometry");
        fi;
		test := Set(List([1..Length(list)-1],i -> IsIncident(list[i],list[i+1])));
		if (test <> [ true ] and test <> []) then
		  Error("<els> does not determine a flag>");
		fi;
		flag := rec(geo := as, types := List(list,x->x!.type), els := list);
		ObjectifyWithAttributes(flag, IsFlagOfASType, IsEmptyFlag, false);
		return flag;
	end);

#############################################################################
#O  FlagOfIncidenceStructure( <as>, <els> )
# returns the empty flag of the projective space <ps>.
##
InstallMethod( FlagOfIncidenceStructure,
	"for an affine space and an empty list",
	[ IsAffineSpace, IsList and IsEmpty ],
	function(as,els)
		local flag;
		flag := rec(geo := as, types := [], els := []);
		ObjectifyWithAttributes(flag, IsFlagOfASType, IsEmptyFlag, true);
		return flag;
	end);

#############################################################################
# View/Print/Display methods for flags
#############################################################################

InstallMethod( ViewObj, 
	"for a flag of an affine space",
	[ IsFlagOfAffineSpace and IsFlagOfIncidenceStructureRep ],
	function( flag )
		Print("<a flag of AffineSpace(",flag!.geo!.dimension,", ",Size(flag!.geo!.basefield),")>");
	end );

InstallMethod( PrintObj,
	"for a flag of an affine space",
	[ IsFlagOfAffineSpace and IsFlagOfIncidenceStructureRep ],
	function( flag )
		PrintObj(flag!.els);
	end );

InstallMethod( Display, 
	"for a flag of an affine space",
	[ IsFlagOfAffineSpace and IsFlagOfIncidenceStructureRep ],
	function( flag )
		if IsEmptyFlag(flag) then
			Print("<empty flag of AffineSpace(",flag!.geo!.dimension,", ",Size(flag!.geo!.basefield),")>\n");
		else
			Print("<a flag of AffineSpace(",flag!.geo!.dimension,", ",Size(flag!.geo!.basefield),")> with elements of types ",flag!.types,"\n");
			Print("respectively spanned by\n");
			Display(flag!.els);
		fi;
	end );

#############################################################################
# Enumerator method(s)
#############################################################################

#############################################################################
#O  Enumerator( <trans> )
# return an Enumerator for a vector space transversal.
##
InstallMethod( Enumerator, 
	"for a vector space transversal",	
	[ IsVectorSpaceTransversal ],
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
            NumberElement := function(e,v)
				local n;
				n := Position(enumcomp,v);
				return n;
			end,
			#NumberElement := enumcomp!.NumberElement,
            #Length := e -> enumcomp!.Length ));   # silly enumerator of v.spaces doesn't always have "Length"
            Length := e -> Size(complement) ));
    return enum;
	end );

# 24/3/2014. cmat adapted.
#############################################################################
#O  Enumerator( <vs> )
# return an Enumerator for subspaces of an affine space of given type.
##
InstallMethod( Enumerator, 
	"for subspaces of an affine space",
    [ IsSubspacesOfAffineSpace ],  
	function( vs )
	## An affine subspace will be represented by a pair (vector,direction).
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
              #ConvertToVectorRep(v, f);
              return Wrap(as, 1, NewMatrix(IsCMatRep,f,as!.dimension,[v])[1]); #looks ugly, avoids an extra call.
            end,
            NumberElement := function(e, x)
              local v;
              v := Unpack(x!.obj);
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
               v := NewMatrix(IsCMatRep,f,as!.dimension,BasisVectors(Basis(enumV[k])));
               enumtrans := Enumerator( VectorSpaceTransversal(vec, Unpack(v)) );
               return Wrap(as, j, [ NewMatrix(IsCMatRep,f,as!.dimension,[enumtrans[l]])[1], v ] );
            end,
            NumberElement := function( e, x )
              local w, vw, k, enumtrans, l;
              ## Here we must first find the unique direction of x
              ## incident with x, and then find its place in the ordering.
              w :=Unpack(x!.obj[2]); #x!.obj = [cvec,cmat]        
              vw := Subspace(vec,w);
			  k := Position(enumV, vw);  
              enumtrans := Enumerator( VectorSpaceTransversal(vec, w) );
              l := Position(enumtrans, Unpack(x!.obj[1]));
              return (k-1)*classsize + l;
            end ) );
     fi;
    return enum;
 end );    

# cmat adapted.
#############################################################################
#O  Iterator( <vs> )
# iterator for affine subspaces of a given type.
##
InstallMethod( Iterator, 
	"for subspaces of an affine space",
    [IsSubspacesOfAffineSpace],  
	function( vs )
	## An affine subspace will be represented by a pair (vector,direction).
	## So for example, an affine plane x+<W> will be represented by
	## (x', proj. line)  (where x' is the transversal rep corresponding to x).
		local ps, j, vars, vec, subs, f;
		ps := vs!.geometry;
		j := vs!.type;
		vec := ps!.vectorspace;
		f := ps!.basefield;
		if j = 1 then 
			vars := List(vec, x -> AffineSubspace(ps, x));
			return IteratorList( vars );
		else
	## we need a transversal for each subspace
			if j = 2 then 
				subs := List(ElementsOfIncidenceStructure(ProjectiveSpace(ps!.dimension-1,f), 1), 
                     x -> [Unpack(x!.obj)]);
			else
				subs := List(ElementsOfIncidenceStructure(ProjectiveSpace(ps!.dimension-1,f), j-1), 
                     x -> Unpack(x!.obj));
			fi;
			vars := Union(List(subs, x -> 
                 List(VectorSpaceTransversal(vec, x), y -> AffineSubspace(ps,y,x))));
			return IteratorList( vars );
		fi;
  end );


#############################################################################
#
# Basic methods: \in (set theoretic containment for elements), 
# IsIncident, Span, Meet, IsParallel, ProjectiveCompletion
#
#############################################################################

# CHECKED 13/03/12 jdb
#############################################################################
#O  \in( <x>, <as> )
# returns true if <x> is an element of the affine space <as>
##
InstallMethod( \in, 
	"for an element of an affine space and an affine space",
	[IsSubspaceOfAffineSpace, IsAffineSpace],
	function( x, as )
		local s;
		s := x!.geo;
		return s!.dimension = as!.dimension and s!.basefield = as!.basefield;
	end );

#############################################################################
#O  \in( <x>, <y> )
# set theoretic containment for an affine space and a subspace. 
##
InstallOtherMethod( \in, 
	"for an affine space and an element of an affine space",
	[ IsAffineSpace, IsSubspaceOfAffineSpace ],
	function( x, y )
		if x = y!.geo then
			return false;
		else
			Error( "<x> is different from the ambient space of <y>" );
		fi;
	end );

# CREATED 20/3/2012 jdb
#############################################################################
#O  \in( <x>, <y> )
# returns true if <x> is contained in <y>, from the set theoretic point of view.
##
InstallMethod( \in,  
	"for two subspaces of an affine space",
	[IsSubspaceOfAffineSpace, IsSubspaceOfAffineSpace],
	function( x, y )
		local ambx, amby, typx, typy, mat, flag,
          zero, nrows, ncols, vectors, 
          nvectors, i, j, z, nzheads, row;
		ambx := x!.geo;
		amby := y!.geo;
		typx := x!.type;
		typy := y!.type;
		#set theoretic containment makes only sense if the dimension of x is at most the dimension of y.
		if typx > typy then
			return false;
		elif typx = typy then
			return x=y;
		fi;
			
		## x + A in y + B iff y-x in B and A subset of B
		# x+a in y+B for all a => (y-x) in B and A subset of B 

		if ambx!.vectorspace = amby!.vectorspace then   

		## First step: check that the translations are compatible,
		## that is, that x!.obj[1] - y!.obj[1] is in the subspace
		## spanned by "vectors"

			if typx = 1 and typy > 1 then
				return x!.obj - y!.obj[1] in Subspace(ambx!.vectorspace, y!.obj[2]);
			else 
				flag := x!.obj[1] - y!.obj[1] in Subspace(ambx!.vectorspace, y!.obj[2]);
			fi;
			if not flag then return false; fi;

		## Second step: checking that the directions are compatible.
		## Algorithm is the same as for projective spaces.
		## Note that here we will have typx, typy > 1.

			vectors := y!.obj[2];
			nvectors := typy-1;
			mat := MutableCopyMat(x!.obj[2]);
			nrows := typx - 1;

			ncols:= amby!.dimension ;
			zero:= ZeroOfBaseDomain( mat );

		# here we are going to treat "vectors" as a list of basis vectors. first
		# figure out which column is the first nonzero column for each row
			nzheads := [];
			for i in [ 1 .. nvectors ] do
				row := vectors[i];
				j := PositionNonZero( row );
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
				j := PositionNonZero( row );
				if j <= ncols then
					flag := false; break;
				fi;
			od;
      
			return flag;
		else
			Error( "type is unknown or not implemented" );
		fi;
		return false;
  end );

# CREATED 20/3/2012 jdb
#############################################################################
#O  IsIncident( <x>, <y> )
# returns true if and only if <x> is incident with <y>. Relies on set theoretic
# containment.
##
InstallMethod( IsIncident,  
		[IsSubspaceOfAffineSpace, IsSubspaceOfAffineSpace],
        function(x,y)
                return x in y or y in x;
        end );


## An affine space is a complete lattice 
## (with the empty set as bottom element).

#############################################################################
#O  Span( <x>, <y> )
# cvec/cmat: I took no risk and unpacked everything to do linear algebra.
##
InstallMethod( Span,
	"for two affine subspaces",
	[IsSubspaceOfAffineSpace, IsSubspaceOfAffineSpace],
	function( x, y )  
		local ux1, uy1, ux2, uy2, ambx, amby, typx, typy, span, temp;
		ambx := AmbientSpace(x!.geo);
		amby := AmbientSpace(y!.geo);
		typx := x!.type;
		typy := y!.type;

		## affine span of x + A and y + B is 
		##  x + <y-x, A,B>

		if not (ambx!.vectorspace = amby!.vectorspace) then
			Error("Subspaces belong to different ambient spaces");
		fi;

		if typx = 1 then 
			ux1 := Unpack(x!.obj); 
			ux2 := [];
		else 
			ux1 := Unpack(x!.obj[1]); 
			ux2 := Unpack(x!.obj[2]);
		fi;
		if typy = 1  then 
			uy1 := Unpack(y!.obj); 
			uy2 := [];
		else 
			uy1 := Unpack(y!.obj[1]); 
			uy2 := Unpack(y!.obj[2]);
		fi;  
		span := MutableCopyMat(ux2);
		Append(span, uy2); #this is the reason to unpack everything, if span would be cmat, and uy2 e.g. [], this becomes very ugly.
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
		#TriangulizeMat(span); #AffineSubspace will do this now.
		return AffineSubspace(ambx, VectorSpaceTransversalElement(ambx!.vectorspace,span,ux1), span); #makes sure cvec/cmat is used.
  end );


#############################################################################
#O  Meet( <x>, <y> )
# cvec/cmat: I took no risk and unpacked everything to do linear algebra.
##
InstallMethod( Meet, 
	"for two affine subspaces",
	[IsSubspaceOfAffineSpace, IsSubspaceOfAffineSpace],
	function( x, y )
		local ag, ux1, uy1, ux2, uy2, typx, typy, int, 
          rep, t, f, vec, rk, trans, ambx, amby, m, mat;
		ag := x!.geo;
		ambx := AmbientSpace(x);
		amby := AmbientSpace(x);
		if not (ambx!.vectorspace = amby!.vectorspace) then
			Error("Subspaces belong to different ambient spaces");
		fi;

		typx := x!.type;
		typy := y!.type; 
		f := ag!.basefield; 
		vec := ag!.vectorspace;

  ## Cases for the intersection of x + A and y + B
  ## (i) A int B = 0 => (x+A) int (y+B) is a point or empty
  ## (ii) dim(A int B) = 1 => (x+A) int (y+B) is a point or empty
  ## (iii) dim(A int B) > 1 => (x+A) int (y+B) is subspace of 
  ## dimension dim(A int B), or empty
  ## (iv) A empty or B empty => equal or empty 

	## redundant cases 
		if x = y then 
			return x;
		fi;
		# we can assume now that x<>y
		if typx = 1 and typy = 1 then
			return [];
		elif (typx = 1 or typy = 1) then
			if typx = 1 then
				ux1 := Unpack(x!.obj); 
			else
				ux1 := Unpack(x!.obj[1]); 
				mat := Unpack(x!.obj[2]);
			fi;
			if typy = 1 then
				uy1 := Unpack(y!.obj);
				mat := [ ]; 
			else
				uy1 := Unpack(y!.obj[1]); 
				mat := Unpack(y!.obj[2]);
			fi;
						
			if IsZero( VectorSpaceTransversalElement( vec, mat, ux1 - uy1) ) then
				return Minimum(x, y);
			else 
			    return [];
			fi;
		fi;

		ux1 := Unpack(x!.obj[1]); 
		ux2 := Unpack(x!.obj[2]);
		uy1 := Unpack(y!.obj[1]); 
		uy2 := Unpack(y!.obj[2]);
	## parallel spaces, case x=y is handled above.
		if ux2 = uy2 then 
			return []; 
		fi;
	
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
				return AffineSubspace( ag, rep);
			fi;
			return AffineSubspace( ag, rep, int);
		else   ## case (i)
			rep := 0;
			for t in vec do
				if not IsZero(t) and Rank(Union([t-ux1], ux2)) = Rank(ux2) and
					Rank(Union([t-uy1], uy2)) = Rank(uy2) then
					rep := t; break;
				fi;
			od;
			if rep = 0 then 
				return [];
			else
				return AffineSubspace( ag, rep);
			fi;
		fi;
	end );


# CHECKED 20/3/2012 jdb
#############################################################################
#O  IsParallel( <x>, <y> )
# returns true if and only if <x> is parallel with <y>. 
##
#InstallMethod( IsParallel, 
#	"for two affine subspaces",
#	[ IsSubspaceOfAffineSpace, IsSubspaceOfAffineSpace ],
#	function( a, b );
#		if a!.type <> b!.type then
#			Error("Subspaces must be of the same dimension");
#		fi;
#		if a!.geo <> a!.geo then
#			Error("Ambient affine spaces must be the same");
#		fi;
#		if a!.type = 1 then
#			return true;
#		else
#			return a!.obj[2] = b!.obj[2];
#		fi;
#	end );

# WRITTEN 12/9/2014 jb
# Two affine subspaces of possibly different dimensions are parallel if
# and only if the direction of one contains the direction of the other.

InstallMethod( IsParallel, 
	"for two affine subspaces",
	[ IsSubspaceOfAffineSpace, IsSubspaceOfAffineSpace ],
	function( a, b )
		local vectors, nvectors, mat, nrows, ncols, zero, row, i, j, nzheads, z, flag, x, y;
		if a!.geo <> b!.geo then
			Error("Ambient affine spaces must be the same");
		fi;
		if a!.type = 1 or b!.type = 1 then
			return true;
		fi;
		
		## checking that the directions are incident.
		## Algorithm is the same as for projective spaces.
		## Note that here we will have typx, typy > 1.
		x := SortedList([a,b])[1];
		y := SortedList([a,b])[2];
		flag := true;
		vectors := y!.obj[2];
		nvectors := y!.type-1;
		mat := MutableCopyMat(x!.obj[2]);
		nrows := x!.type - 1;
		ncols:= y!.geo!.dimension ;
		zero:= ZeroOfBaseDomain( mat );

		# here we are going to treat "vectors" as a list of basis vectors. first
		# figure out which column is the first nonzero column for each row
		nzheads := [];
		for i in [ 1 .. nvectors ] do
			row := vectors[i];
			j := PositionNonZero( row );
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
			j := PositionNonZero( row );
			if j <= ncols then
				flag := false; break;
			fi;
		od;
      
		return flag;
	end );
	

#############################################################################
#O  ProjectiveCompletion( <x>, <y> )
# geometry morphism. Usual unpack to avoid potential problems.
# to be checked later.
##
InstallMethod( ProjectiveCompletion, 
	"for an affine space",
	[ IsAffineSpace ],
	function( as )
    # Returns an embedding of an affine space
    # into a projective space (its projective completion). 
    # For example, the point (x, y, z) goes to <(1, x, y, z)>    
    # An intertwiner is unnecessary, CollineationGroup(as) is 
    # subgroup of CollineationGroup(ps).

    local d, gf, ps, func, pre, map, morphism, vec, one, hom, infinity;
    d := as!.dimension;
    gf := as!.basefield;
    ps := ProjectiveSpace(d, gf);
    one := One(gf);    
    vec := as!.vectorspace;
 	infinity := VectorSpaceToElement(ps,ShallowCopy(IdentityMat(d+1,gf)){[2..d+1]});
	func := function( x )
		local repx, subspace, n, trans, new, i, j;
		repx := x!.obj;
		n := x!.type;
		if not x in as then 
			Error("Subspace is not an element of the domain (affine space)");
		fi;
		if n > 1 then      
			subspace := Unpack(repx[2]);
			trans := Unpack(repx[1]);
			# simply put all vectors together and put 0's in the first column
			new := NullMat(n, d+1, gf);
			new{[1..n-1]}{[2..d+1]} := subspace;
			new[n][1] := one;
			new[n]{[2..d+1]} := trans; 
		else
			new := Concatenation([one], Unpack(repx)); 
		fi;
		return VectorSpaceToElement(ps, new);
    end;
 
    pre := function( y )
		local n, repy, subspace, trans, new, zerov, elm, hyp;
		if not y in ps then 
			Error("Subspace is not an element of the range (projective space)");
		fi;
		if y * infinity then
			Error("Subspace is an element at infinity");
		fi;
		n := y!.type;
		repy := y!.obj;
		if n > 1 then
			repy := List(repy,x->Unpack(x));
			zerov := NullMat(1,d,gf);
			hyp := TransposedMat(Concatenation(zerov, IdentityMat(d, gf)));
			subspace := SumIntersectionMat(hyp, repy)[2];
#			repy := SortedList(y!.obj);  ## JB: 11/09/2014 (found the bug here)
			repy := SortedList(repy);
			# Make use of lexicographic ordering
			# Zero at front gives the parallel class       
			trans := repy[n];
      
			# curtail
			trans := trans{[2..d+1]};
			subspace := subspace{[1..n-1]}{[2..d+1]}; 
			new := VectorSpaceTransversalElement(vec, subspace, trans);
			elm := AffineSubspace(as, new, subspace);
		else
			trans := repy{[2..d+1]};
			elm := AffineSubspace(as, trans);
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
#############################################################################


#############################################################################
#O  ShadowOfElement( <as>, <v>, <j> )
# Returns the elements of type j incidence with <v>, a usual shadow object 
# in FinInG.
##
InstallMethod( ShadowOfElement, 
	"for an affine space, an affine subspace, a positive integer",
	[IsAffineSpace, IsSubspaceOfAffineSpace, IsPosInt],
	function( as, v, j )   
        if not AmbientGeometry(v) = as then
            Error("Ambient geometry of <v> is not <as>");
        fi;
        return Objectify(
			NewType( ElementsCollFamily, IsElementsOfIncidenceStructure and
                                   IsShadowSubspacesOfAffineSpace and
                                   IsShadowSubspacesOfAffineSpaceRep),
					rec( geometry := as, type := j, list := [v] ) );
	end );
  

#############################################################################
#O  ShadowOfFlag( <as>, <v>, <j> )
# Returns the elements of type j incidence with a flag, a usual shadow object 
# in FinInG.
##
InstallMethod( ShadowOfFlag, 
	"for an affine space, a flag of affine elements, a positive integer",
	[IsAffineSpace, IsFlagOfIncidenceStructure, IsPosInt],
	function( as, flag, j )
        #   empty flag - return all subspaces of the right type
		if IsEmptyFlag(flag) then
			return ElementsOfIncidenceStructure(as, j);
		fi;
		return Objectify(
			NewType( ElementsCollFamily, IsElementsOfIncidenceStructure and
                                   IsShadowSubspacesOfAffineSpace and
                                   IsShadowSubspacesOfAffineSpaceRep),
				rec( geometry := as, type := j, list := flag!.els ) #JDB: added !.els here (flags used to be lists, are objects now).
					);
	end);
  
#############################################################################
#O  ParallelClass( <as>, <v> )
# returns the collection of elements parallel with <v>
##
InstallMethod( ParallelClass, 
	"for an affine space and subspace",
	[IsAffineSpace, IsSubspaceOfAffineSpace], 
	function( as, v )
		#if v!.type = 0 then
		#	Error("Subspace must be nontrivial"); #this never occurs, it is not possible te constuct elements of type 0.
		if v!.type = 1 then
			return Points( as );     
		else
			return Objectify(
				NewType( ElementsCollFamily, IsElementsOfIncidenceStructure and
                                   IsParallelClassOfAffineSpace and
                                   IsParallelClassOfAffineSpaceRep),
						rec( geometry := as, element := v, type := v!.type ) );
                        # Added the type to the parallelclass 
                        # ml 12/09/2014
		fi;      
	end );
  
#############################################################################
#O  ParallelClass( <v> )
# returns the collection of elements parallel with <v>
##
InstallMethod( ParallelClass, 
	"for an affine subspace", 
	[ IsSubspaceOfAffineSpace ], 
	x -> ParallelClass( x!.geo, x ) );


#############################################################################
#O  Iterator( <pclass> )
# iterator for a parallel class of an element of an affine space.
##
InstallMethod( Iterator, 
	"for a parallel class of an affine space",
	[IsParallelClassOfAffineSpace and IsParallelClassOfAffineSpaceRep ],
	function( pclass )
		local as, v, type, direction, vec, elms;
		as := pclass!.geometry;
		v := pclass!.element;
		type := v!.type;
		direction := Unpack(v!.obj[2]); #a parallel class of a point is not in IsParallelClassOfAffineSpace(Rep)
		vec := as!.vectorspace;
        ## Do the trivial cases:
		#if type = 1 then #cannot occur
        ## it already has an iterator...
        #return Iterator( pclass );
        #fi;
		elms := List(VectorSpaceTransversal(vec, direction), y -> AffineSubspace(as, y,direction) );
		return IteratorList( elms ); 
	end );


#############################################################################
#O  Size( <vs> )
# number of elements in a shadow.
##
InstallMethod( Size, 
	"for a shadow of an element of an affine subspace",
	[IsShadowSubspacesOfAffineSpace and IsShadowSubspacesOfAffineSpaceRep ],
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


#############################################################################
#O  Iterator( <shadow> )
# iterator for a shadow of an element in an affine space.
##
InstallMethod( Iterator, 
	"for a shadow in an affine space",
	[IsShadowSubspacesOfAffineSpace and IsShadowSubspacesOfAffineSpaceRep ],
	function( vs )
		local as, i, j, ps, map, iter, list, dim, hyperplane, x, newfinish, assoc;
		as := vs!.geometry;
		j := vs!.type;
		map := ProjectiveCompletion(as);
		ps := Range(map)!.geometry;
		list := vs!.list;  
		dim := ps!.dimension+1;
		hyperplane := VectorSpaceToElement(ps, IdentityMat(dim,ps!.basefield){[2..dim]});
		if Size( list ) = 1 then
			x := list[1];
			i := x!.type;
			iter := StructuralCopy(Iterator( ShadowOfElement( ps, ImageElm(map, x), j) ));
			#
			#  We simple change the IsDoneIterator in iter.
			#  It took me ages to figure out how this all works!
			# 
			newfinish := Maximum(  [ Binomial(i-1,j-1), Binomial(dim-i,j-i) ] );   ##JB: Happy that this works!
			assoc := iter!.S!.associatedIterator;
			assoc!.choiceiter!.IsDoneIterator := 
				iter -> iter!.pos = newfinish and IsDoneIterator(assoc!.spaceiter);
		else
			# still need to truncate the iterator of this one ...
			Print("Iterators of shadows of flags in affine spaces are not complete in this version\n");
			iter := Iterator( ShadowOfFlag( ps, ImagesSet(map, list), j) );
		fi;
		return IteratorByFunctions( rec(
									NextIterator := function(iter)
										local x;
										repeat
											x := NextIterator(iter!.S);
										until not x in hyperplane;
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


InstallMethod( ViewObj, 
	[ IsShadowSubspacesOfAffineSpace and IsShadowSubspacesOfAffineSpaceRep ],
	function( vs )
		Print("<shadow ",TypesOfElementsOfIncidenceStructurePlural(vs!.geometry)[vs!.type]," in ");
		ViewObj(vs!.geometry);
		Print(">");
	end );
  
InstallMethod( ViewObj, 
	[ IsParallelClassOfAffineSpace and IsParallelClassOfAffineSpaceRep ],
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

#############################################################################
#O  IncidenceGraph( <gp> )
# Note that computing the collineation group of a projective space is zero
# computation time. So useless to print the warning here if the group is not
# yet computed.
###
InstallMethod( IncidenceGraph,
    "for a projective space",
    [ IsAffineSpace ],
    function( as )
        local elements, graph, adj, coll, sz;
		if IsBound(as!.IncidenceGraphAttr) then
            return as!.IncidenceGraphAttr;
        fi;
		coll := CollineationGroup(as);
		elements := Concatenation(List([1..Rank(as)], i -> List(AsList(ElementsOfIncidenceStructure(as,i)))));
        adj := function(x,y)
            if x!.type <> y!.type then
                return IsIncident(x,y);
            else
                return false;
            fi;
        end;
        graph := Graph(coll,elements,OnAffineSubspaces,adj,true);
        Setter( IncidenceGraphAttr )( as, graph );
        return graph;
    end );




