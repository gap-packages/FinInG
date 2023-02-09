#############################################################################
##
##  liegeometry.gi            FinInG package
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
##  Implementation stuff for Lie geometries.
##
#############################################################################


#############################################################################
# General methods.
#############################################################################

# CHECKED 02/08/2014 (made general for Lie geometries) jdb
#############################################################################
#O  UnderlyingVectorSpace( <ps> )
# returns the Underlying vectorspace of the projective space <ps>
##
InstallMethod( UnderlyingVectorSpace, 
	"for a lie geometry",
	[ IsLieGeometry],
	function(ps)
		return ShallowCopy(ps!.vectorspace);
	end);

# CHECKED 02/08/2014 (made general for Lie geometries) jdb
#############################################################################
#A  ProjectiveDimension( <ps> )
# returns the projective dimension of <ps>
##
InstallMethod( ProjectiveDimension,
	"for a lie space",
	[ IsLieGeometry ],
	geom -> geom!.dimension
	);
	
# CHECKED 02/08/2014 (made general for Lie geometries) jdb
#############################################################################
#A  Dimension( <ps> )
# returns the projective dimension of <ps>
##
InstallMethod( Dimension, 
	"for a lie space",
	[ IsLieGeometry ],
	geom -> geom!.dimension
	);

# CHECKED 02/08/2014 (made general for Lie geometries) jdb
#############################################################################
#O  BaseField( <ps> )
# returns the basefield of <ps>
##
InstallMethod( BaseField, 
	"for a projective space", 
	[ IsLieGeometry ],
	geom -> geom!.basefield );

# CHECKED 02/08/2014 jdb
#############################################################################
#O  Wrap( <geo>, <type>, <o> )
# general method that just wraps the data (<o>) as an element of a Lie geometry.
# does not do any checking. <geo> and <type> are obviously also arguments. 
# Not for users, not documented. Note that currently, we have no geometries
# that are in IsLieGeometry but not in a subcategory. So this method is never
# used. 
##
InstallMethod( Wrap, 
	"for a Lie geometry and an object",
	[IsLieGeometry, IsPosInt, IsObject],
	function( geo, type, o )
		local w;
		w := rec( geo := geo, type := type, obj := o );
		Objectify( NewType( ElementsOfIncidenceStructureFamily, IsElementOfIncidenceStructure and
		IsElementOfIncidenceStructureRep and IsElementOfLieGeometry ), w );
		return w;
	end );

# CHECKED 17/04/11 jdb
#############################################################################
#O  UnderlyingObject( <x> )
# User version of Unwrap, for elements of Lie geometries.
##
InstallMethod( UnderlyingObject, 
	"for an element of a LieGeometry",
	[IsElementOfLieGeometry],
	function( x )
		return Unwrap(x);
	end );

# ADDED 28/11/11 jdb + pc
#############################################################################
#O  AmbientSpace( <subspace> ) returns the ambient space of <subspace>, an
# element of a Lie geometry.
##
InstallMethod( AmbientSpace, 
	"for an element of a Lie geometry",
	[IsElementOfLieGeometry],
	function(subspace)
		return AmbientSpace(subspace!.geo);
	end );

#############################################################################
# Viewing/Printing/Displaying methods.
#############################################################################

InstallMethod( ViewObj, "for IsAllElementsOfLieGeometry",
	[ IsAllElementsOfLieGeometry and IsAllElementsOfLieGeometryRep ],
	function( vs )
		Print("<All elements of ");
		ViewObj(vs!.geometry);
		Print(">");
	end );

InstallMethod( PrintObj, "for IsAllElementsOfLieGeometry",
	[ IsAllElementsOfLieGeometry and IsAllElementsOfLieGeometryRep ],
	function( vs )
		Print("AllElementsOfIncidenceStructure( ",vs!.geometry," )");
	end );

InstallMethod( ViewObj, "for IsAllElementsOfLieGeometry",
	[ IsElementsOfLieGeometry and IsElementsOfLieGeometryRep ],
	function( vs )
		Print("<",TypesOfElementsOfIncidenceStructurePlural(vs!.geometry)[vs!.type]," of ");
		ViewObj(vs!.geometry);
		Print(">");
	end );

InstallMethod( PrintObj, "for IsElementsOfLieGeometry",
	[ IsElementsOfLieGeometry and IsElementsOfLieGeometryRep ],
	function( vs )
		Print("ElementsOfIncidenceStructure( ",vs!.geometry," , ",vs!.type,")");
	end );

#############################################################################
# User friendly named operations for points, lines, planes, solids
# for Lie geometries. These operations are not checking if the Lie geometry
# really contains the elements of the asked type. If not, an error will be
# produced by ElementsOfIncidenceStructure. The latter method needs to be
# created for each type of Lie geometry separately.
#############################################################################

# CHECKED 18/4/2011 jdb
#############################################################################
#O  Points( <ps> )
# returns ElementsOfIncidenceStructure(ps,1), <ps> a Lie Geometry.
## 
InstallMethod( Points, "for a Lie geometry",
	[IsLieGeometry],
	function( ps )
		return ElementsOfIncidenceStructure(ps, 1);
	end);

# CHECKED 18/4/2011 jdb
#############################################################################
#O  Lines( <ps> )
# returns ElementsOfIncidenceStructure(ps,2), <ps> a Lie Geometry.
## 
InstallMethod( Lines, "for a Lie geometry",
	[IsLieGeometry],
	function( ps )
		return ElementsOfIncidenceStructure(ps, 2);
	end);

# CHECKED 18/4/2011 jdb
#############################################################################
#O  Planes( <ps> )
# returns ElementsOfIncidenceStructure(ps,3), <ps> a Lie Geometry.
## 
InstallMethod( Planes, "for a Lie geometry",
	[IsLieGeometry],
	function( ps )
		return ElementsOfIncidenceStructure(ps, 3);
	end);

# CHECKED 18/4/2011 jdb
#############################################################################
#O  Solids( <ps> )
# returns ElementsOfIncidenceStructure(ps,4), <ps> a Lie Geometry.
## 
InstallMethod( Solids, "for a Lie geometry",
	[IsLieGeometry],
	function( ps )
		return ElementsOfIncidenceStructure(ps, 4);
	end);

#############################################################################
# Generic method for the emptysubspace of Lie geometries.
#############################################################################

# CHECKED 02/08/2014 jdb
#############################################################################
#O  EmptySubspace( <g> )
# returns the empty subspace in the Lie geometry <g>
## 
InstallMethod( EmptySubspace, 
	"for a Lie geometry",
	[IsLieGeometry],
	function( g )
		local  vs,x,w,ty;
		vs:=UnderlyingVectorSpace(g);
		#x := ShallowCopy(Zero(vs));
		#x := Unpack(ShallowCopy(Zero(vs))); #Unpack is the answer to all our CVec/CMat questions. 
#This is actually a bit eaxagerated. Sometimes Zero(vs) will return a "dirty" GAP vector, then Unpack works. But sometims
#it will be a list, then Unpack does not work anymore. I don't want a case distinction here. ZeroVector (from cvec) 
#asks a cvec as argument, which is a bit inconvenient. So I do it the dirty way.
		x := NewMatrix(IsCMatRep,g!.basefield,g!.dimension+1,[Zero(ShallowCopy(vs))]);
		#w := rec( geo := g, obj := x ); #changed 19/3/2014
		w := rec( geo := g, obj := x[1] );
		ty:= NewType( NewFamily("EmptySubspaceFamily"), IsEmptySubspace and IsEmptySubspaceRep );
		#ObjectifyWithAttributes( w, ty, AmbientSpace, g, ProjectiveDimension, -1);
		ObjectifyWithAttributes( w, ty, AmbientSpace, AmbientSpace(g), ProjectiveDimension, -1);
		return w;
	end );

# Added 31/10/2012 jdb
#############################################################################
#O  BaseField( <sub> )
# returns the basefield of the empty subspace.
##
InstallMethod( BaseField, 
	"for the empty subspace of a Lie geometry", 
	[IsEmptySubspace and IsEmptySubspaceRep],
	sub -> AmbientSpace(sub)!.basefield );


# 3 CHECKED 7/09/2011 jdb
#############################################################################
# View, PRint and Display for IsEmptySubspace
#############################################################################

InstallMethod( ViewObj, [IsEmptySubspace],
  function(x)
    Print("< empty subspace >");
  end );
  
InstallMethod( PrintObj, [IsEmptySubspace],
  function(x)
    PrintObj(Zero(UnderlyingVectorSpace(AmbientSpace(x))));
  end );

InstallMethod( Display, [IsEmptySubspace],
  function(x)
    Print("< empty subspace >");
  end );

# CHECKED 7/09/2011 jdb
#############################################################################
#O  \=( <e1>, <e2> )
# returns true if <e1> and <e2> are empty spaces in the same ambient spaces.
# Remark: this method is correct, but at this very moment, I might get unhappy
# with the meaning of AmbientSpace (and its sibling AmbientGeometry). Making me
# happy again with these concepts, might change the code below later on.
## 
InstallMethod( \=, "for two empty subspaces",
        [IsEmptySubspace, IsEmptySubspace],
        function(e1,e2);
        return AmbientSpace(e1) = AmbientSpace(e2);
  end );

# CHECKED 7/09/2011 jdb
#jdb 30/10/15: This method is not used anymore after commenting out the Unwrapper stuff in geometry.gd
#see the comment there.
#############################################################################
#O  \^( <e>, <u> )
# unwrapping the empty subspace
## 
#InstallMethod( \^, "unwrapping an empty subspace",
#  [ IsEmptySubspace, IsUnwrapper ],
#  function( e, u )
#    return [];
#  end );

#############################################################################
# Methods for \in with the EmtySubspace and elements of a Lie geometry.
# Since the empty subspace is not 
# an element of an incidence geometry, we don't provide methods for IsIncident.
# This was decided on 17/04/2011 (Vicenza).
# Now I add the extra checks to see if two empty subspaces belong to the same
# space.
#############################################################################

# CHECKED 7/09/2011 jdb
#############################################################################
#O  \in( <x>, <y> )
# for two empty subspaces. Returns true if the vectorspace of their ambient
# spaces is the same.
## 
InstallOtherMethod( \in, 
	"for a empty subspace and a empty subspace", 
	[ IsEmptySubspace, IsEmptySubspace ],
	function( x, y )
		if x!.geo!.vectorspace = y!.geo!.vectorspace then
			return true;
		else
			Error( "The subspaces <x> and <y> belong to different ambient spaces" );
		fi;
	end );
  
# CHECKED 7/09/2011 jdb
#############################################################################
#O  \in( <x>, <y> )
# for the empty subspace and a non empty one. Returns true if the 
# vectorspace of their ambient spaces is the same.
## 
InstallOtherMethod( \in, 
	"for the empty subspace and an element of a Lie geometry", 
	[ IsEmptySubspace, IsElementOfLieGeometry ],
	function( x, y )
		if x!.geo!.vectorspace = y!.geo!.vectorspace then
			return true;
		else
			Error( "The subspaces <x> and <y> belong to different ambient spaces" );
		fi;
	end );
	
# CHECKED 7/09/2011 jdb
#############################################################################
#O  \in( <x>, <y> )
# for a non empty subspace and the empty one. Returns true if the 
# vectorspace of their ambient spaces is the same.
## 
InstallOtherMethod( \in, 
	"for an element of a Lie geometry and the empty subspace", 
	[ IsElementOfLieGeometry, IsEmptySubspace ],
	function( x, y )
		if x!.geo!.vectorspace = y!.geo!.vectorspace then
			return false;
		else
			Error( "The subspaces <x> and <y> belong to different ambient spaces" );
		fi;
	end );

# CHECKED 7/09/2011 jdb
#############################################################################
#O  \in( <x>, <y> )
# for the empty subspace and a Lie geometry. Returns true if the vectorspace 
# of <x> is the same as the vectorspace of the geomery.
# Remark that we do not implement a generic method to check if a particular 
# Lie geometry is contained in the empty subspace. If this is desired, e.g. for
# projective spces, it should be implemented for this particular space.
## 
InstallOtherMethod( \in, 
	"for the empty subspace and a Lie geometry", 
	[ IsEmptySubspace, IsLieGeometry ],
	function( x, y )
		if x!.geo!.vectorspace = y!.vectorspace then
			return true;
		else
			Error( "The subspace <x> has a different ambient space than <y>" );
		fi;
	end );

#############################################################################
# Span and Meet methods for the trival space and elements of Lie geometires.
# Same checks as in the \in methods.
#############################################################################

# CHECKED 7/09/2011 jdb
#############################################################################
#O  Span( <x>, <y> )
# for the empty subspace and an element of a Lie geometry. Returns <y> if
# both arguments' ambient space has the same vectorspace
##
InstallMethod( Span, 
	"for the empty subspace and an element of a Lie geometry", 
	[ IsEmptySubspace, IsElementOfLieGeometry ],
	function( x, y )
		if x!.geo!.vectorspace = y!.geo!.vectorspace then
			return y;
		else
			Error( "The subspaces <x> and <y> belong to different ambient spaces" );
		fi;
	end );

# CHECKED 7/09/2011 jdb
#############################################################################
#O  Span( <x>, <y> )
# for an element of a Lie geometry and the empty subspace. Returns <y> if
# both arguments' ambient space has the same vectorspace
##
InstallMethod( Span, 
	"for an element of a Lie geometry and the empty subspace", 
	[ IsElementOfLieGeometry, IsEmptySubspace ],
	function( x, y )
		if x!.geo!.vectorspace = y!.geo!.vectorspace then
			return x;
		else
			Error( "The subspaces <x> and <y> belong to different ambient spaces" );
		fi;
	end );

# CHECKED 7/09/2011 jdb
#############################################################################
#O  Span( <x>, <y> )
# for the empty subspace and the empty subspace. Returns <x> if
# both arguments' ambient space has the same vectorspace
##
InstallMethod( Span, 
	"for the empty subspace and the empty subspace",
	[IsEmptySubspace, IsEmptySubspace],
	function( x, y )
		if x!.geo!.vectorspace = y!.geo!.vectorspace then
			return x;
		else
			Error( "The subspaces <x> and <y> belong to different ambient spaces" );
		fi;
	end );

# CHECKED 7/09/2011 jdb
#############################################################################
#O  Meet( <x>, <y> )
# for the empty subspace and the empty subspace. Returns <x> if
# both arguments' ambient space has the same vectorspace.
##
InstallMethod( Meet, 
	"for the empty subspace and a projective subspace", 
	[ IsEmptySubspace, IsElementOfLieGeometry ],
	function( x, y )
		if x!.geo!.vectorspace = y!.geo!.vectorspace then
			return x;
		else
			Error( "The subspaces <x> and <y> belong to different ambient spaces" );
		fi;
	end );

# CHECKED 7/09/2011 jdb
#############################################################################
#O  Meet( <x>, <y> )
# for an element of a Lie geometry and the empty subspace. Returns <y> if
# both arguments' ambient space has the same vectorspace
##
InstallMethod( Meet, 
	"for the empty subspace and a projective subspace", 
	[ IsElementOfLieGeometry, IsEmptySubspace ],
	function( x, y )
		if x!.geo!.vectorspace = y!.geo!.vectorspace then
			return y;
		else
			Error( "The subspaces <x> and <y> belong to different ambient spaces" );
		fi;
	end );

# CHECKED 7/09/2011 jdb
#############################################################################
#O  Meet( <x>, <y> )
# for the empty subspace and the empty subspace. Returns <x> if
# both arguments' ambient space has the same vectorspace
##
InstallMethod( Meet, 
	"for the empty subspace and the empty subspace",
	[IsEmptySubspace, IsEmptySubspace],
	function( x, y )
		if x!.geo!.vectorspace = y!.geo!.vectorspace then
			return x;
		else
			Error( "The subspaces <x> and <y> belong to different ambient spaces" );
		fi;
	end );

#############################################################################
#
# Nice shortcuts to ShadowOfElement. Remind that shadow functionality 
# is to be implemented for the particular Lie geometries. See corresponding
# files. Very important: although we see functions here for Lie geometries
# where one could expect "containment" to be more in use than "incidence"
# the methods for Points, etc. really refer to "is incident with" than to
# is contained in. So these are really shortcuts to ElementsIncidentWithElementOfIncidenceStructure.
# From that point of view, we do not implement shadow functionaly for the empty subspace.
#
# These operations are user friendly and documented. But be carefull, the version
# with two arguments, <geo> and <el> can be used to "convert" elements created
# a Lie geometry, say a quadric, into the Lie geometry <geo>, e.g. the ambient
# projective space. Since these are generic methods for Lie geometries and 
# their elements, the responsability of checking that such is conversion is possible
# must be done in the methods for the operation ShadowOfElement. This is a decision
# of the developpers (or at least the dictator among them). 7/9/11 jdb.
#
#############################################################################


# CHECKED 19/4/2011 jdb
#############################################################################
#O  Points( <el> )
# returns the points, i.e. elements of type <1> in <el>, relying on ShadowOfElement 
# for particular <el>.
## 
InstallMethod( Points, "for IsElementOfLieGeometry",
	[ IsElementOfLieGeometry ],
	function( var )
		return ShadowOfElement(var!.geo, var, 1);
	end );

# CHECKED 19/4/2011 jdb
#############################################################################
#O  Points( <geo>, <el> )
# returns the points, i.e. elements of type <1> in <el>, all in <geo> 
# relying on ShadowOfElement for particular <geo> and <el>.
##
InstallMethod( Points, "for IsLieGeometry and IsElementOfLieGeometry", 
	[ IsLieGeometry, IsElementOfLieGeometry ],
	function( geo, var )
		return ShadowOfElement(geo, var, 1);
	end );

# CHECKED 19/4/2011 jdb
#############################################################################
#O  Lines( <el> )
# returns the lines, i.e. elements of type <2> in <el>, relying on ShadowOfElement 
# for particular <el>.
## 
InstallMethod( Lines, "for IsElementOfLieGeometry", 
	[ IsElementOfLieGeometry ],
	function( var )
		return ShadowOfElement(var!.geo, var, 2);
	end );

# CHECKED 19/4/2011 jdb
#############################################################################
#O  Lines( <geo>, <el> )
# returns the points, i.e. elements of type <2> in <el>, all in <geo> 
# relying on ShadowOfElement for particular <geo> and <el>.
##
InstallMethod( Lines, "for IsLieGeometry and IsElementOfLieGeometry", 
	[ IsLieGeometry, IsElementOfLieGeometry ],
	function( geo, var )
		return ShadowOfElement(geo, var, 2);
	end );

# CHECKED 19/4/2011 jdb
#############################################################################
#O  Planes( <el> )
# returns the lines, i.e. elements of type <3> in <el>, relying on ShadowOfElement 
# for particular <el>.
## 
InstallMethod( Planes, "for IsElementOfLieGeometry", 
	[ IsElementOfLieGeometry ],
	function( var )
		return ShadowOfElement(var!.geo, var, 3);
	end );

# CHECKED 19/4/2011 jdb
#############################################################################
#O  Planes( <geo>, <el> )
# returns the points, i.e. elements of type <2> in <el>, all in <geo> 
# relying on ShadowOfElement for particular <geo> and <el>.
##
InstallMethod( Planes, "for IsLieGeometry and IsElementOfLieGeometry", 
	[ IsLieGeometry, IsElementOfLieGeometry ],
	function( geo, var )
		return ShadowOfElement(geo, var, 3);
	end );

# CHECKED 19/4/2011 jdb
#############################################################################
#O  Solids( <el> )
# returns the lines, i.e. elements of type <3> in <el>, relying on ShadowOfElement 
# for particular <el>.
##
InstallMethod( Solids, [ IsElementOfLieGeometry ],
  function( var )
    return ShadowOfElement(var!.geo, var, 4);
  end );

# CHECKED 19/4/2011 jdb
#############################################################################
#O  Solids( <geo>, <el> )
# returns the points, i.e. elements of type <2> in <el>, all in <geo> 
# relying on ShadowOfElement for particular <geo> and <el>.
##
InstallMethod( Solids, "for IsElementOfLieGeometry", 
	[ IsLieGeometry, IsElementOfLieGeometry ],
	function( geo, var )
		return ShadowOfElement(geo, var, 4);
	end );

# ADDED 22/12/2011 jdb
#############################################################################
#O  Hyperplanes( <el> )
# returns the hyperplanes contained in <el>, relying on ShadowOfElement 
# for particular <el>.
##
InstallMethod( Hyperplanes, 
	"for elements of a Lie geometry",
	[ IsElementOfLieGeometry ],
	function( var )
		if var!.type = 1 then
			Error("There are no elements of type 0");
		fi;
		return ShadowOfElement( var!.geo, var, var!.type - 1 );
  end );

# ADDED 22/12/2011 jdb
#############################################################################
#O  Hyperplanes( <geo>, <el> )
# returns the hyperplanes incident with <el>, relying on ShadowOfElement 
# for particular <el>.
##
InstallMethod( Hyperplanes, 
	"for a Lie geometry and elements of a Lie geometry",
	[ IsLieGeometry, IsElementOfLieGeometry ],
	function( geo, var )
		return ShadowOfElement( geo, var, geo!.dimension );
end );

#############################################################################
# Finally a generic ViewObj method for shadow elements.
#############################################################################
#CHECKED 7/9/2011 jdb
##
InstallMethod( ViewObj,
	"for shadow elements of a Lie geometry", 
	[ IsShadowElementsOfLieGeometry and IsShadowElementsOfLieGeometryRep ],
	function( vs )
		Print("<shadow ",TypesOfElementsOfIncidenceStructurePlural(vs!.geometry)[vs!.type]," in ");
		ViewObj(vs!.geometry);
		Print(">");
	end );


#############################################################################
# A method to check set theoretic containment for elements of a Lie geometry.
#############################################################################


# CREATED 13/9/2011 jdb
#############################################################################
#O  \in( <x>, <y> )
# returns true if <x> is contained in <y>, from the set theoretic point of view.
##
InstallMethod( \in,
	"for two elements of a Lie geometry",
	[IsElementOfLieGeometry,   IsElementOfLieGeometry],
## some of this function is based on the
## SemiEchelonMat function. we save time by assuming that the matrix of
## each subspace is already in semiechelon form.
## method only applies to projective and polar spaces
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
    
        if typx > typy then
		  return false;
        else
          vectors := y!.obj;
          nvectors := typy;
          mat := x!.obj;
          nrows := typx;
        fi;
      # subspaces of type 1 need to be nested to make them lists of vectors

      if nrows = 1 then mat := [mat]; fi;
      if nvectors = 1 then vectors := [vectors]; fi;

      mat := MutableCopyMat(mat);
      ncols:= amby!.dimension + 1;
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

        # if the row is now not zero then y is not a subvariety of x
        j := PositionNonZero( row );
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


#############################################################################
# A method to check set theoretic containment for an element of a Lie geometry
# and a collection of shadow elements of a Lie geometry
#############################################################################

# new since 5/4/2018 jdb
# see also remarks at method O  \in( <x>, <dom> )
# "for an element and collection of shadow elements of an incidence structure"
#############################################################################
#O  \in( <x>, <dom> )
# returns true if <x> belongs to the elements collected in <dom> It is checked if their
# geometry matches.
##
InstallMethod( \in,
    "for an element and set of shadow elements of a Lie geometry",
    # 1*SUM_FLAGS+3 increases the ranking for this method
    # 5/4/2018: jdb wonders if the above line is necessary.
    [IsElementOfLieGeometry, IsShadowElementsOfLieGeometry],
    1*SUM_FLAGS+3,
    function( x, dom )
        return x in dom!.geometry and x!.type = dom!.type and IsIncident(x,dom!.parentflag);
    end );
#############################################################################
## Methods for random selection of elements
#############################################################################  

# CHECKED 14/09/2011 jdb.
#############################################################################
#O  Random( <subs> )
# <subs> must be a collections of subspaces of a vector space, of the same 
# dimension. Returns a random element from it.
##
InstallMethod( Random, 
	"for a collection of subspaces of a vector space",
    [ IsSubspacesVectorSpace ],
	# chooses a random element out of the collection of subspaces of given dimension of a vectorspace
	function( subs )
		local d, vspace, V, W, w;
		## the underlying vector space
		vspace := subs!.structure;
		if not IsInt(subs!.dimension) then #here we must know that this field is "all" if it is not an integer.
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

# CHECKED 14/09/2011 jdb.
#############################################################################
#O  RandomSubspace( <vspace>, <d> )
# <vspace> is a vector space, <d> a dimension. Returns a random subspace of 
# dimension <d>
##
InstallMethod( RandomSubspace,
	"for a vectorspace and a dimension",
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

#############################################################################
# Converting element methods (added 28/11/11)
#############################################################################

# Added 28/11/2011 jdb.
# Repaired 21/6/16
#############################################################################
#O  ElementToElement( <geo>, <el> )
# returns the element VectorSpaceToElement(<geo>,UnderlyingObject(<el>))
##
InstallMethod( ElementToElement,
	"for a Lie geometry and an element of a Lie geometry",
	[IsLieGeometry, IsElementOfLieGeometry],
	function(ps,el)
		return VectorSpaceToElement(ps,Unpack(UnderlyingObject(el))); #here was a bug, there was no unpack, and there should be.
	end );

#############################################################################
# the two methods below are not yet finished, and commented out
#############################################################################

# Added 28/11/2011 jdb.
#############################################################################
#O  ConvertElement( <geo>, <el> )
# chages the element <el> to VectorSpaceToElement(<geo>,UnderlyingObject(<el>))
# with a check whether this is possible.
##
#InstallMethod( ConvertElement,
#	"for a Lie geometry and an element of a Lie geometry",
#	[IsProjectiveSpace, IsElementOfLieGeometry],
#	function(ps,el)
#		if el in ps then
#			el!.geo := ps;
#		else
#			Error( "<el> cannot be converted to an element of <ps>");
#		fi;
#		
#	end );

# Added 28/11/2011 jdb.
#############################################################################
#O  ConvertElementNC( <geo>, <el> )
# chages the element <el> to VectorSpaceToElement(<geo>,UnderlyingObject(<el>))
# *without* checks. Please use with extreme care. Belgium might get a government if you use
# this function when you shouldn't.
##
#InstallMethod( ConvertElementNC,
#	"for a Lie geometry and an element of a Lie geometry",
#	[IsLieGeometry, IsElementOfLieGeometry],
#	function(ps,el)
#		el!.geo := ps;
#	end );


#############################################################################
# elements of Lie geometries are always constructed using a spaning set for
# the underlying sub vector space. OjectToElement for Lie geometries simply 
# pipes the object to VectorSpaceToElement, which will check whether the
# object is suitable or not.
#############################################################################

# Added 31/07/2014 jdb.
#############################################################################
#O  ObjectToElement( <geo>, <type>, <obj> )
# returns the element VectorSpaceToElement(<geo>, <obj>)), and checks whether
# the type is correct.
#
InstallMethod( ObjectToElement,
	"for a Lie geometry, and integer, and an object",
	[IsLieGeometry, IsPosInt, IsObject],
	function(geom, type, obj)
		local el;
		el := VectorSpaceToElement(geom,obj);
		if type <> el!.type then
			Error("<obj> represents an element of a different type than requested");
		else
			return el;
		fi;
	end );

# Added 31/07/2014 jdb.
#############################################################################
#O  ObjectToElement( <geo>, <obj> )
# returns the element VectorSpaceToElement(<geo>, <obj>))
#
InstallMethod( ObjectToElement,
	"for a Lie geometry, and integer, and an object",
	[IsLieGeometry, IsObject],
	function(geom, obj)
		local el;
		return VectorSpaceToElement(geom,obj);
	end );

