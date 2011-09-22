#############################################################################
##
##  liegeometry.gi            FinInG package
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
##  Copyright 2011	Colorado State University, Fort Collins
##					Università degli Studi di Padova
##					Universeit Gent
##					University of St. Andrews
##					University of Western Australia, Perth
##                  Vrije Universiteit Brussel
##                 
##
##  Implementation stuff for Lie geometries.
##
#############################################################################

########################################
#
# Things To Do:
#
# - Consider putting in more Lie incidence geometries
#   parapolar spaces, E6, E7, E8, etc, dual polar spaces?
########################################

#############################################################################
# General methods.
#############################################################################

# CHECKED 17/04/11 jdb
#############################################################################
#O  Wrap( <geo>, <type>, <o> )
# general method that just wraps the data (<o>) as an element of a Lie geometry.
# does not do any checking. <geo> and <type> are obviously also arguments. 
# Not for users, not documented.
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
#O  ElementToVectorSpace( <x> )
# User version of Unwrap, for elements of Lie geometries.
##
InstallMethod( ElementToVectorSpace, 
	"for an element of a LieGeometry",
	[IsElementOfLieGeometry],
	function( x )
		return Unwrap(x);
	end );

#############################################################################
# containment for Lie geometries:
#############################################################################
## overload "in" to mean inclusion.
## in nice geometries like projective spaces and polar spaces, it makes sense
## to install methods also for ElementsOfIncidenceStructure, IsEmptySubspace 
## and maybe even the whole space.

# CHECKED 17/04/11 jdb
# this makes me unhappy now. \in is more general than IsIncident. 
# commented out 20/09/2011. I must say that this method got installed twice now. 
# what a shame of me... jdb. BTW: new method for \in decreases the unhappiness.
#############################################################################
#O  \in( <a>, <b> )
# set theoretic containment for elements of a Lie geometry. 
##
#InstallMethod( \in, 
#	"for two IsElementOfLieGeometry",
#	[IsElementOfLieGeometry, IsElementOfLieGeometry],
#	function( a, b )
#		return IsIncident(b, a) and (a!.type <= b!.type); #made a little change here
#	end );	#to let in correspond with set theoretic containment. jdb 8/2/9
#			#During a nice afternoon in Vicenza back enabled. jdb and pc, 11/411


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
# Generic methods for the emptysubspace of Lie geometries.
#############################################################################

#InstallMethod( EmptySubspace, "for a projective space",
# [IsProjectiveSpace],
# function( pg )
#   local  vs,x,w,ty;
#	 vs:=UnderlyingVectorSpace(pg);
#     x := ShallowCopy(Zero(vs));
#	 w := rec( geo := pg, obj := x );
#	 ty:= NewType( NewFamily("EmptySubspaceFamily"), IsEmptySubspace and IsEmptySubspaceRep );
#    ObjectifyWithAttributes( w, ty, 
#						AmbientSpace, pg,
#						ProjectiveDimension, -1);
#    return w;
#end );

#InstallMethod( EmptySubspace, "for a polar space",
# [IsClassicalPolarSpace],
# function( pg )
#   local  vs,x,w,ty;
#	 vs:=UnderlyingVectorSpace(pg);
#     x := ShallowCopy(Zero(vs));
#	 w := rec( geo := pg, obj := x );
#	 ty:= NewType( NewFamily("EmptySubspaceFamily"), IsEmptySubspace and IsEmptySubspaceRep );
#    ObjectifyWithAttributes( w, ty, 
#						AmbientSpace, pg,
#						ProjectiveDimension, -1);
#    return w;
#end );

# CHECKED 7/09/2011 jdb
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
		x := ShallowCopy(Zero(vs));
		w := rec( geo := g, obj := x );
		ty:= NewType( NewFamily("EmptySubspaceFamily"), IsEmptySubspace and IsEmptySubspaceRep );
		ObjectifyWithAttributes( w, ty, AmbientSpace, g, ProjectiveDimension, -1);
		return w;
	end );

# 3 CHECKED 7/09/2011 jdb
#############################################################################
# View, PRint and Display for IsEmptySubspace
#############################################################################

InstallMethod( ViewObj, [IsEmptySubspace],
  function(x)
    Print("< trivial subspace >");
  end );
  
InstallMethod( PrintObj, [IsEmptySubspace],
  function(x)
    PrintObj(Zero(UnderlyingVectorSpace(AmbientSpace(x))));
  end );

InstallMethod( Display, [IsEmptySubspace],
  function(x)
    Print("< trivial subspace >");
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
#############################################################################
#O  \^( <e>, <u> )
# unwrapping the empty subspace
## 
InstallMethod( \^, "unwrapping an empty subspace",
  [ IsEmptySubspace, IsUnwrapper ],
  function( e, u )
    return [];
  end );

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
# for two trivial subspaces. Returns true if the vectorspace of their ambient
# spaces is the same.
## 
InstallOtherMethod( \in, 
	"for a trivial subspace and a trivial subspace", 
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
# for the trivial subspace and a non trivial one. Returns true if the 
# vectorspace of their ambient spaces is the same.
## 
InstallOtherMethod( \in, 
	"for the trivial subspace and an element of a Lie geometry", 
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
# for a non trivial subspace and the trivial one. Returns true if the 
# vectorspace of their ambient spaces is the same.
## 
InstallOtherMethod( \in, 
	"for an element of a Lie geometry and the trivial subspace", 
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
# for the trivial subspace and a Lie geometry. Returns true if the vectorspace 
# of <x> is the same as the vectorspace of the geomery.
# Remark that we do not implement a generic method to check if a particular 
# Lie geometry is contained in the empty subspace. If this is desired, e.g. for
# projective spces, it should be implemented for this particular space.
## 
InstallOtherMethod( \in, 
	"for the trivial subspace and a Lie geometry", 
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
# for the trivial subspace and an element of a Lie geometry. Returns <y> if
# both arguments' ambient space has the same vectorspace
##
InstallMethod( Span, 
	"for the trivial subspace and an element of a Lie geometry", 
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
# for an element of a Lie geometry and the trivial subspace. Returns <y> if
# both arguments' ambient space has the same vectorspace
##
InstallMethod( Span, 
	"for an element of a Lie geometry and the trivial subspace", 
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
# for the trivial subspace and the trivial subspace. Returns <x> if
# both arguments' ambient space has the same vectorspace
##
InstallMethod( Span, 
	"for the trivial subspace and the trivial subspace",
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
# for the trivial subspace and the trivial subspace. Returns <x> if
# both arguments' ambient space has the same vectorspace.
##
InstallMethod( Meet, 
	"for the trivial subspace and a projective subspace", 
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
# for an element of a Lie geometry and the trivial subspace. Returns <y> if
# both arguments' ambient space has the same vectorspace
##
InstallMethod( Meet, 
	"for the trivial subspace and a projective subspace", 
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
# for the trivial subspace and the trivial subspace. Returns <x> if
# both arguments' ambient space has the same vectorspace
##
InstallMethod( Meet, 
	"for the trivial subspace and the trivial subspace",
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
#O  ElementsIncidentWithElementOfIncidenceStructure( <el>, <i> )
# returns the elements of type <i> in <el>, relying on ShadowOfElement 
# for particular <el>.
## 
InstallMethod( ElementsIncidentWithElementOfIncidenceStructure, "for IsElementOfLieGeometry",
	[ IsElementOfLieGeometry, IsPosInt],
	function( el, i )
		return ShadowOfElement(el!.geo, el, i);
	end );

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
InstallMethod(\in,
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

