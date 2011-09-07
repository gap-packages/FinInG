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
#
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
#############################################################################
#O  \in( <a>, <b> )
# set theoretic containment for elements of a Lie geometry. 
##
InstallMethod( \in, 
	"for two IsElementOfLieGeometry",
	[IsElementOfLieGeometry, IsElementOfLieGeometry],
	function( a, b )
		return IsIncident(b, a) and (a!.type <= b!.type); #made a little change here
	end );	#to let in correspond with set theoretic containment. jdb 8/2/9
			#During a nice afternoon in Vicenza back enabled. jdb and pc, 11/411


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
# for Lie geometries.
#############################################################################

# CHECKED 18/4/2011 jdb
#############################################################################
#O  Points( <ps> )
# returns ElementsOfIncidenceStructure(ps,1), <ps> a Lie Geometry.
## 
InstallMethod( Points, "for IsLieGeometry",
	[IsLieGeometry],
	function( ps )
		return ElementsOfIncidenceStructure(ps, 1);
	end);

# CHECKED 18/4/2011 jdb
#############################################################################
#O  Lines( <ps> )
# returns ElementsOfIncidenceStructure(ps,2), <ps> a Lie Geometry.
## 
InstallMethod( Lines, "for IsLieGeometry",
	[IsLieGeometry],
	function( ps )
		return ElementsOfIncidenceStructure(ps, 2);
	end);

# CHECKED 18/4/2011 jdb
#############################################################################
#O  Planes( <ps> )
# returns ElementsOfIncidenceStructure(ps,3), <ps> a Lie Geometry.
## 
InstallMethod( Planes, "for IsLieGeometry",
	[IsLieGeometry],
	function( ps )
		return ElementsOfIncidenceStructure(ps, 3);
	end);

# CHECKED 18/4/2011 jdb
#############################################################################
#O  Solids( <ps> )
# returns ElementsOfIncidenceStructure(ps,4), <ps> a Lie Geometry.
## 
InstallMethod( Solids, "for IsLieGeometry",
	[IsLieGeometry],
	function( ps )
		return ElementsOfIncidenceStructure(ps, 4);
	end);

# CHECKED 18/4/2011 jdb
# I will move this piece to projectivespace.gi.
InstallMethod( Hyperplanes, "for IsProjectiveSpace",
	[IsProjectiveSpace],
	function( ps )
		return ElementsOfIncidenceStructure(ps, ps!.dimension);
	end);

InstallGlobalFunction( OnProjSubspaces,
  function( var, el )
    local amb,geo,newvar;
    geo := var!.geo;   
    if var!.type = 1 then
        newvar := OnProjPointsWithFrob(var!.obj,el);
    else
        newvar := OnProjSubspacesWithFrob(var!.obj,el);
    fi;
    return Wrap(geo,var!.type,newvar);
  end );


InstallOtherMethod( \^, [IsElementOfIncidenceStructure, IsProjGrpElWithFrob],
  function(x, em)
    return OnProjSubspaces(x,em);
  end );

InstallGlobalFunction( OnSetsProjSubspaces,
  function( var, el )
    return Set( var, i -> OnProjSubspaces( i, el ) );
  end );


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
# Methods for \in with the EmtySubspace. Since the empty subspace is not 
# an element of an incidence geometry, we don't provide methods for IsIncident.
# This was decided on 17/04/2011 (Vicenza).
# Now I add the extra checks to see if two empty subspaces belong to the same
# space.
#############################################################################

InstallOtherMethod( \in, 
	"for the trivial subspace and a trivial subspace", 
	[ IsEmptySubspace, IsEmptySubspace ],
	function( x, y )
		if x!.geo = y!.geo then
			return true;
		else
			Error( "The subspaces <x> and <y> belong to different ambient spaces" );
		fi;
	end );
  
InstallOtherMethod( \in, 
	"for the trivial subspace and a non trivial subspace", 
	[ IsEmptySubspace, IsSubspaceOfProjectiveSpace ],
	function( x, y )
		if x!.geo = y!.geo then
			return true;
		else
			Error( "The subspaces <x> and <y> belong to different ambient spaces" );
		fi;
	end );
	
InstallOtherMethod( \in, 
	"for the trivial subspace and a non trivial subspace", 
	[ IsSubspaceOfProjectiveSpace, IsEmptySubspace ],
	function( x, y )
		if x!.geo = y!.geo then
			return false;
		else
			Error( "The subspaces <x> and <y> belong to different ambient spaces" );
		fi;
	end );

InstallOtherMethod( \in, 
	"for a projective subspace and its trivial subspace ", 
	[ IsProjectiveSpace, IsEmptySubspace ],
	function( x, y )
		if x = y!.geo then
			return false;
		else
			Error( "<x> is different from the ambient space of <y>" );
		fi;
	end );
  
InstallOtherMethod( \in, 
	"for the trivial subspace and a projective subspace", 
	[ IsEmptySubspace, IsProjectiveSpace ],
	function( x, y )
		if x!.geo = y then
			return false;
		else
			Error( "The subspace <x> has a different ambient space than <y>" );
		fi;
	end );


#############################################################################
#
# Nice methods for shadows of elements. Remind that shadow functionality 
# is to be implemented for the particular Lie geometries. See corresponding
# files. Very important: although we see functions here for Lie geometries
# where one could expect "containment" to be more in use than "incidence"
# the methods for Points, etc. really refer to "is incident with" than to
# is contained in. So these are really shortcuts to ElementsIncidentWithElementOfIncidenceStructure
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
InstallMethod( Points, "for IsElementOfLieGeometry",
	[ IsElementOfLieGeometry ],
	function( var )
		return ShadowOfElement(var!.geo, var, 1);
	end );

# CHECKED 19/4/2011 jdb
InstallMethod( Points, "for IsLieGeometry and IsElementOfLieGeometry", 
	[ IsLieGeometry, IsElementOfLieGeometry ],
	function( geo, var )
		return ShadowOfElement(geo, var, 1);
	end );

# CHECKED 19/4/2011 jdb
InstallMethod( Lines, "for IsElementOfLieGeometry", 
	[ IsElementOfLieGeometry ],
	function( var )
		return ShadowOfElement(var!.geo, var, 2);
	end );

# CHECKED 19/4/2011 jdb
InstallMethod( Lines, "for IsLieGeometry and IsElementOfLieGeometry", 
	[ IsLieGeometry, IsElementOfLieGeometry ],
	function( geo, var )
		return ShadowOfElement(geo, var, 2);
	end );

# CHECKED 19/4/2011 jdb
InstallMethod( Planes, "for IsElementOfLieGeometry", 
	[ IsElementOfLieGeometry ],
	function( var )
		return ShadowOfElement(var!.geo, var, 3);
	end );

# CHECKED 19/4/2011 jdb
InstallMethod( Planes, "for IsLieGeometry and IsElementOfLieGeometry", 
	[ IsLieGeometry, IsElementOfLieGeometry ],
	function( geo, var )
		return ShadowOfElement(geo, var, 3);
	end );

# CHECKED 19/4/2011 jdb
InstallMethod( Solids, [ IsElementOfLieGeometry ],
  function( var )
    return ShadowOfElement(var!.geo, var, 4);
  end );

# CHECKED 19/4/2011 jdb
InstallMethod( Solids, "for IsElementOfLieGeometry", 
	[ IsLieGeometry, IsElementOfLieGeometry ],
	function( geo, var )
		return ShadowOfElement(geo, var, 4);
	end );


#maybe move this bit to projective spaces?
InstallMethod( Hyperplanes, [ IsSubspaceOfProjectiveSpace ],
  function( var )
    local geo, d, f;
    geo := var!.geo;
    d := geo!.dimension;
    f := geo!.basefield;
    return ShadowOfElement( ProjectiveSpace(d, f), var, var!.type - 1 );
  end );

InstallMethod( Hyperplanes, [ IsLieGeometry, IsSubspaceOfProjectiveSpace ],
  function( geo, var )
	local d, f;
    d := geo!.dimension;
    f := geo!.basefield;
    return ShadowOfElement( geo, var, var!.type - 1 );
  end );


InstallMethod( ViewObj, [ IsShadowElementsOfLieGeometry and
  IsShadowElementsOfLieGeometryRep ],
  function( vs )
    Print("<shadow ",TypesOfElementsOfIncidenceStructurePlural(vs!.geometry)[vs!.type]," in ");
    ViewObj(vs!.geometry);
    Print(">");
  end );

