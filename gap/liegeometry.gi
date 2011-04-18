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
#
# Nice methods for shadows of elements. Remind that shadow functionality 
# is to be implemented for the particular Lie geometries. See corresponding
# files.
#
#############################################################################


InstallMethod( Points, [ IsElementOfLieGeometry ],
  function( var )
    return ShadowOfElement(var!.geo, var, 1);
  end );

InstallMethod( Points, [ IsLieGeometry, IsElementOfLieGeometry ],
  function( geo, var )
    return ShadowOfElement(geo, var, 1);
  end );

InstallMethod( Lines, [ IsElementOfLieGeometry ],
  function( var )
    return ShadowOfElement(var!.geo, var, 2);
  end );

InstallMethod( Lines, [ IsLieGeometry, IsElementOfLieGeometry ],
  function( geo, var )
    return ShadowOfElement(geo, var, 2);
  end );

InstallMethod( Planes, [ IsElementOfLieGeometry ],
  function( var )
    return ShadowOfElement(var!.geo, var, 3);
  end );

InstallMethod( Planes, [ IsLieGeometry, IsElementOfLieGeometry ],
  function( geo, var )
    return ShadowOfElement(geo, var, 3);
  end );

InstallMethod( Solids, [ IsElementOfLieGeometry ],
  function( var )
    return ShadowOfElement(var!.geo, var, 4);
  end );

InstallMethod( Solids, [ IsLieGeometry, IsElementOfLieGeometry ],
  function( geo, var )
    return ShadowOfElement(geo, var, 4);
  end );

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

