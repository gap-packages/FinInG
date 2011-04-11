#############################################################################
##
##  liegeometry.gi              FinInG package
##                                                              John Bamberg
## 								Anton Betten
##                                                              Jan De Beule
##                                                             Philippe Cara
## 							      Michel Lavrauw
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
# Constructor methods:
#############################################################################

InstallMethod( Wrap, "for a Lie geometry and an object",
  [IsLieGeometry, IsPosInt, IsObject],
  function( geo, type, o )
    local w;
    w := rec( geo := geo, type := type, obj := o );
    Objectify( NewType( ElementsOfIncidenceStructureFamily, IsElementOfIncidenceStructure and
      IsElementOfIncidenceStructureRep and IsElementOfLieGeometry ), w );
    return w;
  end );

InstallMethod( ElementToVectorSpace, 
   "for an element of a LieGeometry", [IsElementOfLieGeometry],
  function( x )
    return Unwrap(x);
  end );

InstallMethod( Points, [IsLieGeometry],
  function( ps )
    return ElementsOfIncidenceStructure(ps, 1);
  end);

InstallMethod( Lines, [IsLieGeometry],
  function( ps )
    return ElementsOfIncidenceStructure(ps, 2);
  end);

InstallMethod( Planes, [IsLieGeometry],
  function( ps )
    return ElementsOfIncidenceStructure(ps, 3);
  end);

InstallMethod( Solids, [IsLieGeometry],
  function( ps )
    return ElementsOfIncidenceStructure(ps, 4);
  end);

InstallMethod( Hyperplanes, [IsProjectiveSpace],
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
# Nice methods for shadows of elements
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

#############################################################################
# View methods:
#############################################################################

InstallMethod( ViewObj, [ IsAllElementsOfLieGeometry and
  IsAllElementsOfLieGeometryRep ],
  function( vs )
    Print("<All elements of ");
    ViewObj(vs!.geometry);
    Print(">");
  end );

InstallMethod( PrintObj, [ IsAllElementsOfLieGeometry and
  IsAllElementsOfLieGeometryRep ],
  function( vs )
    Print("AllElementsOfIncidenceStructure( ",vs!.geometry," )");
  end );

InstallMethod( ViewObj, [ IsShadowElementsOfLieGeometry and
  IsShadowElementsOfLieGeometryRep ],
  function( vs )
    Print("<shadow ",TypesOfElementsOfIncidenceStructurePlural(vs!.geometry)[vs!.type]," in ");
    ViewObj(vs!.geometry);
    Print(">");
  end );

InstallMethod( ViewObj, [ IsElementsOfLieGeometry and
  IsElementsOfLieGeometryRep ],
  function( vs )
    Print("<",TypesOfElementsOfIncidenceStructurePlural(vs!.geometry)[vs!.type]," of ");
    ViewObj(vs!.geometry);
    Print(">");
  end );

InstallMethod( PrintObj, [ IsElementsOfLieGeometry and
  IsElementsOfLieGeometryRep ],
  function( vs )
    Print("ElementsOfIncidenceStructure( ",vs!.geometry," , ",vs!.type,")");
  end );


