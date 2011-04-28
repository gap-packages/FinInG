#############################################################################
##
##  liegeometry.gd            FinInG package
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
##  Declaration stuff for Lie geometries.
##
#############################################################################

#############################################################################
# Constructor operations:
#############################################################################

## none yet

#############################################################################
#
# Elements of Lie geometries -  definitions and operations:
#
#############################################################################

DeclareCategory( "IsElementOfLieGeometry", IsElementOfIncidenceGeometry );
DeclareCategoryCollections("IsElementOfLieGeometry");

DeclareCategory( "IsElementsOfLieGeometry", IsElementsOfIncidenceGeometry );
DeclareCategory( "IsAllElementsOfLieGeometry", IsAllElementsOfIncidenceGeometry );

DeclareRepresentation( "IsAllElementsOfLieGeometryRep", IsAllElementsOfIncidenceStructureRep, [ "geometry", "type" ] );
  
DeclareRepresentation( "IsElementsOfLieGeometryRep", IsElementsOfIncidenceStructureRep, [ "geometry", "type" ] );

DeclareCategory( "IsFlagOfLieGeometry", IsFlagOfIncidenceGeometry );

#DeclareRepresentation( "IsFlagOfLieGeometryRep", IsFlagOfLieGeometry, [ "geo", "types", "els" ] );

DeclareCategory( "IsShadowElementsOfLieGeometry", IsElementsOfIncidenceStructure );
DeclareRepresentation( "IsShadowElementsOfLieGeometryRep", IsElementsOfIncidenceStructure, [ "geometry", "type", "inner", "outer", "factorspace" ]);

DeclareOperation( "UnderlyingVectorSpace", [IsLieGeometry] );
DeclareOperation( "UnderlyingVectorSpace", [IsElementOfLieGeometry] );


DeclareOperation( "VectorSpaceToElement", [IsLieGeometry, IsRowVector] );
DeclareOperation( "VectorSpaceToElement", [IsLieGeometry, Is8BitVectorRep] );
DeclareOperation( "VectorSpaceToElement", [IsLieGeometry, IsPlistRep] );
DeclareOperation( "VectorSpaceToElement", [IsLieGeometry, Is8BitMatrixRep] );
DeclareOperation( "VectorSpaceToElement", [IsLieGeometry, IsGF2MatrixRep] );
DeclareOperation( "ElementToVectorSpace", [IsElementOfLieGeometry] );

DeclareGlobalFunction( "OnProjSubspaces" );
DeclareGlobalFunction( "OnSetsProjSubspaces" );

#DeclareSynonym( "OnLieVarieties", OnProjSubspaces );
#DeclareSynonym( "OnSetsLieVarieties", OnSetsProjSubspaces );

## Shadows

#DeclareOperation( "Points", [ IsElementOfLieGeometry ] );
#DeclareOperation( "Lines", [ IsElementOfLieGeometry ] );
#DeclareOperation( "Planes", [ IsElementOfLieGeometry ] );
#DeclareOperation( "Solids", [ IsElementOfLieGeometry ] );
#DeclareOperation( "Hyperplanes", [ IsElementOfLieGeometry ] );

#DeclareOperation( "Points", [ IsLieGeometry, IsElementOfLieGeometry ] );
#DeclareOperation( "Lines", [ IsLieGeometry, IsElementOfLieGeometry ] );
#DeclareOperation( "Planes", [ IsLieGeometry, IsElementOfLieGeometry ] );
#DeclareOperation( "Solids", [ IsLieGeometry, IsElementOfLieGeometry ] );
#DeclareOperation( "Hyperplanes", [ IsLieGeometry, IsElementOfLieGeometry ] );

