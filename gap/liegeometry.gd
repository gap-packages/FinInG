#############################################################################
##
##  liegeometry.gd              FinInG package
##                                                              John Bamberg
## 								                                Anton Betten
##                                                             Philippe Cara
##                                                              Jan De Beule
## 							                                  Michel Lavrauw
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

DeclareCategory( "IsElementOfLieGeometry", IsElementOfIncidenceStructure );
DeclareCategory( "IsElementsOfLieGeometry", IsElementsOfIncidenceStructure );
DeclareCategory( "IsAllElementsOfLieGeometry", IsAllElementsOfIncidenceStructure );

DeclareRepresentation( "IsAllElementsOfLieGeometryRep", IsAllElementsOfIncidenceStructureRep, [ "geometry", "type" ] );
  
DeclareRepresentation( "IsElementsOfLieGeometryRep", IsElementsOfIncidenceStructureRep, [ "geometry", "type" ] );

DeclareCategory( "IsShadowElementsOfLieGeometry", IsElementsOfIncidenceStructure );
DeclareRepresentation( "IsShadowElementsOfLieGeometryRep", IsElementsOfIncidenceStructure, [ "geometry", "type", "inner", "outer", "factorspace" ]);

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

DeclareOperation( "Points", [ IsElementOfLieGeometry ] );
DeclareOperation( "Lines", [ IsElementOfLieGeometry ] );
DeclareOperation( "Planes", [ IsElementOfLieGeometry ] );
DeclareOperation( "Solids", [ IsElementOfLieGeometry ] );
DeclareOperation( "Hyperplanes", [ IsElementOfLieGeometry ] );

DeclareOperation( "Points", [ IsLieGeometry, IsElementOfLieGeometry ] );
DeclareOperation( "Lines", [ IsLieGeometry, IsElementOfLieGeometry ] );
DeclareOperation( "Planes", [ IsLieGeometry, IsElementOfLieGeometry ] );
DeclareOperation( "Solids", [ IsLieGeometry, IsElementOfLieGeometry ] );
DeclareOperation( "Hyperplanes", [ IsLieGeometry, IsElementOfLieGeometry ] );

