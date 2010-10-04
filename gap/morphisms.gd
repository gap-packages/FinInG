#############################################################################
##
##  morphisms.gd              FinInG package
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
##  Declaration stuff for incidence geometry morphisms.
##
#############################################################################

#############################################################################
#
# Basic data types and operations
#
#############################################################################

DeclareCategory( "IsGeometryMorphism", IsMapping );
#BindGlobal( "GeometryMorphismFamily", NewFamily( "GeometryMorphismFamily" ) );
DeclareAttribute( "Intertwiner", IsGeometryMorphism );

#############################################################################
#
# Constructor operations:
#
#############################################################################


DeclareOperation( "GeometryMorphismByFunction", 
  [ IsAllElementsOfIncidenceStructure, IsAllElementsOfIncidenceStructure,
    IsFunction, IsBool, IsFunction ] );
    
DeclareOperation( "GeometryMorphismByFunction", 
  [ IsAllElementsOfIncidenceStructure, IsAllElementsOfIncidenceStructure,
    IsFunction, IsFunction ] );

DeclareOperation( "GeometryMorphismByFunction", 
  [ IsAllElementsOfIncidenceStructure, IsAllElementsOfIncidenceStructure,
    IsFunction ] );



## C1 morphisms
DeclareOperation( "NaturalEmbeddingBySubspace", 
                     [ IsLieGeometry, IsLieGeometry, IsSubspaceOfProjectiveSpace ]);
DeclareOperation( "NaturalEmbeddingBySubspaceNC", 
                     [ IsLieGeometry, IsLieGeometry, IsSubspaceOfProjectiveSpace ]);

DeclareOperation( "NaturalProjectionBySubspace",
                     [ IsClassicalPolarSpace, IsSubspaceOfClassicalPolarSpace ]);
DeclareOperation( "NaturalProjectionBySubspace",
                     [ IsProjectiveSpace, IsSubspaceOfProjectiveSpace ]);
DeclareOperation( "NaturalProjectionBySubspaceNC",
                     [ IsClassicalPolarSpace, IsSubspaceOfClassicalPolarSpace ]);
DeclareOperation( "NaturalProjectionBySubspaceNC",
                     [ IsProjectiveSpace, IsSubspaceOfProjectiveSpace ]);

## C3 morphisms
DeclareOperation( "NaturalEmbeddingByFieldReduction",
                     [ IsProjectiveSpace, IsProjectiveSpace ]);
DeclareOperation( "NaturalEmbeddingByFieldReduction", 
                     [ IsProjectiveSpace, IsProjectiveSpace, IsBasis ]);
DeclareOperation( "NaturalEmbeddingByFieldReduction",
                     [ IsClassicalPolarSpace, IsClassicalPolarSpace, IsBool ]);
DeclareOperation( "NaturalEmbeddingByFieldReduction",
					 [ IsClassicalPolarSpace, IsClassicalPolarSpace]);

## C5 morphisms
DeclareOperation( "NaturalEmbeddingBySubfield",
                     [ IsProjectiveSpace, IsProjectiveSpace ]);
DeclareOperation( "NaturalEmbeddingBySubfield",
					 [ IsClassicalPolarSpace, IsClassicalPolarSpace, IsBool ]);
DeclareOperation( "NaturalEmbeddingBySubfield",
                     [ IsClassicalPolarSpace, IsClassicalPolarSpace ]);



## Other
DeclareOperation( "IsomorphismPolarSpaces", 
                     [ IsClassicalPolarSpace, IsClassicalPolarSpace, IsBool ]);
DeclareOperation( "IsomorphismPolarSpaces", 
                     [ IsClassicalPolarSpace, IsClassicalPolarSpace ]);
DeclareOperation( "IsomorphismPolarSpacesNC", 
					 [ IsClassicalPolarSpace, IsClassicalPolarSpace, IsBool ]);
DeclareOperation( "IsomorphismPolarSpacesNC", 
                     [ IsClassicalPolarSpace, IsClassicalPolarSpace ]);
DeclareOperation( "PluckerCoordinates", [ IsSubspaceOfProjectiveSpace ]);
DeclareOperation( "InversePluckerCoordinates", [ IsSubspaceOfProjectiveSpace ]);
DeclareOperation( "KleinCorrespondence", [ IsHyperbolicQuadric ]);

DeclareOperation( "NaturalDuality", [ IsSymplecticSpace and IsGeneralisedPolygon ]);
DeclareOperation( "NaturalDuality", [ IsHermitianVariety and IsGeneralisedPolygon ]);
DeclareOperation( "ProjectiveCompletion", [ IsAffineSpace ] );






#############################################################################
#
# Global functions:
#
#############################################################################

DeclareGlobalFunction( "LeukBasis" );
DeclareGlobalFunction( "ShrinkMat" );
