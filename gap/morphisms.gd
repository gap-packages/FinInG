#############################################################################
##
##  morphisms.gd              FinInG package
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
##  Implementation stuff for incidence geometry morphisms
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
  [ IsAnyElementsOfIncidenceStructure, IsAnyElementsOfIncidenceStructure,
    IsFunction, IsBool, IsFunction ] );
    
DeclareOperation( "GeometryMorphismByFunction", 
  [ IsAnyElementsOfIncidenceStructure, IsAnyElementsOfIncidenceStructure,
    IsFunction, IsFunction ] );

DeclareOperation( "GeometryMorphismByFunction", 
  [ IsAnyElementsOfIncidenceStructure, IsAnyElementsOfIncidenceStructure,
    IsFunction ] );

#############################################################################
## isomorphisms
#############################################################################

DeclareOperation( "IsomorphismPolarSpaces", 
                     [ IsClassicalPolarSpace, IsClassicalPolarSpace, IsBool ]);
DeclareOperation( "IsomorphismPolarSpaces", 
                     [ IsClassicalPolarSpace, IsClassicalPolarSpace ]);
DeclareOperation( "IsomorphismPolarSpacesNC", 
					 [ IsClassicalPolarSpace, IsClassicalPolarSpace, IsBool ]);
DeclareOperation( "IsomorphismPolarSpacesNC", 
                     [ IsClassicalPolarSpace, IsClassicalPolarSpace ]);

#############################################################################
## C1 morphisms
#############################################################################

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

#############################################################################
## C3 morphisms
#############################################################################

DeclareOperation( "ShrinkMat", [IsBasis, IsMatrix] );
DeclareGlobalFunction( "LeukBasis" );
DeclareOperation( "BlownUpProjectiveSpace", [IsBasis, IsProjectiveSpace ] );  
DeclareOperation( "BlownUpProjectiveSpaceBySubfield", [ IsField, IsProjectiveSpace ] ); 
DeclareOperation( "BlownUpSubspaceOfProjectiveSpace", [ IsBasis, IsSubspaceOfProjectiveSpace ] );
DeclareOperation( "BlownUpSubspaceOfProjectiveSpaceBySubfield", [IsField, IsSubspaceOfProjectiveSpace ] ); 
DeclareOperation( "IsDesarguesianSpreadElement", [ IsBasis, IsSubspaceOfProjectiveSpace ] ); 
DeclareOperation( "IsBlownUpSubspaceOfProjectiveSpace", [IsBasis, IsSubspaceOfProjectiveSpace ] ); 

DeclareOperation( "NaturalEmbeddingByFieldReduction",
                     [ IsProjectiveSpace, IsProjectiveSpace ]);
DeclareOperation( "NaturalEmbeddingByFieldReduction", 
                     [ IsProjectiveSpace, IsProjectiveSpace, IsBasis ]);
DeclareOperation( "NaturalEmbeddingByFieldReduction",
                     [ IsClassicalPolarSpace, IsClassicalPolarSpace, IsBool ]);
DeclareOperation( "NaturalEmbeddingByFieldReduction",
					 [ IsClassicalPolarSpace, IsClassicalPolarSpace]);

#############################################################################
## C5 morphisms
#############################################################################

DeclareOperation( "NaturalEmbeddingBySubfield",
                     [ IsProjectiveSpace, IsProjectiveSpace ]);
DeclareOperation( "NaturalEmbeddingBySubfield",
					 [ IsClassicalPolarSpace, IsClassicalPolarSpace, IsBool ]);
DeclareOperation( "NaturalEmbeddingBySubfield",
                     [ IsClassicalPolarSpace, IsClassicalPolarSpace ]);

#############################################################################
## Other morphisms
#############################################################################

DeclareOperation( "PluckerCoordinates", [ IsSubspaceOfProjectiveSpace ]);
DeclareOperation( "InversePluckerCoordinates", [ IsSubspaceOfProjectiveSpace ]);
DeclareOperation( "KleinCorrespondence", [ IsHyperbolicQuadric ]);

DeclareOperation( "NaturalDuality", [ IsSymplecticSpace and IsGeneralisedPolygon ]);
DeclareOperation( "NaturalDuality", [ IsHermitianVariety and IsGeneralisedPolygon ]);
DeclareOperation( "ProjectiveCompletion", [ IsAffineSpace ] );


