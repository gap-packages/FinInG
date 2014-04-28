#############################################################################
##
##  morphisms.gd              FinInG package
##                                                              John Bamberg
##                                                              Anton Betten
##                                                              Jan De Beule
##                                                             Philippe Cara
##                                                            Michel Lavrauw
##                                                           Max Neunhoeffer
##
##  Copyright 2014	Colorado State University, Fort Collins
##					Università degli Studi di Padova
##					Universeit Gent
##					University of St. Andrews
##					University of Western Australia, Perth
##                  Vrije Universiteit Brussel
##                 
##
##  Declaration stuff for incidence geometry morphisms
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
                     
DeclareOperation( "IsomorphismPolarSpacesProjectionFromNucleus", [IsClassicalPolarSpace, IsClassicalPolarSpace, IsBool ] );


#############################################################################
## C3 morphisms
#############################################################################

DeclareOperation( "ShrinkMat", [IsBasis, IsMatrix] );
DeclareOperation( "ShrinkMat", [IsField,IsField,IsVector]);

DeclareOperation( "ShrinkVec", [IsField,IsField,IsVector]);
DeclareOperation( "ShrinkVec", [IsField,IsField,IsVector,IsBasis]);

DeclareGlobalFunction( "LeukBasis" );
DeclareOperation( "BlownUpProjectiveSpace", [IsBasis, IsProjectiveSpace ] );  
DeclareOperation( "BlownUpProjectiveSpaceBySubfield", [ IsField, IsProjectiveSpace ] ); 
DeclareOperation( "BlownUpSubspaceOfProjectiveSpace", [ IsBasis, IsSubspaceOfProjectiveSpace ] );
DeclareOperation( "BlownUpSubspaceOfProjectiveSpaceBySubfield", [IsField, IsSubspaceOfProjectiveSpace ] ); 
DeclareOperation( "IsDesarguesianSpreadElement", [ IsBasis, IsSubspaceOfProjectiveSpace ] ); 
DeclareOperation( "IsBlownUpSubspaceOfProjectiveSpace", [IsBasis, IsSubspaceOfProjectiveSpace ] ); 


DeclareOperation( "NaturalEmbeddingByFieldReduction", 
                     [ IsProjectiveSpace, IsField, IsBasis ]);
DeclareOperation( "NaturalEmbeddingByFieldReduction", 
                     [ IsProjectiveSpace, IsField ]);

DeclareOperation( "NaturalEmbeddingByFieldReduction",
                     [ IsProjectiveSpace, IsProjectiveSpace ]);
DeclareOperation( "NaturalEmbeddingByFieldReduction", 
                     [ IsProjectiveSpace, IsProjectiveSpace, IsBasis ]);

DeclareOperation( "BilinearFormFieldReduction",	[ IsBilinearForm, IsField, IsFFE, IsBasis ]);
DeclareOperation( "QuadraticFormFieldReduction", [ IsQuadraticForm, IsField, IsFFE, IsBasis ]);
DeclareOperation( "HermitianFormFieldReduction", [ IsHermitianForm, IsField, IsFFE, IsBasis ]);

DeclareOperation( "BilinearFormFieldReduction",	[ IsBilinearForm, IsField, IsFFE ]);
DeclareOperation( "QuadraticFormFieldReduction", [ IsQuadraticForm, IsField, IsFFE ]);
DeclareOperation( "HermitianFormFieldReduction", [ IsHermitianForm, IsField, IsFFE ]);


# master version for the user: handle all parameters.
DeclareOperation( "NaturalEmbeddingByFieldReduction", 
					[ IsClassicalPolarSpace, IsField, IsFFE, IsBasis, IsBool ] );

# first particular version: user agrees with everything

DeclareOperation( "NaturalEmbeddingByFieldReduction",
				[ IsClassicalPolarSpace, IsField ]);

# second particular version: user agrees but wants to control intertwiner

DeclareOperation( "NaturalEmbeddingByFieldReduction",
				[ IsClassicalPolarSpace, IsField, IsBool ]);

#third particular version: user wants a particular alpha, agrees with base and bool.

DeclareOperation( "NaturalEmbeddingByFieldReduction",
				[ IsClassicalPolarSpace, IsField, IsFFE ]);

#fourth version: user wants a particular alpha, and base, agrees with bool.
DeclareOperation( "NaturalEmbeddingByFieldReduction",
				[ IsClassicalPolarSpace, IsField, IsFFE, IsBasis ]);


DeclareOperation( "NaturalEmbeddingByFieldReduction",
                     [ IsClassicalPolarSpace, IsClassicalPolarSpace, IsBool ]);

DeclareOperation( "NaturalEmbeddingByFieldReduction",
					 [ IsClassicalPolarSpace, IsClassicalPolarSpace]);


DeclareOperation( "CanonicalEmbeddingByFieldReduction", 
					[ IsClassicalPolarSpace, IsField, IsBool ]);  
DeclareOperation( "CanonicalEmbeddingByFieldReduction", 
					[ IsClassicalPolarSpace, IsClassicalPolarSpace, IsBool ]);

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

#helper operations
DeclareOperation( "PluckerCoordinates", [ IsMatrix ]);
DeclareOperation( "InversePluckerCoordinates", [ IsVector ]);

DeclareOperation( "NaturalDualitySymplectic", [ IsClassicalGQ, IsClassicalGQ ]);
DeclareOperation( "NaturalDualityHermitian", [ IsClassicalGQ, IsClassicalGQ ]);

DeclareOperation( "NaturalDualityParabolic", [ IsClassicalGQ, IsClassicalGQ ]);
DeclareOperation( "NaturalDualityElliptic", [ IsClassicalGQ, IsClassicalGQ ]);

#the user stuff.

DeclareOperation( "KleinCorrespondence", [ IsClassicalPolarSpace ]);

DeclareOperation( "NaturalDuality", [ IsClassicalGQ, IsClassicalGQ ]);
DeclareOperation( "NaturalDuality", [ IsClassicalGQ ]);



DeclareOperation( "ProjectiveCompletion", [ IsAffineSpace ] );


