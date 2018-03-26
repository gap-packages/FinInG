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
##  Copyright 2018	Colorado State University
##                  Sabancı Üniversitesi
##					Università degli Studi di Padova
##					Universiteit Gent
##					University of St. Andrews
##					University of Western Australia
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

# first one is a helper operations

DeclareOperation( "IsomorphismPolarSpacesProjectionFromNucleus", [IsClassicalPolarSpace, IsClassicalPolarSpace, IsBool ] );

# the NC versions of the isomorphisms, for internal use, not including the previous one.

DeclareOperation( "IsomorphismPolarSpacesNC", 
					 [ IsClassicalPolarSpace, IsClassicalPolarSpace, IsBool ]);
DeclareOperation( "IsomorphismPolarSpacesNC", 
                     [ IsClassicalPolarSpace, IsClassicalPolarSpace ]);

# the user operations.

DeclareOperation( "IsomorphismPolarSpaces", 
                     [ IsClassicalPolarSpace, IsClassicalPolarSpace, IsBool ]);
DeclareOperation( "IsomorphismPolarSpaces", 
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
DeclareOperation( "NaturalEmbeddingByFieldReduction", [ IsClassicalPolarSpace, IsField, IsFFE, IsBasis, IsBool ] );

#first version: user wants a particular alpha, and basis, agrees with bool=true.
DeclareOperation( "NaturalEmbeddingByFieldReduction", [ IsClassicalPolarSpace, IsField, IsFFE, IsBasis ]);

#second particular version: user wants a particular alpha, and bool, agrees with basis.
DeclareOperation( "NaturalEmbeddingByFieldReduction", [ IsClassicalPolarSpace, IsField, IsFFE, IsBool ] );

#third particular version: user wants a particular alpha, agrees with basis and bool.
DeclareOperation( "NaturalEmbeddingByFieldReduction", [ IsClassicalPolarSpace, IsField, IsFFE ]);

# fourth particular version: user agrees but wants to control intertwiner
DeclareOperation( "NaturalEmbeddingByFieldReduction", [ IsClassicalPolarSpace, IsField, IsBool ]);

# fifth particular version: user agrees with everything
DeclareOperation( "NaturalEmbeddingByFieldReduction", [ IsClassicalPolarSpace, IsField ]);


# master version with two polar spaces as argument, second one with control on intertwiner.
DeclareOperation( "NaturalEmbeddingByFieldReduction", [ IsClassicalPolarSpace, IsClassicalPolarSpace, IsBool ]);
DeclareOperation( "NaturalEmbeddingByFieldReduction", [ IsClassicalPolarSpace, IsClassicalPolarSpace]);


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

#Klein correspondence: from the bare essentials to the more advanced (all user functions)

#user version of PluckerCoordinates.
DeclareOperation( "PluckerCoordinates", [ IsSubspaceOfProjectiveSpace ]);

# the morpihms.
DeclareOperation( "KleinCorrespondence", [ IsField, IsBool ]);
DeclareOperation( "KleinCorrespondence", [ IsField ]);

DeclareOperation( "KleinCorrespondence", [ IsPosInt, IsBool ]);
DeclareOperation( "KleinCorrespondence", [ IsPosInt ]);

DeclareOperation( "KleinCorrespondence", [ IsClassicalPolarSpace, IsBool ]);
DeclareOperation( "KleinCorrespondence", [ IsClassicalPolarSpace ]);

DeclareOperation( "KleinCorrespondenceExtended", [ IsField, IsBool ]);
DeclareOperation( "KleinCorrespondenceExtended", [ IsField ]);
DeclareOperation( "KleinCorrespondenceExtended", [ IsPosInt, IsBool ]);
DeclareOperation( "KleinCorrespondenceExtended", [ IsPosInt ]);
DeclareOperation( "KleinCorrespondenceExtended", [ IsClassicalPolarSpace, IsBool ]);
DeclareOperation( "KleinCorrespondenceExtended", [ IsClassicalPolarSpace ]);

# derived dualities. 
# first: helper operations

DeclareOperation( "NaturalDualitySymplectic", [ IsClassicalGQ, IsClassicalGQ, IsBool, IsBool ]);
DeclareOperation( "NaturalDualityHermitian", [ IsClassicalGQ, IsClassicalGQ, IsBool, IsBool ]);

DeclareOperation( "SelfDualitySymplectic", [ IsClassicalGQ, IsBool ]);
DeclareOperation( "SelfDualityParabolic", [ IsClassicalGQ, IsBool ]);

# second: user operations.

DeclareOperation( "NaturalDuality", [ IsClassicalGQ, IsClassicalGQ, IsBool ]);
DeclareOperation( "NaturalDuality", [ IsClassicalGQ, IsClassicalGQ ]);
DeclareOperation( "NaturalDuality", [ IsClassicalGQ, IsBool ]);
DeclareOperation( "NaturalDuality", [ IsClassicalGQ ]);

DeclareOperation( "SelfDuality", [ IsClassicalGQ, IsBool ]);
DeclareOperation( "SelfDuality", [ IsClassicalGQ ]);

# some left over: projective completion.

DeclareOperation( "ProjectiveCompletion", [ IsAffineSpace ] );


