#############################################################################
##
##  polarspace.gd              FinInG package
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
##  Copyright 2006 University of Western Australia, Perth
##                 Lehrstuhl D fuer Mathematik, RWTH Aachen
##                 Ghent University
##                 Colorado State University
##                 Vrije Universiteit Brussel
##
##  Declaration stuff for polar spaces
##
#############################################################################

#############################################################################
# Subspaces of polar spaces
#############################################################################

DeclareCategory( "IsSubspaceOfClassicalPolarSpace", IsSubspaceOfProjectiveSpace );

DeclareCategory( "IsSubspacesOfClassicalPolarSpace", IsSubspacesOfProjectiveSpace );
DeclareRepresentation( "IsSubspaceOfClassicalPolarSpaceRep", IsAllSubspacesOfProjectiveSpaceRep, [ "geometry", "type" ] );

DeclareCategory( "IsAllSubspacesOfClassicalPolarSpace", IsAllSubspacesOfProjectiveSpace );
DeclareRepresentation( "IsAllSubspacesOfClassicalPolarSpaceRep", IsAllSubspacesOfProjectiveSpaceRep, [ "geometry", "type" ] );

DeclareCategory( "IsShadowSubspacesOfClassicalPolarSpace", IsShadowElementsOfLieGeometry );
DeclareRepresentation( "IsShadowSubspacesOfClassicalPolarSpaceRep", IsShadowElementsOfLieGeometryRep, [ "geometry", "type", "inner", "outer", "factorspace" ]);


#############################################################################
# Categories and Representations:
#############################################################################

#DeclareCategory( "IsPerpOfVariety", IsAttributeStoringRep ); # not used
#DeclareCategory( "IsPolarity", IsAttributeStoringRep ); #not used anymore

#############################################################################
# Attributes and Properties:
#############################################################################

DeclareAttribute( "ProjectiveDimension", IsClassicalPolarSpace );
DeclareAttribute( "SesquilinearForm", IsClassicalPolarSpace );
DeclareAttribute( "QuadraticForm", IsClassicalPolarSpace );
DeclareAttribute( "AmbientSpace", IsClassicalPolarSpace );
DeclareAttribute( "SimilarityGroup", IsClassicalPolarSpace );
DeclareAttribute( "IsometryGroup", IsClassicalPolarSpace );
DeclareAttribute( "SpecialIsometryGroup", IsClassicalPolarSpace );
DeclareAttribute( "IsomorphismCanonicalPolarSpace", IsClassicalPolarSpace );
DeclareAttribute( "IsomorphismCanonicalPolarSpaceWithIntertwiner", IsClassicalPolarSpace );
DeclareAttribute( "IsCanonicalPolarSpace", IsClassicalPolarSpace );
DeclareAttribute( "PolarSpaceType", IsClassicalPolarSpace );
DeclareAttribute( "CompanionAutomorphism", IsClassicalPolarSpace );
DeclareAttribute( "ClassicalGroupInfo", IsClassicalPolarSpace );

DeclareProperty( "IsEllipticQuadric", IsClassicalPolarSpace );
DeclareProperty( "IsSymplecticSpace", IsClassicalPolarSpace );
DeclareProperty( "IsParabolicQuadric", IsClassicalPolarSpace );
DeclareProperty( "IsHyperbolicQuadric", IsClassicalPolarSpace );
DeclareProperty( "IsHermitianVariety", IsClassicalPolarSpace );

#############################################################################
# Constructor operations:
#############################################################################

DeclareOperation( "PolarSpace", [IsForm, IsField, IsGroup, IsFunction] );
DeclareOperation( "PolarSpace", [IsForm] );
DeclareOperation( "Polarity", [IsClassicalPolarSpace] );
#DeclareOperation( "Polarisation", [IsQuadraticForm] ); #We can use now AssociatedBilinearForm
DeclareOperation( "IsTotallySingular", [ IsClassicalPolarSpace and IsClassicalPolarSpaceRep,
                                         IsSubspaceOfProjectiveSpace ] );
DeclareOperation( "IsTotallyIsotropic", [ IsClassicalPolarSpace and IsClassicalPolarSpaceRep,
                                         IsSubspaceOfProjectiveSpace ] );
DeclareOperation( "TypeOfSubspace", [ IsClassicalPolarSpace, IsSubspaceOfProjectiveSpace ]);
DeclareOperation( "CanonicalGramMatrix", [IsString, IsPosInt, IsField]); 
DeclareOperation( "CanonicalQuadraticForm", [IsString, IsPosInt, IsField]); 
DeclareOperation( "CanonicalOrbitRepresentativeForSubspaces", [IsString, IsPosInt, IsField]);
DeclareOperation( "RandomSubspace", [IsClassicalPolarSpace, IsPosInt]);


DeclareOperation( "EllipticQuadric", [IsPosInt, IsField] );
DeclareOperation( "EllipticQuadric", [IsPosInt, IsPosInt ]);
DeclareOperation( "SymplecticSpace", [IsPosInt, IsField] );
DeclareOperation( "SymplecticSpace", [IsPosInt, IsPosInt] );
DeclareOperation( "ParabolicQuadric", [IsPosInt, IsField] );
DeclareOperation( "ParabolicQuadric", [IsPosInt, IsPosInt] );
DeclareOperation( "HyperbolicQuadric", [IsPosInt, IsField] );
DeclareOperation( "HyperbolicQuadric", [IsPosInt, IsPosInt] );
DeclareOperation( "HermitianVariety", [IsPosInt, IsField] );
DeclareOperation( "HermitianVariety", [IsPosInt, IsPosInt] );

#DeclareAttribute( "DefiningPolarity", IsClassicalPolarSpace );


