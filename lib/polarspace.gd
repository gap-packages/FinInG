#############################################################################
##
##  polarspace.gd              FinInG package
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
##  Declaration stuff for polar spaces
##
#############################################################################

#############################################################################
# Subspaces of polar spaces
#############################################################################

DeclareCategory( "IsSubspaceOfClassicalPolarSpace", IsSubspaceOfProjectiveSpace );

DeclareCategory( "IsSubspacesOfClassicalPolarSpace", IsSubspacesOfProjectiveSpace );
DeclareRepresentation( "IsSubspacesOfClassicalPolarSpaceRep", IsSubspacesOfProjectiveSpaceRep, [ "geometry", "type" ] );

DeclareCategory( "IsAllSubspacesOfClassicalPolarSpace", IsAllSubspacesOfProjectiveSpace );
DeclareRepresentation( "IsAllSubspacesOfClassicalPolarSpaceRep", IsAllSubspacesOfProjectiveSpaceRep, [ "geometry", "type" ] );

DeclareCategory( "IsShadowSubspacesOfClassicalPolarSpace", IsShadowElementsOfLieGeometry );
DeclareRepresentation( "IsShadowSubspacesOfClassicalPolarSpaceRep", IsShadowElementsOfLieGeometryRep, [ "geometry", "type", "inner", "outer", "factorspace" ]);

#DeclareCategoryCollections("IsSubspaceOfClassicalPolarSpace");
#BindGlobal( "SoCPSFamily", 
#  NewFamily( "SoCPSFamily", IsSubspaceOfClassicalPolarSpace, IsSubspaceOfClassicalPolarSpace));

#BindGlobal( "SoCPSCollFamily", CollectionsFamily(SoCPSFamily) );

DeclareCategory( "IsFlagOfClassicalPolarSpace", IsFlagOfProjectiveSpace );

BindGlobal( "FlagsOfCPS", NewFamily( "FlagsOfCPSFamily", IsObject ));  

BindGlobal( "IsFlagOfCPSType", NewType( FlagsOfCPS,
                                    IsFlagOfClassicalPolarSpace and IsFlagOfIncidenceStructureRep) );

#############################################################################
# Categories and Representations:
#############################################################################

#DeclareCategory( "IsPerpOfVariety", IsAttributeStoringRep ); # not used
#DeclareCategory( "IsPolarity", IsAttributeStoringRep ); #not used anymore

#############################################################################
# Attributes and Properties:
#############################################################################

#DeclareAttribute( "ProjectiveDimension", IsClassicalPolarSpace );
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
DeclareAttribute( "EquationForPolarSpace", IsClassicalPolarSpace );
DeclareAttribute( "NucleusOfParabolicQuadric", IsClassicalPolarSpace );

DeclareProperty( "IsEllipticQuadric", IsClassicalPolarSpace );
DeclareProperty( "IsSymplecticSpace", IsClassicalPolarSpace );
DeclareProperty( "IsParabolicQuadric", IsClassicalPolarSpace );
DeclareProperty( "IsHyperbolicQuadric", IsClassicalPolarSpace );
DeclareProperty( "IsHermitianPolarSpace", IsClassicalPolarSpace );
DeclareProperty( "IsStandardPolarSpace", IsClassicalPolarSpace );


#############################################################################
# Constructor operations:
#############################################################################

DeclareOperation( "PolarSpaceStandard", [IsForm, IsBool] );
DeclareOperation( "PolarSpace", [IsForm, IsField, IsGroup, IsFunction] );
DeclareOperation( "PolarSpace", [IsForm] );
DeclareOperation( "PolarMap", [IsClassicalPolarSpace] ); #only for internal use.

DeclareOperation( "TangentSpace", [IsSubspaceOfClassicalPolarSpace] );
DeclareOperation( "TangentSpace", [IsClassicalPolarSpace, IsSubspaceOfProjectiveSpace] );
DeclareOperation( "Pole", [IsClassicalPolarSpace, IsSubspaceOfProjectiveSpace] );

#DeclareOperation( "Polarisation", [IsQuadraticForm] ); #We can use now AssociatedBilinearForm
#DeclareOperation( "IsTotallySingular", [ IsClassicalPolarSpace and IsClassicalPolarSpaceRep,
#                                         IsSubspaceOfProjectiveSpace ] );
#DeclareOperation( "IsTotallyIsotropic", [ IsClassicalPolarSpace and IsClassicalPolarSpaceRep,
#                                         IsSubspaceOfProjectiveSpace ] );

DeclareOperation( "TypeOfSubspace", [ IsClassicalPolarSpace, IsSubspaceOfProjectiveSpace ]);
DeclareOperation( "CanonicalOrbitRepresentativeForSubspaces", [IsString, IsPosInt, IsField]);
DeclareOperation( "RandomSubspace", [IsClassicalPolarSpace, IsPosInt]);
DeclareOperation( "NumberOfTotallySingularSubspaces", [IsClassicalPolarSpace, IsPosInt]);

DeclareOperation( "EllipticQuadric", [IsPosInt, IsField] );
DeclareOperation( "EllipticQuadric", [IsPosInt, IsPosInt ]);
DeclareOperation( "SymplecticSpace", [IsPosInt, IsField] );
DeclareOperation( "SymplecticSpace", [IsPosInt, IsPosInt] );
DeclareOperation( "ParabolicQuadric", [IsPosInt, IsField] );
DeclareOperation( "ParabolicQuadric", [IsPosInt, IsPosInt] );
DeclareOperation( "HyperbolicQuadric", [IsPosInt, IsField] );
DeclareOperation( "HyperbolicQuadric", [IsPosInt, IsPosInt] );
DeclareOperation( "HermitianPolarSpace", [IsPosInt, IsField] );
DeclareOperation( "HermitianPolarSpace", [IsPosInt, IsPosInt] );

DeclareOperation( "CanonicalPolarSpace", [IsClassicalPolarSpace] );
DeclareOperation( "StandardPolarSpace", [IsClassicalPolarSpace] );

#DeclareAttribute( "DefiningPolarity", IsClassicalPolarSpace );

DeclareOperation( "Span", [IsSubspaceOfClassicalPolarSpace, IsSubspaceOfClassicalPolarSpace, IsBool] );

