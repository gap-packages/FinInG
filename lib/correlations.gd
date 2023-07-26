#############################################################################
##
##  correlations.gd              FinInG package
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
##  Declaration stuff for correlation groups and polarities of a projective
##  space.
##
#############################################################################

DeclareCategory( "IsProjectiveSpaceIsomorphism", IsSPGeneralMapping );
DeclareCategory( "IsStandardDualityOfProjectiveSpace", IsProjectiveSpaceIsomorphism );
DeclareCategory( "IsIdentityMappingOfElementsOfProjectiveSpace", IsProjectiveSpaceIsomorphism );

DeclareCategory( "IsProjGrpElWithFrobWithPSIsom", IsComponentObjectRep and IsMultiplicativeElementWithInverse );
DeclareSynonym( "IsCorrelationCollineation", IsProjGrpElWithFrobWithPSIsom);

DeclareProperty( "IsCorrelation", IsProjGrpElWithFrobWithPSIsom );# user-friendly functionality 
DeclareProperty( "IsCorrelation", IsProjGrpElWithFrob );# user-friendly functionality 
DeclareProperty( "IsCorrelation", IsProjGrpEl );# user-friendly functionality 

#DeclareProperty( "IsCollineation", IsProjGrpElWithFrobWithPSIsom );

DeclareCategoryCollections( "IsProjGrpElWithFrobWithPSIsom" );
InstallTrueMethod( IsGeneratorsOfMagmaWithInverses, IsProjGrpElWithFrobWithPSIsomCollection );

DeclareRepresentation( "IsProjGrpElWithFrobWithPSIsomRep", IsProjGrpElWithFrobWithPSIsom, ["mat","fld","frob","psisom"] );

BindGlobal( "ProjElsWithFrobWithPSIsomFamily", 
            NewFamily( "ProjElsWithFrobWithPSIsomFamily",IsObject,
	                 IsProjGrpElWithFrobWithPSIsom ) );  
BindGlobal( "ProjElsWithFrobWithPSIsomCollFamily",
            CollectionsFamily(ProjElsWithFrobWithPSIsomFamily) );

BindGlobal( "ProjElsWithFrobWithPSIsomType",
     NewType( ProjElsWithFrobWithPSIsomFamily, 
              IsProjGrpElWithFrobWithPSIsom and 
	      IsProjGrpElWithFrobWithPSIsomRep ) );

InstallTrueMethod( IsHandledByNiceMonomorphism, IsProjectiveGroupWithFrob );

DeclareSynonym( "IsProjGroupWithFrobWithPSIsom", IsGroup and IsProjGrpElWithFrobWithPSIsomCollection);
DeclareSynonym( "IsCorrelationCollineationGroup", IsGroup and IsProjGrpElWithFrobWithPSIsomCollection);

InstallTrueMethod( IsHandledByNiceMonomorphism, IsProjGroupWithFrobWithPSIsom );

DeclareGlobalFunction( "OnProjPointsWithFrobWithPSIsom" );
DeclareGlobalFunction( "OnProjSubspacesWithFrobWithPSIsom" );
DeclareGlobalFunction( "OnProjSubspacesExtended" );

DeclareOperation( "StandardDualityOfProjectiveSpace", [IsProjectiveSpace] );
DeclareOperation( "IdentityMappingOfElementsOfProjectiveSpace", [IsProjectiveSpace] );
DeclareOperation( "ActionOnAllPointsHyperplanes", [IsProjGroupWithFrobWithPSIsom] );    
DeclareOperation( "ProjElWithFrobWithPSIsom",
   [IsMatrix and IsFFECollColl, IsMapping, IsField] ); 
DeclareOperation( "ProjElWithFrobWithPSIsom",
   [IsMatrix and IsFFECollColl, IsMapping, IsField,
   IsStandardDualityOfProjectiveSpace] ); 
DeclareOperation( "ProjElWithFrobWithPSIsom",
   [IsMatrix and IsFFECollColl, IsMapping, IsField,
   IsGeneralMapping and IsSPGeneralMapping and IsOne] ); 
DeclareOperation( "ProjElsWithFrobWithPSIsom", [IsList, IsField] );
DeclareAttribute( "Dimension", IsProjGroupWithFrobWithPSIsom );
DeclareProperty( "CanComputeActionOnPoints", IsProjGroupWithFrobWithPSIsom );

DeclareOperation( "CorrelationOfProjectiveSpace", [ IsList, IsField] );
DeclareOperation( "CorrelationOfProjectiveSpace", [ IsList, IsMapping, IsField] );

DeclareOperation( "CorrelationOfProjectiveSpace", [ IsList, IsField, IsStandardDualityOfProjectiveSpace] );
DeclareOperation( "CorrelationOfProjectiveSpace", [ IsList, IsField, IsIdentityMappingOfElementsOfProjectiveSpace] );

DeclareOperation( "CorrelationOfProjectiveSpace", [ IsList, IsMapping, IsField, IsStandardDualityOfProjectiveSpace] );
DeclareOperation( "CorrelationOfProjectiveSpace", [ IsList, IsMapping, IsField, IsIdentityMappingOfElementsOfProjectiveSpace] );

DeclareOperation( "CorrelationOfProjectiveSpace", [ IsProjectiveSpace, IsMatrix, IsMapping, IsStandardDualityOfProjectiveSpace] );
DeclareOperation( "CorrelationOfProjectiveSpace", [ IsProjectiveSpace, IsMatrix, IsMapping, IsIdentityMappingOfElementsOfProjectiveSpace] );

DeclareOperation( "Correlation", [ IsProjectiveSpace, IsMatrix, IsMapping, IsStandardDualityOfProjectiveSpace] );
DeclareOperation( "Correlation", [ IsProjectiveSpace, IsMatrix, IsMapping, IsIdentityMappingOfElementsOfProjectiveSpace] );

DeclareOperation( "MatrixOfCorrelation", [ IsProjGrpElWithFrobWithPSIsom and IsProjGrpElWithFrobWithPSIsomRep ] );
DeclareOperation( "FieldAutomorphism", [ IsProjGrpElWithFrobWithPSIsom and IsProjGrpElWithFrobWithPSIsomRep ] );
DeclareOperation( "ProjectiveSpaceIsomorphism", [ IsProjGrpElWithFrobWithPSIsom and IsProjGrpElWithFrobWithPSIsomRep ] );


###################################################################
# Tests whether IsProjGrpElWithFrobWithPSIsom is a projectivity and so on ...
###################################################################

DeclareProperty( "IsProjectivity", IsProjGrpElWithFrobWithPSIsom );
DeclareProperty( "IsStrictlySemilinear", IsProjGrpElWithFrobWithPSIsom );
DeclareProperty( "IsCollineation", IsProjGrpElWithFrobWithPSIsom );

###################################################################
# Tests whether CorrelationCollineationGroup is a ProjectivityGroup and so on ...
###################################################################

DeclareProperty( "IsProjectivityGroup", IsProjGroupWithFrobWithPSIsom );
DeclareProperty( "IsCollineationGroup", IsProjGroupWithFrobWithPSIsom );


#############################################################################
#
# operations to create incidence preserving and reversing maps (to be placed in geometry.gd afterwards).
#
#############################################################################

#DeclareOperation( "Collineation", [IsIncidenceStructure, IsMultiplicativeElementWithInverse] );
#DeclareOperation( "Correlation", [IsIncidenceStructure, IsMultiplicativeElementWithInverse] );
#DeclareOperation( "Triality", [IsIncidenceStructure, IsMultiplicativeElementWithInverse] );

#DeclareOperation( "Polarity", [IsIncidenceStructure, IsMultiplicativeElementWithInverse] );
#DeclareOperation( "PolarityOfProjectiveSpace", [IsProjectiveSpace, IsMultiplicativeElementWithInverse] );

DeclareCategory( "IsPolarityOfProjectiveSpace", IsProjGrpElWithFrobWithPSIsomRep );
DeclareRepresentation( "IsPolarityOfProjectiveSpaceRep", IsProjGrpElWithFrobWithPSIsomRep, ["mat","fld","frob","psisom", "form"] );

#############################################################################
# polarities are equivalent with sesquilinear forms. 
# This explains the basic constructor
#############################################################################

DeclareOperation( "PolarityOfProjectiveSpaceOp", [IsForm] );
DeclareOperation( "PolarityOfProjectiveSpace", [IsForm] );
DeclareOperation( "PolarityOfProjectiveSpace", [IsMatrix,IsField and IsFinite] );
DeclareOperation( "PolarityOfProjectiveSpace", [IsMatrix,IsFrobeniusAutomorphism,IsField and IsFinite] );
DeclareOperation( "HermitianPolarityOfProjectiveSpace", [IsMatrix, IsField and IsFinite ] );

DeclareOperation( "PolarityOfProjectiveSpace", [IsClassicalPolarSpace] );

#DeclareAttribute( "IsDegeneratePolarity", IsPolarityOfProjectiveSpace ); #obsolete.
DeclareOperation( "BaseField", [ IsPolarityOfProjectiveSpace ]);
DeclareAttribute( "GramMatrix", IsPolarityOfProjectiveSpace );
DeclareAttribute( "CompanionAutomorphism", IsPolarityOfProjectiveSpace );
DeclareAttribute( "SesquilinearForm", IsPolarityOfProjectiveSpace );


#############################################################################
# operations and attributes for polarities of projective space. 
#############################################################################

DeclareProperty( "IsHermitianPolarityOfProjectiveSpace", IsPolarityOfProjectiveSpace );
DeclareProperty( "IsSymplecticPolarityOfProjectiveSpace", IsPolarityOfProjectiveSpace );
DeclareProperty( "IsOrthogonalPolarityOfProjectiveSpace", IsPolarityOfProjectiveSpace );
DeclareProperty( "IsPseudoPolarityOfProjectiveSpace", IsPolarityOfProjectiveSpace );

DeclareOperation( "IsAbsoluteElement", [ IsElementOfIncidenceStructure, IsPolarityOfProjectiveSpace ] );

DeclareOperation( "GeometryOfAbsolutePoints", [ IsPolarityOfProjectiveSpace ] );
DeclareOperation( "AbsolutePoints", [ IsPolarityOfProjectiveSpace ] );
DeclareOperation( "PolarSpace", [ IsPolarityOfProjectiveSpace ] );
