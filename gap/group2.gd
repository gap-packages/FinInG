#############################################################################
##
##  group2.gd              Desargues package
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
##  Copyright 2008 University of Western Australia, Perth
##                 Lehrstuhl D fuer Mathematik, RWTH Aachen
##                 Ghent University
##                 Colorado State University
##                 Vrije Universiteit Brussel
##
##  Declaration stuff for correlation groups
##
#############################################################################

########################################
#
# Things To Do:
#
# - reorganise
#
########################################

DeclareCategory( "IsProjectiveSpaceIsomorphism", IsSPGeneralMapping );
DeclareCategory( "IsStandardDualityOfProjectiveSpace", IsProjectiveSpaceIsomorphism );
DeclareCategory( "IsIdentityMappingOfElementsOfProjectiveSpace", IsProjectiveSpaceIsomorphism );

DeclareCategory( "IsProjGrpElWithFrobWithPSIsom", IsComponentObjectRep and IsMultiplicativeElementWithInverse );
DeclareCategoryCollections( "IsProjGrpElWithFrobWithPSIsom" );
InstallTrueMethod( IsGeneratorsOfMagmaWithInverses, IsProjGrpElWithFrobWithPSIsomCollection );

DeclareRepresentation( "IsProjGrpElWithFrobWithPSIsomRep", IsProjGrpElWithFrobWithPSIsom, ["mat","fld","frob","psisom"] );

BindGlobal( "ProjElsWithFrobWithPSIsomFamily", 
            NewFamily( "ProjElsWithFrobWithPSIsomFamily",IsObject,
	                 IsProjGrpElWithFrobWithPSIsom) );  
BindGlobal( "ProjElsWithFrobWithPSIsomCollFamily",
            CollectionsFamily(ProjElsWithFrobWithPSIsomFamily) );

BindGlobal( "ProjElsWithFrobWithPSIsomType",
     NewType( ProjElsWithFrobWithPSIsomFamily, 
              IsProjGrpElWithFrobWithPSIsom and 
	      IsProjGrpElWithFrobWithPSIsomRep) );

InstallTrueMethod( IsHandledByNiceMonomorphism, IsProjectiveGroupWithFrob );

DeclareSynonym( "IsProjGroupWithFrobWithPSIsom", IsGroup and IsProjGrpElWithFrobWithPSIsomCollection);

InstallTrueMethod( IsHandledByNiceMonomorphism, IsProjGroupWithFrobWithPSIsom );

DeclareGlobalFunction( "OnProjPointWithFrobWithPSIsom" );
DeclareGlobalFunction( "OnProjSubspacesWithFrobWithPSIsom" );
DeclareGlobalFunction( "OnProjSubspacesReversing" );

DeclareOperation( "StandardDualityOfProjectiveSpace", [IsProjectiveSpace] );
DeclareOperation( "IdentityMappingOfElementsOfProjectiveSpace", [IsProjectiveSpace] );
DeclareOperation( "ActionOnPointsHyperplanes", [IsProjGroupWithFrobWithPSIsom] );    
DeclareOperation( "ProjElWithFrobWithPSIsom",
   [IsMatrix and IsFFECollColl, IsMapping, IsField] ); 
DeclareOperation( "ProjElWithFrobWithPSIsom",
   [IsMatrix and IsFFECollColl, IsMapping, IsField,
   IsStandardDualityOfProjectiveSpace] ); 
DeclareOperation( "ProjElWithFrobWithPSIsom",
   [IsMatrix and IsFFECollColl, IsMapping, IsField,
   IsGeneralMapping and IsSPGeneralMapping and IsOne] ); 
DeclareOperation( "ProjElsWithFrobWithPSIsom", [IsList, IsField] );
DeclareOperation( "SetAsNiceMono", 
                  [IsProjGroupWithFrobWithPSIsom, IsGroupHomomorphism] );
DeclareAttribute( "Dimension", IsProjGroupWithFrobWithPSIsom );
DeclareProperty( "CanComputeActionOnPoints", IsProjGroupWithFrobWithPSIsom );
DeclareOperation( "CorrelationOfProjectiveSpace", [ IsList, IsField] );
DeclareOperation( "CorrelationOfProjectiveSpace", [ IsList, IsMapping, IsField] );
DeclareOperation( "CorrelationOfProjectiveSpace", [ IsList, IsField, IsStandardDualityOfProjectiveSpace] );
DeclareOperation( "CorrelationOfProjectiveSpace", [ IsList, IsMapping, IsField, IsStandardDualityOfProjectiveSpace] );
DeclareOperation( "UnderlyingMatrix", [ IsProjGrpElWithFrobWithPSIsom and IsProjGrpElWithFrobWithPSIsomRep ] );
DeclareOperation( "FieldAutomorphism", [ IsProjGrpElWithFrobWithPSIsom and IsProjGrpElWithFrobWithPSIsomRep ] );
DeclareOperation( "ProjectiveSpaceIsomorphism", [ IsProjGrpElWithFrobWithPSIsom and IsProjGrpElWithFrobWithPSIsomRep ] );
