DeclareCategory( "IsStandardDualityOfVectorSpace", IsSPGeneralMapping);
DeclareCategory( "IsStandardDualityOfProjectiveSpace", IsSPGeneralMapping);
DeclareCategory( "IsProjGrpElWithFrobWithVSIsom", 
                 IsComponentObjectRep and IsMultiplicativeElementWithInverse );
DeclareCategoryCollections( "IsProjGrpElWithFrobWithVSIsom" );
InstallTrueMethod( IsGeneratorsOfMagmaWithInverses, IsProjGrpElWithFrobWithVSIsomCollection );

DeclareRepresentation( "IsProjGrpElWithFrobWithVSIsomRep", IsProjGrpElWithFrobWithVSIsom, 
		       ["mat","fld","frob","vsisom"] );

BindGlobal( "ProjElsWithFrobWithVSIsomFamily", 
            NewFamily( "ProjElsWithFrobWithVSIsomFamily",IsObject,
	                 IsProjGrpElWithFrobWithVSIsom) );  
BindGlobal( "ProjElsWithFrobWithVSIsomCollFamily",
            CollectionsFamily(ProjElsWithFrobWithVSIsomFamily) );

BindGlobal( "ProjElsWithFrobWithVSIsomType",
     NewType( ProjElsWithFrobWithVSIsomFamily, 
              IsProjGrpElWithFrobWithVSIsom and 
	      IsProjGrpElWithFrobWithVSIsomRep) );

InstallTrueMethod( IsHandledByNiceMonomorphism, IsProjectiveGroupWithFrob );

DeclareGlobalFunction( "StandardDualityOfVectorSpace" );
DeclareGlobalFunction( "OnPointsHyperplanesWithFrobWithVSIsom" );

DeclareOperation( "StandardDualityOfProjectiveSpace", [IsProjectiveSpace] );

DeclareSynonym( "IsProjGroupWithFrobWithVSIsom", IsGroup and IsProjGrpElWithFrobWithVSIsomCollection);
InstallTrueMethod( IsHandledByNiceMonomorphism, IsProjGroupWithFrobWithVSIsom );

DeclareOperation( "ActionOnPointsHyperplanes", [IsProjGroupWithFrobWithVSIsom] );
	      
DeclareOperation( "ProjElWithFrobWithVSIsom",
   [IsMatrix and IsFFECollColl, IsMapping, IsField] ); 

DeclareOperation( "ProjElWithFrobWithVSIsom",
   [IsMatrix and IsFFECollColl, IsMapping, IsField,
   IsStandardDualityOfVectorSpace] ); 

DeclareOperation( "ProjElWithFrobWithVSIsom",
   [IsMatrix and IsFFECollColl, IsMapping, IsField,
   IsGeneralMapping and IsSPGeneralMapping and IsOne] ); 

DeclareOperation( "ProjElsWithFrobWithVSIsom", [IsList, IsField] );

DeclareOperation( "SetAsNiceMono", 
                  [IsProjGroupWithFrobWithVSIsom, IsGroupHomomorphism] );

DeclareAttribute( "Dimension", IsProjGroupWithFrobWithVSIsom );
DeclareProperty( "CanComputeActionOnPoints", IsProjGroupWithFrobWithVSIsom );
