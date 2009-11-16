#############################################################################
##
##  jansforms.gd              Desargues package
##                                                              John Bamberg
##                                                              Jan De Beule
##
##  Copyright 2006 University of Western Australia
##                 Lehrstuhl D fuer Mathematik
##                 Ghent University
##
##  Declaration stuff for quadratic and sesquilinear forms
##
#############################################################################


#############################################################################
# Categories and representations:
#############################################################################

BindGlobal( "FormFamily", NewFamily( "FormFamily" ) );
DeclareCategory( "IsForm", IsAttributeStoringRep );
DeclareRepresentation( "IsFormRep", IsForm,
  [ "matrix", "basefield", "type" ] );
DeclareCategory( "IsQuadraticForm", IsForm );
DeclareCategoryCollections( "IsQuadraticForm" );

DeclareCategory( "IsSesquilinearForm", IsForm );
DeclareCategoryCollections( "IsSesquilinearForm" );

DeclareCategory( "IsHermitianForm", IsForm );
DeclareCategoryCollections( "IsHermitianForm" );


BindGlobal( "QuadraticFormFamily", 
            NewFamily( "QuadraticFormFamily", IsObject, IsQuadraticForm ) );  
BindGlobal( "QuadraticFormCollFamily", CollectionsFamily(QuadraticFormFamily) );
BindGlobal( "QuadraticFormType", NewType( QuadraticFormFamily, 
                                    IsQuadraticForm and IsFormRep) );


BindGlobal( "SesquilinearFormFamily", 
            NewFamily( "SesquilinearFormFamily", IsObject, IsSesquilinearForm ) );	
BindGlobal( "SesquilinearFormCollFamily", CollectionsFamily(SesquilinearFormFamily) );
BindGlobal( "SesquilinearFormType", NewType( SesquilinearFormFamily, 
                                    IsSesquilinearForm and IsFormRep) );

BindGlobal( "HermitianFormFamily", 
            NewFamily( "HermitianFormFamily", IsObject, IsHermitianForm ) );	    
BindGlobal( "HermitianFormCollFamily", CollectionsFamily(HermitianFormFamily) );
BindGlobal( "HermitianFormType", NewType( HermitianFormFamily, 
                                    IsHermitianForm and IsFormRep) );


#############################################################################
# Constructor operations:
#############################################################################
				    
DeclareOperation( "FormByMatrix", 
                      [IsMatrix and IsFFECollColl, IsField, IsString] );
DeclareOperation( "BaseChange", [IsForm] );

DeclareOperation( "MatrixToSesquilinearForm", 
                    [ IsMatrix and IsFFECollColl, IsField, IsString ] );

DeclareOperation( "ChangeFormToCanonical", [IsForm] );
DeclareOperation( "ChangeFormToCanonicalModuloGerm", [IsForm] );
DeclareOperation( "ChangeSymplecticFormToCanonical",
          [IsMatrix and IsFFECollColl, IsField and IsFinite] );

#############################################################################
# Functions:
#############################################################################

DeclareGlobalFunction( "SWR" );
DeclareGlobalFunction( "SUM_OF_SQUARES" );
DeclareGlobalFunction( "REDUCE2" );
DeclareGlobalFunction( "REDUCE4" );
DeclareGlobalFunction( "DIFF_2_S" );
DeclareGlobalFunction( "BASE_REDUCTION1" );
DeclareGlobalFunction( "RESET" );
DeclareGlobalFunction( "SQRT2" );
DeclareGlobalFunction( "PERM_VAR" );
DeclareGlobalFunction( "C1" );
DeclareGlobalFunction( "QUAD_EQ");
DeclareGlobalFunction( "BASE_REDUCTION2" );
DeclareGlobalFunction( "HERM_CONJ" );
DeclareGlobalFunction( "BASE_REDUCTION3" );

