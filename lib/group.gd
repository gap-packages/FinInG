#############################################################################
##
##  group.gd              FinInG package
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
##  Declaration stuff for groups
##
#############################################################################

DeclareGlobalFunction("MakeAllProjectivePoints");
DeclareGlobalFunction("IsFiningScalarMatrix");

## For later versions of GenSS, Max has changed the number of variables
## for the operation FindBasePointCandidates.

if PackageInfo("GenSS")[1]!.Version > "0.95" then
   DeclareOperation( "FindBasePointCandidates", [ IsGroup, IsRecord, IsInt ] );
 else 
   DeclareOperation( "FindBasePointCandidates", [ IsGroup, IsRecord, IsInt, IsObject ] );
fi;

###################################################################
# Construction operations for projective elements, that is matrices modulo scalars
# including representations
###################################################################

DeclareCategory( "IsProjGrpEl", IsComponentObjectRep and IsMultiplicativeElementWithInverse );
DeclareCategoryCollections( "IsProjGrpEl" );
InstallTrueMethod( IsGeneratorsOfMagmaWithInverses, IsProjGrpElCollection );

DeclareRepresentation( "IsProjGrpElRep", IsProjGrpEl, ["mat","fld"] );

BindGlobal( "ProjElsFamily", 
            NewFamily( "ProjElsFamily", IsObject, IsProjGrpEl ) );
BindGlobal( "ProjElsCollFamily", CollectionsFamily(ProjElsFamily) );

BindGlobal( "ProjElsType", NewType( ProjElsFamily, 
                                    IsProjGrpEl and IsProjGrpElRep) );

DeclareOperation( "ProjEl", [IsMatrix and IsFFECollColl] );
DeclareOperation( "ProjEls", [IsList] );

#InstallTrueMethod(IsHandledByNiceMonomorphism, IsProjectivityGroup);

DeclareOperation( "Projectivity", [ IsList, IsField] );
DeclareOperation( "Projectivity", [ IsProjectiveSpace, IsMatrix] );


###################################################################
# Construction operations for projective semilinear elements, that is matrices modulo scalars
# and a Frobenius automorphism, including representations
###################################################################

DeclareCategory( "IsProjGrpElWithFrob", IsComponentObjectRep and IsMultiplicativeElementWithInverse );
DeclareCategoryCollections( "IsProjGrpElWithFrob" );
InstallTrueMethod( IsGeneratorsOfMagmaWithInverses, IsProjGrpElWithFrobCollection );

DeclareRepresentation( "IsProjGrpElWithFrobRep", IsProjGrpElWithFrob, ["mat","fld","frob"] );

BindGlobal( "ProjElsWithFrobFamily", 
            NewFamily( "ProjElsWithFrobFamily",IsObject,IsProjGrpElWithFrob) );
BindGlobal( "ProjElsWithFrobCollFamily",
            CollectionsFamily(ProjElsWithFrobFamily) );
BindGlobal( "ProjElsWithFrobType",
     NewType( ProjElsWithFrobFamily, 
              IsProjGrpElWithFrob and IsProjGrpElWithFrobRep) );

DeclareOperation( "ProjElWithFrob", [IsMatrix and IsFFECollColl, IsMapping] );
DeclareOperation( "ProjElWithFrob", [IsMatrix and IsFFECollColl, IsMapping, IsField] );
DeclareOperation( "ProjElsWithFrob", [IsList] );
DeclareOperation( "ProjElsWithFrob", [IsList, IsField] );

#DeclareOperation( "ProjectiveSemilinearMap", [ IsList, IsField] ); # no longer valid (ml 8/11/12)
DeclareOperation( "CollineationOfProjectiveSpace", [ IsList, IsField] );
DeclareOperation( "CollineationOfProjectiveSpace", [ IsList, IsMapping, IsField] );
DeclareOperation( "CollineationOfProjectiveSpace", [ IsProjectiveSpace, IsMatrix] );
DeclareOperation( "CollineationOfProjectiveSpace", [ IsProjectiveSpace, IsMatrix, IsMapping] );
DeclareOperation( "CollineationOfProjectiveSpace", [ IsProjectiveSpace, IsMapping] );

DeclareOperation( "Collineation", [IsProjectiveSpace, IsMatrix] );
DeclareOperation( "Collineation", [IsProjectiveSpace, IsMatrix, IsMapping] );

DeclareOperation( "ProjectiveSemilinearMap", [ IsList, IsMapping, IsField] );
#DeclareSynonym( "CollineationOfProjectiveSpace", ProjectiveSemilinearMap); # no longer valid (ml 8/11/12)

DeclareOperation( "ProjectivityByImageOfStandardFrameNC", [IsProjectiveSpace, IsList] );

###################################################################
# Tests whether collineation is a projectivity and so on ...
###################################################################

DeclareProperty( "IsProjectivity", IsProjGrpEl );
DeclareProperty( "IsProjectivity", IsProjGrpElWithFrob );

DeclareProperty( "IsStrictlySemilinear", IsProjGrpEl );
DeclareProperty( "IsStrictlySemilinear", IsProjGrpElWithFrob );

DeclareProperty( "IsCollineation", IsProjGrpEl );
DeclareProperty( "IsCollineation", IsProjGrpElWithFrob );


###################################################################
# Some operations for elements
###################################################################

DeclareOperation( "MatrixOfCollineation", [ IsProjGrpElWithFrob and IsProjGrpElWithFrobRep ] );
DeclareOperation( "MatrixOfCollineation", [ IsProjGrpEl and IsProjGrpElRep] );
DeclareOperation( "FieldAutomorphism", [ IsProjGrpElWithFrob and IsProjGrpElWithFrobRep ] );

#################################################
# Frobenius automorphisms and groups using them:
#################################################

DeclareGlobalFunction( "OnProjPoints" );
DeclareGlobalFunction( "OnProjSubspacesNoFrob" );

# the following are not necessary, since the FinInG projectivity group is constructed
# as a projective semilinear group (i.e. a collineation group), and for these
# groups we have the operations defined (ml 05/11/2012)
#DeclareOperation( "ActionOnAllProjPoints", [IsProjectivityGroup] );
#DeclareAttribute( "Dimension", IsProjectivityGroup );
#DeclareProperty( "CanComputeActionOnPoints", IsProjectivityGroup );


#DeclareSynonym( "IsProjectiveSemilinearGroup", IsGroup and IsProjGrpElWithFrobCollection);
DeclareSynonym( "IsProjectiveGroupWithFrob", IsGroup and IsProjGrpElWithFrobCollection);

DeclareProperty( "IsProjectivityGroup", IsProjectiveGroupWithFrob );
DeclareProperty( "IsCollineationGroup", IsProjectiveGroupWithFrob );



#################################################
# action functions:
#################################################

InstallTrueMethod( IsHandledByNiceMonomorphism, IsProjectiveGroupWithFrob );

DeclareGlobalFunction( "OnProjSubspaces" );
DeclareGlobalFunction( "OnSetsProjSubspaces" );

DeclareGlobalFunction( "OnProjPointsWithFrob" );
DeclareGlobalFunction( "OnProjSubspacesWithFrob" );

DeclareOperation( "ActionOnAllProjPoints", [IsProjectiveGroupWithFrob] );

DeclareAttribute( "Dimension", IsProjectiveGroupWithFrob );
DeclareProperty( "CanComputeActionOnPoints", IsProjectiveGroupWithFrob );

DeclareGlobalFunction( "NiceMonomorphismByOrbit" );
DeclareGlobalFunction( "NiceMonomorphismByDomain" );

###########################
# Some group constructions:
###########################

## helper operations for canonical matrices, for classical groups.

DeclareOperation( "CanonicalGramMatrix", [IsString, IsPosInt, IsField]); 
DeclareOperation( "CanonicalQuadraticForm", [IsString, IsPosInt, IsField]); 

## The following are conjugates of the groups in the classical groups
## library which are compatible with the canonical forms in FinInG 

DeclareOperation( "SOdesargues", [IsInt, IsPosInt, IsField and IsFinite]);
DeclareOperation( "GOdesargues", [IsInt, IsPosInt, IsField and IsFinite]);
DeclareOperation( "SUdesargues", [IsPosInt, IsField and IsFinite]);
DeclareOperation( "GUdesargues", [IsPosInt, IsField and IsFinite]);
DeclareOperation( "Spdesargues", [IsPosInt, IsField and IsFinite]);

## The following are methods which return the full group of invertible 
## matrices which preserve a form up to scalars

DeclareOperation( "GeneralSymplecticGroup", [IsPosInt, IsField and IsFinite]);
DeclareOperation( "GSpdesargues", [IsPosInt, IsField and IsFinite]);
DeclareOperation( "DeltaOminus", [IsPosInt, IsField and IsFinite]);
DeclareOperation( "DeltaOplus", [IsPosInt, IsField and IsFinite]);

## The following are methods which return the full group of invertible 
## matrices which preserve a form up to scalars and a field aut.

DeclareOperation( "GammaOminus", [IsPosInt, IsField and IsFinite]);
DeclareOperation( "GammaO", [IsPosInt, IsField and IsFinite]);
DeclareOperation( "GammaOplus", [IsPosInt, IsField and IsFinite]);
DeclareOperation( "GammaU", [IsPosInt, IsField and IsFinite]);
DeclareOperation( "GammaSp", [IsPosInt, IsField and IsFinite]);



