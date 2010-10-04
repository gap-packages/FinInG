#############################################################################
##
##  group.gd              FinInG package
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
##  Declaration stuff for some new group representations
##
##  Declaration stuff for groups. changed by the new gang of four september
##  25, 2008, st. andrews.
#############################################################################

DeclareGlobalFunction("MakeAllProjectivePoints");

###################################################################
# Code for "projective elements", that is matrices modulo scalars:
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

DeclareSynonym( "IsProjectiveGroup", IsGroup and IsProjGrpElCollection);

InstallTrueMethod(IsHandledByNiceMonomorphism, IsProjectiveGroup);

DeclareGlobalFunction( "OnProjPoints" );
DeclareGlobalFunction( "OnProjSubspacesNoFrob" );

DeclareOperation( "ActionOnAllProjPoints", [IsProjectiveGroup] );
DeclareOperation( "SetAsNiceMono", [IsProjectiveGroup, IsGroupHomomorphism] );

DeclareAttribute( "Dimension", IsProjectiveGroup );
DeclareProperty( "CanComputeActionOnPoints", IsProjectiveGroup );

#################################################
# Frobenius automorphisms and groups using them:
#################################################

DeclareCategory( "IsProjGrpElWithFrob", IsComponentObjectRep and IsMultiplicativeElementWithInverse );
DeclareCategoryCollections( "IsProjGrpElWithFrob" );
InstallTrueMethod( IsGeneratorsOfMagmaWithInverses, 
                 IsProjGrpElWithFrobCollection );

DeclareRepresentation( "IsProjGrpElWithFrobRep", IsProjGrpElWithFrob, ["mat","fld","frob"] );

BindGlobal( "ProjElsWithFrobFamily", 
            NewFamily( "ProjElsWithFrobFamily",IsObject,IsProjGrpElWithFrob) );
BindGlobal( "ProjElsWithFrobCollFamily",
            CollectionsFamily(ProjElsWithFrobFamily) );
BindGlobal( "ProjElsWithFrobType",
     NewType( ProjElsWithFrobFamily, 
              IsProjGrpElWithFrob and IsProjGrpElWithFrobRep) );

DeclareOperation( "ProjElWithFrob",
   [IsMatrix and IsFFECollColl, IsMapping] );
DeclareOperation( "ProjElWithFrob",
   [IsMatrix and IsFFECollColl, IsMapping, IsField] );
DeclareOperation( "ProjElsWithFrob", [IsList] );
DeclareOperation( "ProjElsWithFrob", [IsList, IsField] );
DeclareOperation( "Projectivity", [ IsList, IsField] );

DeclareOperation( "ProjectiveSemilinearMap", [ IsList, IsField] );
DeclareOperation( "ProjectiveSemilinearMap", [ IsList, IsMapping, IsField] );
DeclareSynonym( "CollineationOfProjectiveSpace", ProjectiveSemilinearMap);

DeclareOperation( "ProjectivityByImageOfStandardFrameNC", [IsProjectiveSpace, IsList] );

DeclareOperation( "UnderlyingMatrix", [ IsProjGrpEl and IsProjGrpElRep] );
DeclareOperation( "UnderlyingMatrix", [ IsProjGrpElWithFrob and IsProjGrpElWithFrobRep ] );
DeclareOperation( "FieldAutomorphism", [ IsProjGrpElWithFrob and IsProjGrpElWithFrobRep ] );

DeclareSynonym( "IsProjectiveGroupWithFrob", 
  IsGroup and IsProjGrpElWithFrobCollection);

InstallTrueMethod( IsHandledByNiceMonomorphism, IsProjectiveGroupWithFrob );

DeclareGlobalFunction( "OnProjPointsWithFrob" );
DeclareGlobalFunction( "OnProjSubspacesWithFrob" );

DeclareOperation( "ActionOnAllProjPoints", [IsProjectiveGroupWithFrob] );
DeclareOperation( "SetAsNiceMono", [IsProjectiveGroupWithFrob, IsGroupHomomorphism] );

DeclareAttribute( "Dimension", IsProjectiveGroupWithFrob );
DeclareProperty( "CanComputeActionOnPoints", IsProjectiveGroupWithFrob );

DeclareGlobalFunction( "NiceMonomorphismByOrbit" );
DeclareGlobalFunction( "NiceMonomorphismByDomain" );

###########################
# Some group constructions:
###########################

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
