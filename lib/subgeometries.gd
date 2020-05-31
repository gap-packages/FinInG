#############################################################################
##
##  subgeometries.gd        FinInG package
##                                                              John Bamberg
##                                                              Anton Betten
##                                                              Jan De Beule
##                                                             Philippe Cara
##                                                            Michel Lavrauw
##                                                           Max Neunhoeffer
##
##  Copyright 2020	Colorado State University, Fort Collins
##					Università degli Studi di Padova
##					University of St. Andrews
##					University of Western Australia, Perth
##                  Vrije Universiteit Brussel
##                 
##
##  Declaration stuff for subgeometries of a projective spaces.
##
#############################################################################

#categories for sub geometries and their elements

DeclareCategory("IsSubgeometryOfProjectiveSpace", IsProjectiveSpace );

DeclareCategory("IsSubspaceOfSubgeometryOfProjectiveSpace", IsSubspaceOfProjectiveSpace );

DeclareCategoryCollections("IsSubspaceOfSubgeometryOfProjectiveSpace");

BindGlobal( "SoSoPSFamily",
  NewFamily( "SoSoPSFamily", IsSubspaceOfSubgeometryOfProjectiveSpace, IsSubspaceOfSubgeometryOfProjectiveSpace));

BindGlobal( "SoSoPSFamilyCollFamily", CollectionsFamily(SoSoPSFamily) );

DeclareCategory( "IsSubspacesOfSubgeometryOfProjectiveSpace", IsElementsOfLieGeometry );

DeclareRepresentation( "IsSubspacesOfSubgeometryOfProjectiveSpaceRep", IsElementsOfLieGeometryRep, [ "geometry", "type" ] );

BindGlobal( "SubgeometriesFamily", NewFamily( "SubgeometriesFamily" ) );

DeclareCategory( "IsFlagOfSubgeometryOfProjectiveSpace", IsFlagOfProjectiveSpace );

BindGlobal( "FlagsOfSgOPS", NewFamily( "FlagsOfSgOPSFamily", IsObject ));

BindGlobal( "IsFlagsOfSgOPSType", NewType( FlagsOfSgOPS,
                                    IsFlagOfSubgeometryOfProjectiveSpace and IsFlagOfIncidenceStructureRep) );


#representations

DeclareRepresentation( "IsSubgeometryOfProjectiveSpaceRep", IsSubgeometryOfProjectiveSpace, [ "dimension", "basefield", "subfield", "ambientspace", "isomorphicsubgeometry", "frame", "proj", "sigma" ] );

DeclareRepresentation( "IsSubspaceOfSubgeometryOfProjectiveSpaceRep", IsElementsOfLieGeometryRep, [ "geometry", "type" ] );

#operations

#two helper operations for frames

DeclareOperation("IsFrameOfProjectiveSpace", [IsList]);
DeclareOperation("RandomFrameOfProjectiveSpace", [ IsProjectiveSpace ] );

#a helper function for VectorSpaceToElement

DeclareOperation("VectorSpaceToElementForSubgeometries", [ IsSubgeometryOfProjectiveSpace, IsObject ]);

#constructors for subgeometries

DeclareOperation("CanonicalSubgeometryOfProjectiveSpace", [IsProjectiveSpace, IsField and IsFinite]);
DeclareOperation("CanonicalSubgeometryOfProjectiveSpace", [IsProjectiveSpace, IsPosInt]);

DeclareOperation("SubgeometryOfProjectiveSpaceByFrame", [IsProjectiveSpace, IsList, IsPosInt]);
DeclareOperation("SubgeometryOfProjectiveSpaceByFrame", [IsProjectiveSpace, IsList, IsField and IsFinite]);

#DeclareOperation( "VectorSpaceToElement", [IsSubgeometryOfProjectiveSpace, IsObject] );

DeclareOperation("ExtendElementOfSubgeometry", [ IsSubspaceOfSubgeometryOfProjectiveSpace ]);

#DeclareOperation("UnderlyingVectorSpace", [ IsSubspaceOfSubgeometryOfProjectiveSpace ]);

DeclareOperation("SubfieldOfSubgeometry", [ IsSubgeometryOfProjectiveSpace ]);


#attributes

#DeclareAttribute( "StandardFrame", IsSubgeometryOfProjectiveSpace );

DeclareAttribute( "IsCanonicalSubgeometryOfProjectiveSpace", IsSubgeometryOfProjectiveSpace);
DeclareAttribute( "DefiningFrameOfSubgeometry", IsSubgeometryOfProjectiveSpace);

DeclareCategory( "IsShadowSubspacesOfSubgeometryOfProjectiveSpace", IsShadowElementsOfLieGeometry );
DeclareRepresentation( "IsShadowSubspacesOfSubgeometryOfProjectiveSpaceRep", IsShadowElementsOfLieGeometryRep, [ "geometry", "type", "inner", "outer", "factorspace" ]);

DeclareAttribute( "CollineationFixingSubgeometry", IsSubgeometryOfProjectiveSpace);


#action functions


DeclareGlobalFunction( "OnProjSubspacesOfSubgeometryNC" );
DeclareGlobalFunction( "OnProjSubspacesOfSubgeometry" );

DeclareGlobalFunction( "OnSubgeometryOfProjectiveSpace" );

DeclareAttribute( "DefaultGeometry", IsProjectiveGroupWithFrob );

