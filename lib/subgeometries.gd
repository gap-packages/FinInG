#categories for geometries.

DeclareCategory("IsSubgeometryOfProjectiveSpace", IsProjectiveSpace );

DeclareCategory("IsSubspaceOfSubgeometryOfProjectiveSpace", IsSubspaceOfProjectiveSpace );

DeclareCategoryCollections("IsSubspaceOfSubgeometryOfProjectiveSpace");

BindGlobal( "SoSoPSFamily",
  NewFamily( "SoSoPSFamily", IsSubspaceOfSubgeometryOfProjectiveSpace, IsSubspaceOfSubgeometryOfProjectiveSpace));

BindGlobal( "SoSoPSFamilyCollFamily", CollectionsFamily(SoSoPSFamily) );

DeclareCategory( "IsSubspacesOfSubgeometryOfProjectiveSpace", IsElementsOfLieGeometry );

DeclareRepresentation( "IsSubspacesOfSubgeometryOfProjectiveSpaceRep", IsElementsOfLieGeometryRep, [ "geometry", "type" ] );

BindGlobal( "SubgeometriesFamily", NewFamily( "SubgeometriesFamily" ) );

#representations

DeclareRepresentation( "IsSubgeometryOfProjectiveSpaceRep", IsSubgeometryOfProjectiveSpace, [ "dimension", "basefield", "subfield", "ambientspace", "isomorphicsubgeometry", "frame", "proj", "sigma" ] );

DeclareRepresentation( "IsSubspaceOfSubgeometryOfProjectiveSpaceRep", IsElementsOfLieGeometryRep, [ "geometry", "type" ] );

#operations

DeclareOperation("IsFrameOfProjectiveSpace", [IsList]);

DeclareOperation("CanonicalSubgeometryOfProjectiveSpace", [IsProjectiveSpace, IsField and IsFinite]);
DeclareOperation("CanonicalSubgeometryOfProjectiveSpace", [IsProjectiveSpace, IsPosInt]);

DeclareOperation("SubgeometryOfProjectiveSpaceByFrame", [IsProjectiveSpace, IsList, IsPosInt]);
DeclareOperation("SubgeometryOfProjectiveSpaceByFrame", [IsProjectiveSpace, IsList, IsField and IsFinite]);

#DeclareOperation( "VectorSpaceToElement", [IsSubgeometryOfProjectiveSpace, IsObject] );

DeclareOperation("ExtendElementOfSubgeometry", [ IsSubspaceOfSubgeometryOfProjectiveSpace ]);

#DeclareOperation("UnderlyingVectorSpace", [ IsSubspaceOfSubgeometryOfProjectiveSpace ]);

DeclareOperation("SubFieldOFSubGeometry", [ IsSubgeometryOfProjectiveSpace ]);

DeclareOperation("VectorSpaceToElementForSubgeometries", [ IsSubgeometryOfProjectiveSpace, IsObject ]);

#attributes

DeclareAttribute( "IsCanonicalSubgeometryOfProjectiveSpace", IsSubgeometryOfProjectiveSpace);

DeclareCategory( "IsShadowSubspacesOfSubgeometryOfProjectiveSpace", IsShadowElementsOfLieGeometry );
DeclareRepresentation( "IsShadowSubspacesOfSubgeometryOfProjectiveSpaceRep", IsShadowElementsOfLieGeometryRep, [ "geometry", "type", "inner", "outer", "factorspace" ]);


DeclareGlobalFunction( "OnProjSubspacesOfSubgeometriesNC" );
DeclareGlobalFunction( "OnProjSubspacesOfSubgeometries" );

