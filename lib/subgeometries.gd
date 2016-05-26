#categories for geometries.

DeclareCategory("IsSubgeometryOfProjectiveSpace", IsProjectiveSpace );

DeclareCategory("IsSubspaceOfSubgeometryOfProjectiveSpace", IsSubspaceOfProjectiveSpace );

DeclareCategory( "IsSubspacesOfSubgeometryOfProjectiveSpace", IsElementsOfLieGeometry );

DeclareRepresentation( "IsSubspacesOfSubgeometryOfProjectiveSpaceRep", IsElementsOfLieGeometryRep, [ "geometry", "type" ] );

BindGlobal( "SubgeometriesFamily", NewFamily( "SubgeometriesFamily" ) );

#representations

DeclareRepresentation( "IsSubgeometryOfProjectiveSpaceRep", IsSubgeometryOfProjectiveSpace, [ "dimension", "basefield", "ambientspace", "isomorphicsubgeometry", "frame", "proj", "sigma" ] );

DeclareRepresentation( "IsSubspaceOfSubgeometryOfProjectiveSpaceRep", IsElementsOfLieGeometryRep, [ "geometry", "type" ] );


#operations

DeclareOperation("IsFrameOfProjectiveSpace", [IsList]);

DeclareOperation("CanonicalSubgeometryOfProjectiveSpace", [IsProjectiveSpace, IsField and IsFinite]);
DeclareOperation("CanonicalSubgeometryOfProjectiveSpace", [IsProjectiveSpace, IsPosInt]);

DeclareOperation("SubgeometryOfProjectiveSpaceByFrame", [IsProjectiveSpace, IsList, IsPosInt]);
DeclareOperation("SubgeometryOfProjectiveSpaceByFrame", [IsProjectiveSpace, IsList, IsField and IsFinite]);

DeclareOperation( "VectorSpaceToElement", [IsSubgeometryOfProjectiveSpace, IsObject] );

DeclareOperation("ExtendElementOfSubgeometry", [ IsSubspaceOfSubgeometryOfProjectiveSpace ]);

#attributes

DeclareAttribute( "IsCanonicalSubgeometryOfProjectiveSpace", IsSubgeometryOfProjectiveSpace);
