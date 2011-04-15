DeclareCategory( "IsEmptySubspace", IsAttributeStoringRep );
DeclareRepresentation( "IsEmptySubspaceRep", IsEmptySubspace, ["geo", "obj"] );

DeclareOperation( "EmptySubspace", [IsProjectiveSpace] );
#DeclareOperation( "EmptySubspace", [IsSubspaceOfProjectiveSpace] );


#DeclareGlobalVariable( "EmptySubspace" );

#InstallValue( EmptySubspace, Objectify( NewType( 
#      NewFamily( "EmptySubspaceFamily" ), IsEmptySubspace ), rec() ));

#SetProjectiveDimension(EmptySubspace, -1);
#SetDimension(EmptySubspace, -1);

#SetIsTrivial(EmptySubspace, true);

DeclareOperation( "IsIncident", [IsEmptySubspace, IsSubspaceOfProjectiveSpace]);
DeclareOperation( "IsIncident", [IsSubspaceOfProjectiveSpace, IsEmptySubspace]);
DeclareOperation( "IsIncident", [IsEmptySubspace, IsProjectiveSpace]);
DeclareOperation( "IsIncident", [IsProjectiveSpace, IsEmptySubspace]);
DeclareOperation( "IsIncident", [IsEmptySubspace, IsEmptySubspace]);


DeclareOperation( "Span", [IsEmptySubspace, IsSubspaceOfProjectiveSpace]);
DeclareOperation( "Span", [IsSubspaceOfProjectiveSpace, IsEmptySubspace]);
DeclareOperation( "Span", [IsEmptySubspace, IsProjectiveSpace]);
DeclareOperation( "Span", [IsProjectiveSpace, IsEmptySubspace]);
DeclareOperation( "Span", [IsEmptySubspace, IsEmptySubspace]);


DeclareOperation( "Meet", [IsEmptySubspace, IsSubspaceOfProjectiveSpace]);
DeclareOperation( "Meet", [IsSubspaceOfProjectiveSpace, IsEmptySubspace]);
DeclareOperation( "Meet", [IsEmptySubspace, IsProjectiveSpace]);
DeclareOperation( "Meet", [IsProjectiveSpace, IsEmptySubspace]);
DeclareOperation( "Meet", [IsEmptySubspace, IsEmptySubspace]);

#DeclareAttribute( "Dimension", [IsEmptySubspace]);