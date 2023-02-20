#############################################################################
##
##  liegeometry.gd            FinInG package
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
##  Declaration stuff for Lie geometries.
##
#############################################################################

#############################################################################
# Constructor operations:
#############################################################################

## none yet

#############################################################################
#
# Elements of Lie geometries -  definitions and operations:
#
#############################################################################

DeclareCategory( "IsElementOfLieGeometry", IsElementOfIncidenceGeometry );
DeclareCategoryCollections("IsElementOfLieGeometry");

DeclareCategory( "IsElementsOfLieGeometry", IsElementsOfIncidenceGeometry );
DeclareCategory( "IsAllElementsOfLieGeometry", IsAllElementsOfIncidenceGeometry );

DeclareRepresentation( "IsAllElementsOfLieGeometryRep", IsAllElementsOfIncidenceStructureRep, [ "geometry", "type" ] );
  
DeclareRepresentation( "IsElementsOfLieGeometryRep", IsElementsOfIncidenceStructureRep, [ "geometry", "type" ] );

DeclareCategory( "IsFlagOfLieGeometry", IsFlagOfIncidenceGeometry );

#DeclareRepresentation( "IsFlagOfLieGeometryRep", IsFlagOfLieGeometry, [ "geo", "types", "els" ] );

DeclareCategory( "IsEmptySubspace", IsAttributeStoringRep );
DeclareRepresentation( "IsEmptySubspaceRep", IsEmptySubspace, ["geo", "obj"] );

DeclareCategory( "IsShadowElementsOfLieGeometry", IsElementsOfIncidenceStructure );
DeclareRepresentation( "IsShadowElementsOfLieGeometryRep", IsElementsOfIncidenceStructure, [ "geometry", "type", "inner", "outer", "factorspace", "parentflag" ]);

DeclareOperation( "UnderlyingVectorSpace", [IsLieGeometry] );
DeclareOperation( "UnderlyingVectorSpace", [IsElementOfLieGeometry] );
DeclareOperation( "UnderlyingVectorSpace", [IsFlagOfLieGeometry]	); #came from projectivespaces.gd

DeclareAttribute( "AmbientSpace", IsLieGeometry );
DeclareAttribute( "AmbientSpace", IsElementOfLieGeometry );

DeclareAttribute( "ProjectiveDimension", IsLieGeometry ); #next three: more general decl in liegeometry.gd
DeclareAttribute( "ProjectiveDimension", IsElementOfLieGeometry );
DeclareAttribute( "ProjectiveDimension", IsEmptySubspace );

DeclareAttribute( "Dimension", IsLieGeometry );

DeclareOperation( "VectorSpaceToElement", [IsLieGeometry, IsRowVector] );
DeclareOperation( "VectorSpaceToElement", [IsLieGeometry, Is8BitVectorRep] );
DeclareOperation( "VectorSpaceToElement", [IsLieGeometry, IsPlistRep] );
DeclareOperation( "VectorSpaceToElement", [IsLieGeometry, Is8BitMatrixRep] );
DeclareOperation( "VectorSpaceToElement", [IsLieGeometry, IsGF2MatrixRep] );

#new on 18/3/2014 and 20/3/14.
DeclareOperation( "VectorSpaceToElement", [IsLieGeometry, IsCVecRep] );
DeclareOperation( "VectorSpaceToElement", [IsLieGeometry, IsCMatRep] );


#DeclareAttribute( "UnderlyingObject", IsElementOfLieGeometry ); #made more general and moved to geometry.gd
#DeclareOperation( "EmptySubspace", [IsClassicalPolarSpace] );
#DeclareOperation( "EmptySubspace", [IsProjectiveSpace] );
DeclareOperation( "EmptySubspace", [IsLieGeometry] );

#jdb 30/10/15: This method is not used anymore after commenting out the Unwrapper stuff in geometry.gd
#see the comment there.
#DeclareOperation( "\^", [IsEmptySubspace, IsUnwrapper] );

DeclareOperation("RandomSubspace",[IsVectorSpace,IsInt]); #is for vector spaces -> moves to liegeometry.gd


#DeclareGlobalFunction( "OnProjSubspaces" );
#DeclareGlobalFunction( "OnSetsProjSubspaces" );

#DeclareSynonym( "OnLieVarieties", OnProjSubspaces );
#DeclareSynonym( "OnSetsLieVarieties", OnSetsProjSubspaces );

## Shadows

#DeclareOperation( "Points", [ IsElementOfLieGeometry ] );
#DeclareOperation( "Lines", [ IsElementOfLieGeometry ] );
#DeclareOperation( "Planes", [ IsElementOfLieGeometry ] );
#DeclareOperation( "Solids", [ IsElementOfLieGeometry ] );
#DeclareOperation( "Hyperplanes", [ IsElementOfLieGeometry ] );

#DeclareOperation( "Points", [ IsLieGeometry, IsElementOfLieGeometry ] );
#DeclareOperation( "Lines", [ IsLieGeometry, IsElementOfLieGeometry ] );
#DeclareOperation( "Planes", [ IsLieGeometry, IsElementOfLieGeometry ] );
#DeclareOperation( "Solids", [ IsLieGeometry, IsElementOfLieGeometry ] );
#DeclareOperation( "Hyperplanes", [ IsLieGeometry, IsElementOfLieGeometry ] );

#more for EmptySubspace

#was:

#DeclareOperation( "IsIncident", [IsEmptySubspace, IsSubspaceOfProjectiveSpace]);
#DeclareOperation( "IsIncident", [IsSubspaceOfProjectiveSpace, IsEmptySubspace]);
#DeclareOperation( "IsIncident", [IsEmptySubspace, IsProjectiveSpace]);
#DeclareOperation( "IsIncident", [IsProjectiveSpace, IsEmptySubspace]);
#DeclareOperation( "IsIncident", [IsEmptySubspace, IsEmptySubspace]);


#DeclareOperation( "Span", [IsEmptySubspace, IsSubspaceOfProjectiveSpace]);
#DeclareOperation( "Span", [IsSubspaceOfProjectiveSpace, IsEmptySubspace]);
#DeclareOperation( "Span", [IsEmptySubspace, IsProjectiveSpace]);
#DeclareOperation( "Span", [IsProjectiveSpace, IsEmptySubspace]);
#DeclareOperation( "Span", [IsEmptySubspace, IsEmptySubspace]);


#DeclareOperation( "Meet", [IsEmptySubspace, IsSubspaceOfProjectiveSpace]);
#DeclareOperation( "Meet", [IsSubspaceOfProjectiveSpace, IsEmptySubspace]);
#DeclareOperation( "Meet", [IsEmptySubspace, IsProjectiveSpace]);
#DeclareOperation( "Meet", [IsProjectiveSpace, IsEmptySubspace]);
#DeclareOperation( "Meet", [IsEmptySubspace, IsEmptySubspace]);

#became (6/0 AD 2011, jdb)

DeclareOperation( "IsIncident", [IsEmptySubspace, IsElementOfLieGeometry]);
DeclareOperation( "IsIncident", [IsElementOfLieGeometry, IsEmptySubspace]);
DeclareOperation( "IsIncident", [IsEmptySubspace, IsLieGeometry]);
DeclareOperation( "IsIncident", [IsLieGeometry, IsEmptySubspace]);
DeclareOperation( "IsIncident", [IsEmptySubspace, IsEmptySubspace]);

DeclareOperation( "Span", [IsEmptySubspace, IsElementOfLieGeometry]);
DeclareOperation( "Span", [IsElementOfLieGeometry, IsEmptySubspace]);
DeclareOperation( "Span", [IsEmptySubspace, IsLieGeometry]);
DeclareOperation( "Span", [IsLieGeometry, IsEmptySubspace]);
DeclareOperation( "Span", [IsEmptySubspace, IsEmptySubspace]);

DeclareOperation( "Span", [ IsList ]); #moved here on 27/5/16

DeclareOperation( "Meet", [IsEmptySubspace, IsElementOfLieGeometry]);
DeclareOperation( "Meet", [IsElementOfLieGeometry, IsEmptySubspace]);
DeclareOperation( "Meet", [IsEmptySubspace, IsLieGeometry]);
DeclareOperation( "Meet", [IsLieGeometry, IsEmptySubspace]);
DeclareOperation( "Meet", [IsEmptySubspace, IsEmptySubspace]);

#something new after a Brussels day...
DeclareOperation( "ElementToElement", [IsLieGeometry, IsElementOfLieGeometry]);
DeclareSynonym( "Embed", ElementToElement );
DeclareOperation( "ConvertElement", [IsLieGeometry, IsElementOfLieGeometry]);
DeclareOperation( "ConvertElementNC", [IsLieGeometry, IsElementOfLieGeometry]);

