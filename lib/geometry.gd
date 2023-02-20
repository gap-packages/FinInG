#############################################################################
##
##  geometry.gd              FinInG package
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
##  Declaration stuff for geometries.
##
#############################################################################

#############################################################################
# 
# An info class:
#
#############################################################################

DeclareInfoClass( "InfoFinInG" );
SetInfoLevel( InfoFinInG, 1 );


#############################################################################
#
# A global place for variables for the FinInG package:
#
#############################################################################

BindGlobal( "FINING", rec() );


#############################################################################
#
# The top level, all geometries we study are incidence structures:
#
#############################################################################

# categories for incidence structures.

DeclareCategory( "IsIncidenceStructure", IsComponentObjectRep and IsAttributeStoringRep );

# categories for elements of incidence structures.

DeclareCategory( "IsElementOfIncidenceStructure", IsComponentObjectRep and IsAttributeStoringRep
											and CanEasilyCompareElements and CanEasilySortElements );

#DeclareCategory( "IsElementsOfIncidenceStructure", IsDomain and IsCollection and IsComponentObjectRep );
#DeclareCategory( "IsAllElementsOfIncidenceStructure", IsDomain and IsCollection and IsComponentObjectRep);

DeclareCategory( "IsAnyElementsOfIncidenceStructure", IsDomain and IsCollection and IsComponentObjectRep );
   ## e.g.,  Points(PG(3,4)) or ElementsOfIncidenceStructure(PG(3,4));

DeclareCategory( "IsElementsOfIncidenceStructure", IsAnyElementsOfIncidenceStructure );
   ## e.g.,  Points(PG(3,4)) 

DeclareCategory( "IsAllElementsOfIncidenceStructure", IsAnyElementsOfIncidenceStructure);
   ## e.g.,  ElementsOfIncidenceStructure(PG(3,4));

# Note that this implies that we have to implement \= and \< for all
# types of Elements!

DeclareCategory( "IsFlagOfIncidenceStructure", IsAttributeStoringRep );
DeclareAttribute( "IsChamberOfIncidenceStructure", IsFlagOfIncidenceStructure );
DeclareAttribute( "IsEmptyFlag", IsFlagOfIncidenceStructure);

#############################################################################
#
# Abstract incidence geometries:
#
#############################################################################


# categories for incidence geometries
DeclareCategory( "IsIncidenceGeometry", IsIncidenceStructure );

# categories for elements of incidence geometries.

DeclareCategory( "IsElementOfIncidenceGeometry", IsElementOfIncidenceStructure );
DeclareCategoryCollections("IsElementOfIncidenceStructure");

DeclareCategory( "IsElementsOfIncidenceGeometry", IsElementsOfIncidenceStructure );
DeclareCategory( "IsAllElementsOfIncidenceGeometry", IsAllElementsOfIncidenceStructure );
DeclareCategory( "IsShadowElementsOfIncidenceGeometry", IsElementsOfIncidenceStructure );

DeclareCategory( "IsFlagOfIncidenceGeometry", IsFlagOfIncidenceStructure );

#############################################################################
#
# further categories for geometries:
#
#############################################################################

DeclareCategory( "IsLieGeometry", IsIncidenceGeometry );

DeclareCategory( "IsAffineSpace", IsIncidenceGeometry );
DeclareCategory( "IsClassicalPolarSpace", IsLieGeometry );
DeclareCategory( "IsProjectiveSpace", IsLieGeometry );
DeclareCategory( "IsGeneralisedPolygon", IsIncidenceGeometry );

# to resolve the dependency of projectivespace.gi and polarspace.gi from gpolygon.gd, the following particular
# declarations are put here. IsProjectivePlane seems to be the name of an operation in package "rds". Therefore
# we make it IsProjectivePlaneCategory here.
DeclareCategory( "IsProjectivePlaneCategory", IsGeneralisedPolygon );
DeclareCategory( "IsGeneralisedQuadrangle", IsGeneralisedPolygon );
DeclareCategory( "IsGeneralisedHexagon", IsGeneralisedPolygon );
DeclareCategory( "IsGeneralisedOctagon", IsGeneralisedPolygon );

DeclareCategory( "IsDesarguesianPlane", IsProjectivePlaneCategory and IsProjectiveSpace ); 
DeclareCategory( "IsClassicalGQ", IsGeneralisedQuadrangle and IsClassicalPolarSpace ); 
DeclareCategory( "IsClassicalGeneralisedHexagon", IsGeneralisedHexagon and IsLieGeometry ); 

#
#############################################################################
#
# representations
#
#############################################################################

# representations for geometries.

DeclareRepresentation( "IsIncidenceStructureRep", IsIncidenceStructure, ["elements", "increl", "typefun", "typeset" ] ); 
DeclareRepresentation( "IsAffineSpaceRep", IsAffineSpace, [ "basefield", "vectorspace" ] );
DeclareRepresentation( "IsClassicalPolarSpaceRep", IsClassicalPolarSpace, [ "basefield", "vectorspace" ] );
DeclareRepresentation( "IsProjectiveSpaceRep", IsProjectiveSpace, [ "basefield", "vectorspace" ] );

# representations for elements

DeclareRepresentation( "IsElementOfIncidenceStructureRep", IsElementOfIncidenceStructure, [ "geo", "type", "obj" ] );
DeclareRepresentation( "IsElementsOfIncidenceStructureRep", IsElementsOfIncidenceStructure, [ "geometry", "type"  ] );
DeclareRepresentation( "IsAllElementsOfIncidenceStructureRep", IsAllElementsOfIncidenceStructure, [ "geometry", "type"  ] );

# representations for flags/chambers

DeclareRepresentation( "IsFlagOfIncidenceStructureRep", IsFlagOfIncidenceStructure, [ "geo", "types", "els" ] );

#############################################################################
#
# definitions to create complete types for objects.
#
#############################################################################

# Families

BindGlobal( "GeometriesFamily", NewFamily( "GeometriesFamily" ) );
BindGlobal( "ElementsOfIncidenceStructureFamily", NewFamily( "ElementsOfIncidenceStructureFamily", IsElementOfIncidenceStructure ));
BindGlobal( "ElementsCollFamily", CollectionsFamily(ElementsOfIncidenceStructureFamily) );

BindGlobal( "FlagsOfIncidenceStructureFamily", NewFamily( "FlagsOfIncidenceStructureFamily", IsObject ));   

# types

BindGlobal( "IsElementOfIncidenceStructureType", NewType( ElementsOfIncidenceStructureFamily,
                                    IsElementOfIncidenceStructure and IsElementOfIncidenceStructureRep) );

BindGlobal( "IsFlagOfIncidenceStructureType", NewType( FlagsOfIncidenceStructureFamily,
                                    IsFlagOfIncidenceStructure and IsFlagOfIncidenceStructureRep) );

#############################################################################
#
# operations to construct an incidence structure
#
#############################################################################

DeclareOperation( "IncidenceStructure", [ IsList, IsFunction, IsFunction, IsList ]);
DeclareOperation( "ResidueOfFlag", [IsFlagOfIncidenceStructure]);

#############################################################################
#
# Attributes and operations for incidence structures:
#
#############################################################################

DeclareAttribute( "RankAttr", IsIncidenceStructure );
DeclareAttribute( "RankAttr", IsFlagOfIncidenceStructure );
DeclareAttribute( "TypesOfElementsOfIncidenceStructure", IsIncidenceStructure );
DeclareAttribute( "TypesOfElementsOfIncidenceStructurePlural", IsIncidenceStructure );

DeclareOperation( "ElementsOfIncidenceStructure", [IsIncidenceStructure] );
DeclareOperation( "ElementsOfIncidenceStructure", [IsIncidenceStructure, IsPosInt] );
DeclareOperation( "ElementsOfIncidenceStructure", [IsIncidenceStructure, IsString] );

DeclareOperation( "NrElementsOfIncidenceStructure", [IsIncidenceStructure, IsPosInt] );
DeclareOperation( "NrElementsOfIncidenceStructure", [IsIncidenceStructure, IsString] );

DeclareOperation( "IncidenceGraph", [ IsIncidenceStructure ] );
IncidenceGraphAttr := NewAttribute( "IncidenceGraphAttr", IsIncidenceStructure, "mutable" );

DeclareOperation( "Points", [IsIncidenceStructure] );
DeclareOperation( "Lines", [IsIncidenceStructure] );
DeclareOperation( "Planes", [IsIncidenceStructure] );
DeclareOperation( "Solids", [IsIncidenceStructure] );

DeclareAttribute( "CollineationGroup", IsIncidenceStructure );
DeclareAttribute( "CorrelationCollineationGroup", IsIncidenceStructure );
DeclareAttribute( "CollineationAction", IsIncidenceStructure );
DeclareAttribute( "CorrelationAction", IsIncidenceStructure );
DeclareAttribute( "RepresentativesOfElements", IsIncidenceStructure );

DeclareAttribute( "AmbientGeometry", IsIncidenceStructure );
DeclareAttribute( "AmbientGeometry", IsFlagOfIncidenceStructure );

#############################################################################
#
# Attributes and operations for elements of incidence structures:
#
#############################################################################

InstallTrueMethod( IsFinite, IsElementsOfIncidenceStructure );
InstallTrueMethod( IsFinite, IsAllElementsOfIncidenceStructure );

DeclareOperation( "FlagOfIncidenceStructure", [ IsIncidenceStructure, IsElementOfIncidenceStructureCollection ]);
DeclareOperation( "FlagOfIncidenceStructure", [ IsIncidenceStructure, IsList and IsEmpty ]);
DeclareOperation( "ChamberOfIncidenceStructure", [ IsElementOfIncidenceStructureCollection ]);

DeclareOperation( "ElementsOfFlag", [ IsFlagOfIncidenceStructure ]);
DeclareAttribute( "Size", IsFlagOfIncidenceStructure );

DeclareOperation( "IsIncident", [IsElementOfIncidenceStructure, IsElementOfIncidenceStructure] );
DeclareOperation( "IsIncident", [IsElementOfIncidenceStructure, IsFlagOfIncidenceStructure] );
DeclareOperation( "IsIncident", [IsFlagOfIncidenceStructure, IsElementOfIncidenceStructure] );
#DeclareOperation( "ShadowOfFlag", [IsFlagOfIncidenceStructure, IsPosInt] );
DeclareOperation( "ShadowOfElement", [IsElementOfIncidenceStructure, IsPosInt] );
DeclareOperation( "IsCollinear", [IsIncidenceStructure, IsElementOfIncidenceStructure, IsElementOfIncidenceStructure]);
DeclareOperation( "Span", [IsElementOfIncidenceStructure, IsElementOfIncidenceStructure]);
DeclareOperation( "Meet", [IsElementOfIncidenceStructure, IsElementOfIncidenceStructure]);
#DeclareOperation( "Span", [IsHomogeneousList]);
#DeclareOperation( "Meet", [IsHomogeneousList]);
DeclareAttribute( "AmbientGeometry", IsElementOfIncidenceStructure and IsElementOfIncidenceStructureRep  );
DeclareAttribute( "AmbientGeometry", IsElementsOfIncidenceStructure and IsElementsOfIncidenceStructureRep );
DeclareAttribute( "AmbientGeometry", IsAllElementsOfIncidenceStructure );

#DeclareOperation( "RandomFlag", [IsIncidenceStructure]);
#DeclareOperation( "RandomChamber", [IsIncidenceStructure]);

DeclareOperation( "Type", [ IsElementOfIncidenceStructure and IsElementOfIncidenceStructureRep ] );
DeclareOperation( "Type", [ IsElementsOfIncidenceStructure and IsElementsOfIncidenceStructureRep ] );
DeclareOperation( "Type", [ IsFlagOfIncidenceStructure and IsFlagOfIncidenceStructureRep ] );

DeclareOperation( "Wrap", [IsIncidenceStructure, IsPosInt, IsObject] );
DeclareOperation( "Unwrap", [IsElementOfIncidenceStructure] );

# three general operations. Methods to be installed for several filters.
DeclareOperation( "ObjectToElement", [IsIncidenceStructure, IsPosInt, IsObject] );
DeclareOperation( "ObjectToElement", [IsIncidenceStructure, IsObject] );
if IsBoundGlobal( "UnderlyingObject" ) and not IsAttribute( ValueGlobal( "UnderlyingObject" ) ) then
    DeclareOperation( "UnderlyingObject", [IsElementOfIncidenceStructure] );
else
    DeclareAttribute( "UnderlyingObject", IsElementOfIncidenceStructure );
fi;

DeclareGlobalFunction( "HashFuncForElements" );
DeclareGlobalFunction( "HashFuncForSetElements" );

#############################################################################
#
# Shadow related operations:
# The declaration of these operation does however not imply that methods are installed
# for particular cases.
#
#############################################################################

#new on 19/4/2011
DeclareCategory( "IsShadowElementsOfIncidenceStructure", IsElementsOfIncidenceStructure );

DeclareOperation( "ShadowOfElement", [IsIncidenceStructure, IsElementOfIncidenceStructure, IsPosInt] );
DeclareOperation( "ShadowOfElement", [IsIncidenceStructure, IsElementOfIncidenceStructure, IsString] );

DeclareOperation( "ShadowOfFlag", [IsIncidenceStructure, IsFlagOfIncidenceStructure, IsPosInt] );
DeclareOperation( "ShadowOfFlag", [IsIncidenceStructure, IsFlagOfIncidenceStructure, IsString] );
DeclareOperation( "ShadowOfFlag", [IsIncidenceStructure, IsList, IsPosInt] );
DeclareOperation( "ShadowOfFlag", [IsIncidenceStructure, IsList, IsString] );

#now a very general operation. 
DeclareOperation( "ElementsIncidentWithElementOfIncidenceStructure", [ IsElementOfIncidenceStructure, IsPosInt ] );
DeclareOperation( "Points", [ IsElementOfIncidenceStructure ] );
DeclareOperation( "Lines", [ IsElementOfIncidenceStructure ] );
DeclareOperation( "Planes", [ IsElementOfIncidenceStructure ] );
DeclareOperation( "Solids", [ IsElementOfIncidenceStructure ] );
DeclareOperation( "Hyperplanes", [ IsElementOfIncidenceStructure ] );

DeclareOperation( "Points", [ IsIncidenceStructure, IsElementOfIncidenceStructure ] );
DeclareOperation( "Lines", [ IsIncidenceStructure, IsElementOfIncidenceStructure ] );
DeclareOperation( "Planes", [ IsIncidenceStructure, IsElementOfIncidenceStructure ] );
DeclareOperation( "Solids", [ IsIncidenceStructure, IsElementOfIncidenceStructure ] );
DeclareOperation( "Hyperplanes", [ IsIncidenceStructure, IsElementOfIncidenceStructure ] );

#and two very general properties.

DeclareProperty( "IsConfiguration", IsIncidenceStructure );
DeclareProperty( "IsConstellation", IsIncidenceStructure );

#############################################################################
#
# The representation of a collineation group of a geometry could be, 
# in a natural way, incompatible with the elements of the geometry. In such 
# cases, this attribute can be installed returning an appropriate action
# function.
#
#############################################################################

DeclareAttribute( "CollineationAction", IsGroup);
