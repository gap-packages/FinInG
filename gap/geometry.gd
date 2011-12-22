#############################################################################
##
##  geometry.gd              FinInG package
##                                                              John Bamberg
##                                                              Anton Betten
##                                                              Jan De Beule
##                                                             Philippe Cara
##                                                            Michel Lavrauw
##                                                                 Maska Law
##                                                           Max Neunhoeffer
##                                                            Michael Pauley
##                                                             Sven Reichard
##
##  Copyright 2011	Colorado State University, Fort Collins
##					Università degli Studi di Padova
##					Universeit Gent
##					University of St. Andrews
##					University of Western Australia, Perth
##                  Vrije Universiteit Brussel
##                 
##
##  Declaration stuff for geometries. changed by the new gang of 
##  four, September 24, 2008, st. andrews.
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

DeclareGlobalVariable( "FINING" );


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
DeclareCategoryCollections("IsElementOfIncidenceStructure");

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
DeclareCategoryCollections("IsElementOfIncidenceGeometry");

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

# 26/9/08 the next six declarations come from gpolygon.gd. I've pasted them here
# before discussing them, just to resolve the dependency of polarspace.gi from
# gpolygon.gd. 
DeclareCategory( "IsProjectivePlane", IsGeneralisedPolygon );
DeclareCategory( "IsGeneralisedQuadrangle", IsGeneralisedPolygon );
DeclareCategory( "IsGeneralisedHexagon", IsGeneralisedPolygon );
DeclareCategory( "IsGeneralisedOctogon", IsGeneralisedPolygon );

DeclareCategory( "IsElationGQ", IsGeneralisedQuadrangle );
DeclareCategory( "IsElationGQByKantorFamily", IsElationGQ );
#DeclareCategory( "IsClassicalGQ", IsElationGQ and IsClassicalPolarSpace ); #I don't see the 
#point of this, a classical polar space that is a GQ, is a classical GQ. By over specifying 
#it as a ElationGQ, it might cause trouble for methods that are specifycally meant for ElationGQs, which 
# will have another representation than polar spaces. So I change it bravely :-) jdb. 28/9/2011.

DeclareCategory( "IsClassicalGQ", IsGeneralisedQuadrangle and IsClassicalPolarSpace ); 


# This is what we thought it could be, before realizing that these special GQs
# where defined. To be discussed furthre. jdb.
#DeclareCategory( "IsClassicalGeneralisedQuadrangle", IsClassicalPolarSpace and IsGeneralisedPolygon);

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
BindGlobal( "ElementsOfIncidenceStructureFamily", NewFamily( "ElementsOfIncidenceStructureFamily", IsObject ));   
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

#############################################################################
#
# Attributes and operations for incidence structures:
#
#############################################################################

DeclareAttribute( "RankAttr", IsIncidenceStructure );
DeclareAttribute( "TypesOfElementsOfIncidenceStructure", IsIncidenceStructure );
DeclareAttribute( "TypesOfElementsOfIncidenceStructurePlural", IsIncidenceStructure );

DeclareOperation( "ElementsOfIncidenceStructure", [IsIncidenceStructure] );
DeclareOperation( "ElementsOfIncidenceStructure", [IsIncidenceStructure, IsPosInt] );
DeclareOperation( "ElementsOfIncidenceStructure", [IsIncidenceStructure, IsString] );
DeclareOperation( "Points", [IsIncidenceStructure] );
DeclareOperation( "Lines", [IsIncidenceStructure] );
DeclareOperation( "Planes", [IsIncidenceStructure] );
DeclareOperation( "Solids", [IsIncidenceStructure] );

DeclareAttribute( "CollineationGroup", IsIncidenceStructure );
DeclareAttribute( "CorrelationGroup", IsIncidenceStructure );
DeclareAttribute( "CollineationAction", IsIncidenceStructure );
DeclareAttribute( "CorrelationAction", IsIncidenceStructure );
DeclareAttribute( "RepresentativesOfElements", IsIncidenceStructure );

DeclareAttribute( "AmbientGeometry", IsIncidenceStructure );

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

DeclareOperation( "IsIncident", [IsElementOfIncidenceStructure, IsElementOfIncidenceStructure] );
DeclareOperation( "IsCollinear", [IsIncidenceStructure, IsElementOfIncidenceStructure, IsElementOfIncidenceStructure]);
DeclareOperation( "Span", [IsElementOfIncidenceStructure, IsElementOfIncidenceStructure]);
DeclareOperation( "Meet", [IsElementOfIncidenceStructure, IsElementOfIncidenceStructure]);
#DeclareOperation( "Span", [IsHomogeneousList]);
#DeclareOperation( "Meet", [IsHomogeneousList]);
DeclareAttribute( "AmbientGeometry", IsElementOfIncidenceStructure and IsElementOfIncidenceStructureRep  );
DeclareAttribute( "AmbientGeometry", IsElementsOfIncidenceStructure and IsElementsOfIncidenceStructureRep );
DeclareAttribute( "AmbientGeometry", IsAllElementsOfIncidenceStructure );

DeclareOperation( "RandomFlag", [IsIncidenceStructure]);
DeclareOperation( "RandomChamber", [IsIncidenceStructure]);

DeclareOperation( "Type", [ IsElementOfIncidenceStructure and IsElementOfIncidenceStructureRep ] );
DeclareOperation( "Type", [ IsElementsOfIncidenceStructure and IsElementsOfIncidenceStructureRep ] );


DeclareFilter( "IsUnwrapper" );
DeclareGlobalVariable( "_" );
InstallValue( _, Objectify( NewType( NewFamily( "UnwrapperFamily" ), IsUnwrapper ), rec() ));

DeclareOperation( "Wrap", [IsIncidenceGeometry, IsPosInt, IsObject] );
DeclareOperation( "Unwrap", [IsElementOfIncidenceStructure] );
DeclareOperation( "\^", [IsElementOfIncidenceStructure, IsUnwrapper ] ); 


DeclareGlobalFunction( "HashFuncForElements" );
# commenting this out had not effect, presumably...
#DeclareOperation( "ChooseHashFunction", [IsElementOfIncidenceStructure,IsInt] );

#############################################################################
#
# Shadow related operations:
# The declaration of these operation does however not imply that methods are installed
# for particular cases.
#
#############################################################################

#new on 19/4/2011
DeclareCategory( "IsShadowElementsOfIncidenceStructure", IsElementsOfIncidenceStructure );

DeclareOperation( "ShadowOfElement",
  [IsIncidenceStructure, IsElementOfIncidenceStructure, IsPosInt] );
DeclareOperation( "ShadowOfElement",
  [IsIncidenceStructure, IsElementOfIncidenceStructure, IsString] );

DeclareOperation( "ShadowOfFlag",
  [IsIncidenceStructure, IsFlagOfIncidenceStructure, IsPosInt] );
DeclareOperation( "ShadowOfFlag",
  [IsIncidenceStructure, IsFlagOfIncidenceStructure, IsString] );  
DeclareOperation( "ShadowOfFlag",
  [IsIncidenceStructure, IsList, IsPosInt] );
DeclareOperation( "ShadowOfFlag",
  [IsIncidenceStructure, IsList, IsString] );  


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


