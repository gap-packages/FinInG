#############################################################################
##
##  diagram.gd              FinInG package
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
##  Declaration stuff for diagram geometries
##
############################################################################

#############################################################################
#
# In geometry.gd, we defined an IsIncidenceStructure as...
# DeclareCategory( "IsIncidenceStructure",
#                IsComponentObjectRep and IsAttributeStoringRep );
# We also have a subfilter IsIncidenceGeometry.
#
# Thereafter, we defined an elemen to have representation...
# DeclareRepresentation( "IsElementOfIncidenceStructureRep", 
#       IsElementOfIncidenceStructure, [ "geo", "type", "obj" ] );
# 
# So we have already encapsulated the definition of an incidence geometry
# in these data types.
#
# It remains to specialise our categories so that we can construct
# coset geometries, and geometries from labelled graphs.
#
#
#############################################################################

#############################################################################
##
##  Categories and families for Diagrams
##
#############################################################################

DeclareCategory( "IsDiagram", IsComponentObjectRep and IsAttributeStoringRep  );
DeclareRepresentation( "IsDiagramRep", IsDiagram, [ "vertices", "edges", "drawing" ] );
DeclareCategory( "IsVertexOfDiagram", IsComponentObjectRep and IsAttributeStoringRep  );
DeclareRepresentation( "IsVertexOfDiagramRep", IsVertexOfDiagram, [ "type"] );
DeclareCategory( "IsEdgeOfDiagram", IsComponentObjectRep and IsAttributeStoringRep );
DeclareRepresentation( "IsEdgeOfDiagramRep", IsEdgeOfDiagram, [ "edge"] );

## New object... (28/01/09)
# change the filters some time (perhaps subfilter of IsCosetGeometry?)
DeclareCategory( "IsRank2Residue", IsComponentObjectRep and IsAttributeStoringRep );
DeclareRepresentation( "IsRank2ResidueRep", IsRank2Residue, [ "edge", "geo" ] );

DeclareCategory( "IsFlagOfCosetGeometry", IsFlagOfIncidenceStructure );


## Collections

DeclareCategoryCollections("IsDiagram");
BindGlobal( "DiagramFamily", 
  NewFamily( "DiagramFamily", IsDiagram, IsDiagram));

DeclareCategoryCollections("IsVertexOfDiagram");
BindGlobal( "VertexOfDiagramFamily", 
  NewFamily( "VertexOfDiagramFamily", IsVertexOfDiagram, IsVertexOfDiagram));

DeclareCategoryCollections("IsEdgeOfDiagram");
BindGlobal( "EdgeOfDiagramFamily", 
  NewFamily( "EdgeOfDiagramFamily", IsEdgeOfDiagram, IsEdgeOfDiagram));
  
DeclareCategoryCollections("IsRank2Residue");
BindGlobal( "Rank2ResidueFamily", 
  NewFamily( "Rank2ResidueFamily", IsRank2Residue, IsRank2Residue));

#DeclareCategoryCollections("IsElementOfCosetGeometry");

BindGlobal( "FlagsOfCG", NewFamily( "FlagsOfCGFamily", IsObject ));  
BindGlobal( "IsFlagOfCGType", NewType( FlagsOfCG,
                                    IsFlagOfCosetGeometry and IsFlagOfIncidenceStructureRep) );

#############################################################################
##
##  Categories and families for coset geometries
##
#############################################################################

DeclareCategory( "IsCosetGeometry", IsIncidenceGeometry );
DeclareRepresentation( "IsCosetGeometryRep", IsCosetGeometry, [ "group", "parabolics" ] );

## Elements

DeclareCategory( "IsElementOfCosetGeometry", IsElementOfIncidenceGeometry );

DeclareRepresentation( "IsElementOfCosetGeometryRep", IsElementOfIncidenceStructureRep, [ "geo", "type", "obj" ]);
DeclareCategory( "IsElementsOfCosetGeometry", IsElementsOfIncidenceGeometry );
DeclareRepresentation( "IsElementsOfCosetGeometryRep", IsElementsOfIncidenceStructureRep, [ "geometry", "type" ] );

DeclareCategory( "IsAllElementsOfCosetGeometry", IsAllElementsOfIncidenceStructure );
DeclareRepresentation( "IsAllElementsOfCosetGeometryRep", IsAllElementsOfIncidenceStructureRep, [ "geometry", "type" ] );


#############################################################################
##
##  Operations and Attributes
##
##  Note: the attribute "RankAttr" is already defined in geometry.gd,
##  as well as "VarietyTypes", "CollineationGroup", "CorrelationGroup".
##
#############################################################################

DeclareGlobalFunction( "Drawing_Diagram" );
DeclareGlobalFunction( "OnCosetGeometryElement" );

DeclareAttribute( "DiagramOfGeometry", IsIncidenceGeometry ); 
DeclareAttribute( "IsFlagTransitiveGeometry", IsIncidenceGeometry );
DeclareAttribute( "IsResiduallyConnected", IsIncidenceGeometry );
DeclareAttribute( "IsConnected", IsIncidenceGeometry );
DeclareAttribute( "IsFirmGeometry", IsIncidenceGeometry );
DeclareAttribute( "IsThinGeometry", IsIncidenceGeometry );
DeclareAttribute( "IsThickGeometry", IsIncidenceGeometry );
DeclareAttribute( "BorelSubgroup", IsCosetGeometry );
DeclareAttribute( "StandardFlagOfCosetGeometry", IsCosetGeometry );

DeclareOperation( "CosetGeometry", [ IsGroup, IsHomogeneousList ] );
DeclareOperation( "ParabolicSubgroups", [ IsCosetGeometry ] );
DeclareOperation( "AmbientGroup", [ IsCosetGeometry ] );
DeclareOperation( "FlagToStandardFlag", [ IsCosetGeometry, IsFlagOfCosetGeometry ] );
DeclareOperation( "ResidueOfFlag", [ IsFlagOfCosetGeometry ] );
DeclareOperation( "CanonicalResidueOfFlag", [ IsCosetGeometry, IsFlagOfCosetGeometry ] );
#DeclareOperation( "FlagOfIncidenceStructure", [ IsCosetGeometry, IsHomogeneousList ] );
#DeclareOperation( "FlagOfIncidenceStructure", [ IsCosetGeometry, IsList and IsEmpty ] );
#DeclareOperation( "\=", [ IsFlagOfCosetGeometry, IsFlagOfCosetGeometry ]);
#DeclareOperation( "Type", [ IsFlagOfCosetGeometry ] );
DeclareOperation( "RandomElement", [ IsCosetGeometry ] );
DeclareOperation( "RandomFlag", [ IsCosetGeometry ] );
DeclareOperation( "RandomChamber", [ IsCosetGeometry ] );
#DeclareOperation( "Random", [ IsAllElementsOfCosetGeometry ] );
#DeclareOperation( "ElementsOfFlag", [ IsFlagOfCosetGeometry ]); #moved to geometry.gd.
DeclareOperation( "AutGroupIncidenceStructureWithNauty", [ IsCosetGeometry ]);
DeclareOperation( "CorGroupIncidenceStructureWithNauty", [ IsCosetGeometry ]);
DeclareOperation( "IsIsomorphicIncidenceStructureWithNauty", [ IsCosetGeometry, IsCosetGeometry ]);
DeclareOperation( "Rk2GeoDiameter", [ IsCosetGeometry, IsPosInt ] );
DeclareOperation( "Rk2GeoGonality", [ IsCosetGeometry ] );
DeclareOperation( "GeometryOfRank2Residue", [ IsRank2Residue ]);

DeclareAttribute( "Rank2Parameters", IsCosetGeometry );

if not IsBound( Graph ) then 
   Graph := function(arg) return 1; end;
fi;

DeclareOperation( "GeometryFromLabelledGraph", [ IsObject and IS_REC ] );

## Attributes for vertices 

DeclareAttribute( "OrderVertex", IsVertexOfDiagram );
DeclareAttribute( "NrElementsVertex", IsVertexOfDiagram );
DeclareAttribute( "StabiliserVertex", IsVertexOfDiagram );

## Attributes for edges 

DeclareAttribute( "ResidueLabelForEdge", IsEdgeOfDiagram );
DeclareAttribute( "GirthEdge", IsEdgeOfDiagram );
DeclareAttribute( "PointDiamEdge", IsEdgeOfDiagram );
DeclareAttribute( "LineDiamEdge", IsEdgeOfDiagram );
DeclareAttribute( "ParametersEdge", IsEdgeOfDiagram );

DeclareAttribute( "GeometryOfDiagram", IsDiagram );

## Special attribute for incidence graphs

#DeclareOperation( "IncidenceGraph", [ IsCosetGeometry ] );
#IncidenceGraphAttr := NewAttribute( "IncidenceGraph", IsIncidenceGeometry, "mutable" );

DeclareOperation( "Rank2Residues", [ IsIncidenceGeometry ] );
Rank2ResiduesAttr := NewAttribute( "Rank2Residues", IsIncidenceGeometry, "mutable" );

DeclareOperation( "MakeRank2Residue", [ IsRank2Residue ] );

## Drawing diagrams

DeclareGlobalFunction( "DrawDiagram" );
DeclareGlobalFunction( "DrawDiagramWithNeato" );






