#############################################################################
##
##  gpolygons.gd              FinInG package
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
##  Declaration stuff for generalised polygons.
##
#############################################################################


# see .gi file for comments on the representation.
DeclareRepresentation( "IsGeneralisedPolygonRep", IsGeneralisedPolygon, [ "incidence", "pointsobj", "linesobj", "listelements", "shadpoint", "shadline", "distance" ]);

DeclareCategory( "IsElementOfGeneralisedPolygon", IsElementOfIncidenceGeometry );

DeclareRepresentation( "IsElementOfGeneralisedPolygonRep", IsElementOfIncidenceStructureRep, [ "geometry", "type", "obj" ] );

DeclareRepresentation( "IsElementsOfGeneralisedPolygonRep", IsElementsOfIncidenceStructureRep, [ "geometry", "type" ] );
DeclareCategory( "IsElementsOfGeneralisedPolygon", IsElementsOfIncidenceGeometry );
DeclareCategory( "IsAllElementsOfGeneralisedPolygon", IsAllElementsOfIncidenceGeometry );

DeclareCategory( "IsShadowElementsOfGeneralisedPolygon", IsElementsOfIncidenceStructure );
DeclareRepresentation( "IsShadowElementsOfGeneralisedPolygonRep", IsElementsOfIncidenceStructure, [ "geometry", "type", "element", "func" ]);

DeclareCategory( "IsElationGQ", IsGeneralisedQuadrangle );
DeclareCategory( "IsElationGQByKantorFamily", IsElationGQ );
DeclareCategory( "IsElationGQByBLTSet", IsElationGQ );

# A generalised polygon is weak if it has no order. So it is a bipartite graph, with girth=2diam, but that's all.
DeclareCategory( "IsWeakGeneralisedPolygon", IsGeneralisedPolygon );

#############################################################################
# Constructor operations:
#############################################################################

DeclareOperation( "GeneralisedPolygonByBlocks", [ IsHomogeneousList ] );
DeclareOperation( "GeneralisedPolygonByIncidenceMatrix", [ IsMatrix ] );
DeclareOperation( "GeneralisedPolygonByElements", [ IsSet, IsSet, IsFunction ] );
DeclareOperation( "GeneralisedPolygonByElements", [ IsSet, IsSet, IsFunction, IsGroup, IsFunction ] );

#############################################################################
# Operations, Functions, Attributes 
#############################################################################

# distance between elements: second and third declaration are necessary for elements
# of desarguesian projective planes and classical GQs.
DeclareOperation( "DistanceBetweenElements", [IsElementOfGeneralisedPolygon, IsElementOfGeneralisedPolygon ] );
DeclareOperation( "DistanceBetweenElements", [IsSubspaceOfProjectiveSpace, IsSubspaceOfProjectiveSpace ] );

DeclareProperty( "HasGraphWithUnderlyingObjectsAsVertices", IsGeneralisedPolygon);

#DeclareOperation("Span",[IsElementOfGeneralisedPolygon, IsElementOfGeneralisedPolygon]);
#DeclareOperation("Meet",[IsElementOfGeneralisedPolygon, IsElementOfGeneralisedPolygon]);

#############################################################################
# Attributes
#############################################################################

# normal ones

DeclareAttribute( "Order", IsGeneralisedPolygon);
DeclareAttribute( "IncidenceMatrixOfGeneralisedPolygon", IsGeneralisedPolygon);

# do not make the next definition if "design" has been loaded already!
# Note about BlockDesign: this is a global (so read-only) function in the
# "design" package. We have created in FinInG a function BlockDesign (not 
# through DeclareGlobalFunction, so it can be overwritten at user level and
# through loading other packages), so that the method for BlockDesignOfGeneralisedPolygon
# can be loaded without the "design" package loaded. We make sure that the "BlockDesign"
# as variable is only initialised in FinInG if desing is *not* loaded. It is sufficient
# to check whether BlockDesign is bounded when FinInG is loaded, it causes a warning
# message when design is loaded after fining.

if not IsBound( BlockDesign ) then 
   BlockDesign := function(arg) return 1; end;
fi;

# mutable attributes
BlockDesignOfGeneralisedPolygonAttr := NewAttribute( "BlockDesignOfGeneralisedPolygonAttr", 
                    IsGeneralisedPolygon, "mutable" );
#IncidenceGraphOfGeneralisedPolygonAttr := NewAttribute( "IncidenceGraphOfGeneralisedPolygonAttr",
#					IsGeneralisedPolygon, "mutable" );

DeclareOperation( "BlockDesignOfGeneralisedPolygon", [ IsGeneralisedPolygon ] );

#############################################################################
# Classical Generalised Hexagons
#############################################################################

# helper functions for triality

DeclareGlobalFunction( "SplitCayleyPointToPlane" );
DeclareGlobalFunction( "SplitCayleyPointToPlane5" );
DeclareGlobalFunction( "ZeroPointToOnePointsSpaceByTriality" );
DeclareGlobalFunction( "TwistedTrialityHexagonPointToPlaneByTwoTimesTriality" );

# constructor operations for the hexagons

DeclareOperation( "SplitCayleyHexagon", [IsField and IsFinite] );
DeclareOperation( "SplitCayleyHexagon", [IsPosInt] );
DeclareOperation( "SplitCayleyHexagon", [IsClassicalPolarSpace] );
DeclareOperation( "TwistedTrialityHexagon", [IsField and IsFinite] );
DeclareOperation( "TwistedTrialityHexagon", [IsPosInt] );
DeclareOperation( "TwistedTrialityHexagon", [IsClassicalPolarSpace] );

# attributes

DeclareAttribute( "AmbientPolarSpace", IsGeneralisedHexagon);

# groups

DeclareOperation("G2fining", [IsPosInt, IsField and IsFinite] );
DeclareOperation("3D4fining", [IsField and IsFinite] );


#############################################################################
# Elation Generalised Quadrangles
#############################################################################

# two general attributes

DeclareAttribute( "ElationGroup", IsElationGQ);
DeclareAttribute( "BasePointOfEGQ", IsElationGQ);

# Kantor families

DeclareOperation( "IsKantorFamily", [IsGroup, IsList, IsList]);

DeclareCategory( "IsElementOfKantorFamily", IsElementOfGeneralisedPolygon );
DeclareRepresentation( "IsElementOfKantorFamilyRep", IsElementOfKantorFamily and IsElementOfIncidenceStructureRep, [ "geo", "type", "class", "obj" ]);

DeclareGlobalFunction( "OnKantorFamily" );

DeclareOperation( "EGQByKantorFamily", [IsGroup, IsList, IsList] );
DeclareOperation( "Wrap", [IsElationGQByKantorFamily, IsPosInt, IsPosInt, IsObject] );

# q-clans

DeclareCategory( "IsqClanObj", IsComponentObjectRep and IsAttributeStoringRep );
DeclareRepresentation( "IsqClanRep", IsqClanObj, [ "matrices", "basefield" ] ); 

BindGlobal( "qClanFamily", NewFamily( "qClanFamily" ) );

DeclareAttribute( "IsLinearqClan", IsqClanObj );

#particular q-clans

DeclareOperation( "IsAnisotropic", [IsFFECollColl,  IsField and IsFinite]);
DeclareOperation( "IsqClan", [ IsFFECollCollColl, IsField and IsFinite ]);
DeclareOperation( "qClan", [ IsFFECollCollColl, IsField ] );
DeclareOperation( "LinearqClan", [ IsPosInt ] );
DeclareOperation( "FisherThasWalkerKantorBettenqClan", [ IsPosInt ] );
DeclareOperation( "KantorMonomialqClan", [ IsPosInt ] );
DeclareOperation( "KantorKnuthqClan", [ IsPosInt ] );
DeclareOperation( "FisherqClan", [ IsPosInt ] );

# switching between q-clans, Kantor families and BLT sets

DeclareOperation( "BLTSetByqClan", [ IsqClanObj and IsqClanRep ] );
DeclareOperation( "KantorFamilyByqClan", [ IsqClanObj and IsqClanRep ] );
DeclareOperation( "EGQByqClan", [ IsqClanObj and IsqClanRep ] );

# constructor operations

DeclareOperation( "EGQByBLTSet", [IsList, IsSubspaceOfProjectiveSpace, IsSubspaceOfProjectiveSpace] );
DeclareOperation( "EGQByBLTSet", [IsList] );

DeclareAttribute( "DefiningPlanesOfEGQByBLTSet", IsElationGQByBLTSet);
DeclareAttribute( "CollineationSubgroup", IsElationGQByBLTSet);

DeclareOperation( "FlockGQByqClan", [ IsqClanObj ] );
