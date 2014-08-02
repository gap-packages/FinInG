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
##  Copyright 2014	Colorado State University, Fort Collins
##					Università degli Studi di Padova
##					Universeit Gent
##					University of St. Andrews
##					University of Western Australia, Perth
##                  Vrije Universiteit Brussel
##
##
##  Declaration stuff for generalised polygons.
##
#############################################################################

DeclareCategory( "IsElationGQ", IsGeneralisedQuadrangle );
DeclareCategory( "IsElationGQByKantorFamily", IsElationGQ );

# Important notice: in the next line, "points" and "lines" are the underlying objects (or a representative) 
# of the real points and lines of the GP to be constructed.
# "incidence" is a function that determines whether the wrapped elements with given underlying objects, are incident.
DeclareRepresentation( "IsGeneralisedPolygonRep", IsGeneralisedPolygon, [ "incidence", "pointsobj", "linesobj", "listelements", "shadpoint", "shadline", "distance" ]);

DeclareCategory( "IsElementOfGeneralisedPolygon", IsElementOfIncidenceGeometry );
DeclareCategory( "IsElementsOfGeneralisedPolygon", IsElementsOfIncidenceGeometry );
DeclareCategory( "IsAllElementsOfGeneralisedPolygon", IsAllElementsOfIncidenceGeometry );

DeclareRepresentation( "IsElementsOfGeneralisedPolygonRep", IsElementsOfIncidenceStructureRep, [ "geometry", "type", "obj" ] );

#DeclareCategory( "IsAllElementsOfProjectivePlane", IsAllElementsOfGeneralisedPolygon );
#DeclareRepresentation( "IsAllElementsOfProjectivePlaneRep", IsAllElementsOfGeneralisedPolygonRep, [ "geometry", "type" ] );

#DeclareCategory( "IsAllElementsOfGeneralisedQuadrangle", IsAllElementsOfGeneralisedPolygon );
#DeclareRepresentation( "IsAllElementsOfGeneralisedQuadrangleRep", IsAllElementsOfGeneralisedPolygonRep, [ "geometry", "type" ] );


#DeclareCategory( "IsAllElementsOfGeneralisedHexagon", IsAllElementsOfGeneralisedPolygon );
#DeclareRepresentation( "IsAllElementsOfGeneralisedHexagonRep", IsAllElementsOfGeneralisedPolygonRep, [ "geometry", "type" ] );

DeclareCategory( "IsElementOfKantorFamily", IsElementOfGeneralisedPolygon );
DeclareCategory( "IsAllElementsOfKantorFamily", IsAllElementsOfGeneralisedPolygon );
DeclareRepresentation( "IsElementOfKantorFamilyRep", IsElementOfKantorFamily, [ "geo", "type", "class", "obj" ]);

DeclareCategory( "IsShadowElementsOfGeneralisedPolygon", IsElementsOfIncidenceStructure );
DeclareRepresentation( "IsShadowElementsOfGeneralisedPolygonRep", IsElementsOfIncidenceStructure, [ "geometry", "type", "element", "func" ]);

### new stuff

DeclareCategory( "IsqClanObj", IsComponentObjectRep and IsAttributeStoringRep );
DeclareRepresentation( "IsqClanRep", IsqClanObj, [ "matrices", "basefield" ] ); 

BindGlobal( "qClanFamily", NewFamily( "qClanFamily" ) );


DeclareOperation( "GeneralisedPolygonByBlocks", [ IsHomogeneousList ] );
DeclareOperation( "GeneralisedPolygonByIncidenceMatrix", [ IsMatrix ] );
DeclareOperation( "GeneralisedPolygonByElements", [ IsSet, IsSet, IsFunction ] );
DeclareOperation( "GeneralisedPolygonByElements", [ IsSet, IsSet, IsFunction, IsGroup, IsFunction ] );

DeclareOperation( "DistanceBetweenElements", [IsElementOfGeneralisedPolygon, IsElementOfGeneralisedPolygon ] );

#############################################################################
# Attributes:
#############################################################################

DeclareAttribute( "Order", IsGeneralisedPolygon);
DeclareAttribute( "AmbientSpace", IsGeneralisedPolygon);
DeclareAttribute( "CollineationAction", IsGroup);
DeclareAttribute( "ElationGroup", IsElationGQ);
DeclareAttribute( "BasePointOfEGQ", IsElationGQ);
DeclareAttribute( "IncidenceMatrixOfGeneralisedPolygon", IsGeneralisedPolygon);
DeclareAttribute( "IsLinearqClan", IsqClanObj );

DeclareAttribute( "AmbientPolarSpace", IsGeneralisedHexagon);


#############################################################################
# Operations and functions 
#############################################################################

DeclareOperation("G2fining", [IsPosInt, IsField and IsFinite] );
DeclareOperation("3D4fining", [IsField and IsFinite] );

#DeclareOperation("Span",[IsElementOfGeneralisedPolygon, IsElementOfGeneralisedPolygon]);

DeclareGlobalFunction( "OnKantorFamily" );

DeclareGlobalFunction( "SplitCayleyPointToPlane" );
DeclareGlobalFunction( "SplitCayleyPointToPlane5" );
DeclareGlobalFunction( "ZeroPointToOnePointsSpaceByTriality" );
DeclareGlobalFunction( "TwistedTrialityHexagonPointToPlaneByTwoTimesTriality" );

DeclareOperation( "SplitCayleyHexagon", [IsField and IsFinite] );
DeclareOperation( "SplitCayleyHexagon", [IsPosInt] );
DeclareOperation( "SplitCayleyHexagon", [IsClassicalPolarSpace] );
DeclareOperation( "TwistedTrialityHexagon", [IsField and IsFinite] );
DeclareOperation( "TwistedTrialityHexagon", [IsPosInt] );

## q-clans
DeclareOperation( "IsAnisotropic", [IsFFECollColl,  IsField and IsFinite]);
DeclareOperation( "IsqClan", [ IsFFECollCollColl, IsField and IsFinite ]);
DeclareOperation( "qClan", [ IsFFECollCollColl, IsField ] );
DeclareOperation( "LinearqClan", [ IsPosInt ] );
DeclareOperation( "FisherThasWalkerKantorBettenqClan", [ IsPosInt ] );
DeclareOperation( "KantorMonomialqClan", [ IsPosInt ] );
DeclareOperation( "KantorKnuthqClan", [ IsPosInt ] );
DeclareOperation( "FisherqClan", [ IsPosInt ] );

## Elation Generalised Quadrangles

DeclareOperation( "EGQByKantorFamily", [IsGroup, IsList, IsList] );
DeclareOperation( "Wrap", [IsElationGQByKantorFamily, IsPosInt, IsPosInt, IsObject] );
DeclareOperation( "IsKantorFamily", [IsGroup, IsList, IsList]);

#---------------------
## obselete operations
# DeclareOperation( "EGQByqClan", [ IsFFECollCollColl, IsField and IsFinite ]);
# DeclareOperation( "KantorFamilyByqClan", [ IsFFECollCollColl, IsField and IsFinite ]); 
# DeclareOperation( "BLTSetByqClan", [ IsFFECollCollColl, IsField and IsFinite ]);
#---------------------

DeclareOperation( "EGQByBLTSet", [IsList, IsSubspaceOfProjectiveSpace, IsSubspaceOfProjectiveSpace] );
DeclareOperation( "EGQByBLTSet", [IsList] );


DeclareOperation( "FlockGQByqClan", [ IsqClanObj ] );
DeclareOperation( "BLTSetByqClan", [ IsqClanObj and IsqClanRep ] );
DeclareOperation( "KantorFamilyByqClan", [ IsqClanObj and IsqClanRep ] );
DeclareOperation( "EGQByqClan", [ IsqClanObj and IsqClanRep ] );


#############################################################################
# Operations and functions for projective planes
#############################################################################

#DeclareOperation( "ProjectivePlaneByBlocks", [ IsHomogeneousList ] );
#DeclareOperation( "ProjectivePlaneByIncidenceMatrix", [ IsMatrix ] );

#############################################################################
# Mutable attributes
#############################################################################

if not IsBound( BlockDesign ) then 
   BlockDesign := function(arg) return 1; end;
fi;


BlockDesignOfGeneralisedPolygonAttr := NewAttribute( "BlockDesignOfGeneralisedPolygonAttr", 
                    IsGeneralisedPolygon, "mutable" );
IncidenceGraphOfGeneralisedPolygonAttr := NewAttribute( "IncidenceGraphOfGeneralisedPolygonAttr", 
					IsGeneralisedPolygon, "mutable" );

DeclareOperation( "BlockDesignOfGeneralisedPolygon", [ IsGeneralisedPolygon ] );
DeclareOperation( "IncidenceGraphOfGeneralisedPolygon", [ IsGeneralisedPolygon ]);
