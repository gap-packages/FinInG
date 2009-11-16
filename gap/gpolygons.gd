#############################################################################
##
##  gpolyons.gd              Desargues package
##                                                              John Bamberg
## 						                                 		Anton Betten
##                                                              Jan De Beule
##                                                             Philippe Cara
## 							                                  Michel Lavrauw
##                                                                 Maska Law
##                                                           Max Neunhoeffer
##                                                            Michael Pauley
##                                                             Sven Reichard
##
##  Copyright 2008 University of Western Australia, Perth
##                 Lehrstuhl D fuer Mathematik, RWTH Aachen
##                 Ghent University
##                 Colorado State University
##                 Vrije Universiteit Brussel
##
##  Declaration stuff for generalised polygons.
##
#############################################################################

#############################################################################
# Collections, families
#############################################################################

DeclareRepresentation( "IsGeneralisedPolygonRep", IsGeneralisedPolygon, 
  [ "incidence", "points", "lines" ]);

DeclareCategory( "IsElementOfGeneralisedPolygon", IsElementOfIncidenceStructure );
DeclareCategory( "IsAllElementsOfGeneralisedPolygon", IsElementsOfIncidenceStructure );
DeclareRepresentation( "IsAllElementsOfGeneralisedPolygonRep", IsElementsOfIncidenceStructure, 
   [ "geometry", "type" ] );

DeclareCategory( "IsAllElementsOfProjectivePlane", IsAllElementsOfGeneralisedPolygon );
DeclareRepresentation( "IsAllElementsOfProjectivePlaneRep", 
                        IsAllElementsOfGeneralisedPolygonRep, [ "geometry", "type" ] );

DeclareCategory( "IsAllElementsOfGeneralisedQuadrangle", IsAllElementsOfGeneralisedPolygon );
DeclareRepresentation( "IsAllElementsOfGeneralisedQuadrangleRep", 
                        IsAllElementsOfGeneralisedPolygonRep, [ "geometry", "type" ] );


DeclareCategory( "IsAllElementsOfGeneralisedHexagon", IsAllElementsOfGeneralisedPolygon );
DeclareRepresentation( "IsAllElementsOfGeneralisedHexagonRep", 
                        IsAllElementsOfGeneralisedPolygonRep, [ "geometry", "type" ] );

DeclareCategory( "IsElementOfKantorFamily", IsElementOfGeneralisedPolygon );
DeclareCategory( "IsAllElementsOfKantorFamily", IsAllElementsOfGeneralisedPolygon );
DeclareRepresentation( "IsElementOfKantorFamilyRep", IsElementOfKantorFamily, 
  [ "geo", "type", "class", "obj" ]);


#############################################################################
# Attributes:
#############################################################################

DeclareAttribute( "Order", IsGeneralisedPolygon);
DeclareAttribute( "AmbientSpace", IsGeneralisedPolygon);
DeclareAttribute( "CollineationAction", IsGroup);
DeclareAttribute( "ElationGroup", IsElationGQ);
DeclareAttribute( "BasePointOfEGQ", IsElationGQ);
DeclareAttribute( "IncidenceMatrixOfGeneralisedPolygon", IsGeneralisedPolygon);


#############################################################################
# Operations and functions 
#############################################################################

DeclareGlobalFunction( "OnKantorFamily" );

DeclareOperation( "SplitCayleyHexagon", [IsField and IsFinite] );
DeclareOperation( "SplitCayleyHexagon", [IsPosInt] );
DeclareOperation( "TwistedTrialityHexagon", [IsField and IsFinite] );
DeclareOperation( "TwistedTrialityHexagon", [IsPosInt] );

## Just for Elation Generalised Quadrangles

DeclareOperation( "EGQByKantorFamily", [IsGroup, IsList, IsList] );
DeclareOperation( "Wrap", [IsElationGQByKantorFamily, IsPosInt, IsPosInt, IsObject] );
DeclareOperation( "IsKantorFamily", [IsGroup, IsList, IsList]);
DeclareOperation( "IsAnisotropic", [IsFFECollColl,  IsField and IsFinite]);
DeclareOperation( "IsqClan", [ IsFFECollCollColl, IsField and IsFinite ]);
DeclareOperation( "EGQByqClan", [ IsFFECollCollColl, IsField and IsFinite ]);
DeclareOperation( "KantorFamilyByqClan", [ IsFFECollCollColl, IsField and IsFinite ]);
DeclareOperation( "BLTSetByqClan", [ IsFFECollCollColl, IsField and IsFinite ]);
DeclareOperation( "EGQByBLTSet", [IsList, IsSubspaceOfProjectiveSpace, IsSubspaceOfProjectiveSpace] );
DeclareOperation( "EGQByBLTSet", [IsList] );


#############################################################################
# Operations and functions for projective planes
#############################################################################

DeclareOperation( "ProjectivePlaneByBlocks", [ IsHomogeneousList ] );
DeclareOperation( "ProjectivePlaneByIncidenceMatrix", [ IsMatrix ] );

#############################################################################
# Mutable attributes
#############################################################################

if not IsBound( BlockDesign ) then 
   BlockDesign := function(arg) return 1; end;
fi;
if not IsBound( CollapsedAdjacencyMat ) then 
   CollapsedAdjacencyMat := function(arg) return 1; end;
fi;

BlockDesignOfGeneralisedPolygonAttr := NewAttribute( "BlockDesignOfGeneralisedPolygonAttr", 
                    IsGeneralisedPolygon and IsGeneralisedPolygonRep, "mutable" );
IncidenceGraphOfGeneralisedPolygonAttr := NewAttribute( "IncidenceGraphOfGeneralisedPolygonAttr", 
					IsGeneralisedPolygon and IsGeneralisedPolygonRep, "mutable" );

DeclareOperation( "BlockDesignOfGeneralisedPolygon", [ IsGeneralisedPolygon and IsGeneralisedPolygonRep ] );
DeclareOperation( "IncidenceGraphOfGeneralisedPolygon", [ IsGeneralisedPolygon and IsGeneralisedPolygonRep ]);





