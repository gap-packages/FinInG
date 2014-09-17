














#DeclareCategory( "IsAllElementsOfProjectivePlane", IsAllElementsOfGeneralisedPolygon );
#DeclareRepresentation( "IsAllElementsOfProjectivePlaneRep", IsAllElementsOfGeneralisedPolygonRep, [ "geometry", "type" ] );

#DeclareCategory( "IsAllElementsOfGeneralisedQuadrangle", IsAllElementsOfGeneralisedPolygon );
#DeclareRepresentation( "IsAllElementsOfGeneralisedQuadrangleRep", IsAllElementsOfGeneralisedPolygonRep, [ "geometry", "type" ] );


#DeclareCategory( "IsAllElementsOfGeneralisedHexagon", IsAllElementsOfGeneralisedPolygon );
#DeclareRepresentation( "IsAllElementsOfGeneralisedHexagonRep", IsAllElementsOfGeneralisedPolygonRep, [ "geometry", "type" ] );

#DeclareCategory( "IsAllElementsOfKantorFamily", IsAllElementsOfGeneralisedPolygon );

### new stuff





#############################################################################
# Attributes:
#############################################################################

#DeclareAttribute( "AmbientSpace", IsGeneralisedPolygon);



#############################################################################
# Operations and functions 
#############################################################################


#DeclareOperation("Span",[IsElementOfGeneralisedPolygon, IsElementOfGeneralisedPolygon]);




## q-clans

## Elation Generalised Quadrangles


#---------------------
## obselete operations
# DeclareOperation( "EGQByqClan", [ IsFFECollCollColl, IsField and IsFinite ]);
# DeclareOperation( "KantorFamilyByqClan", [ IsFFECollCollColl, IsField and IsFinite ]); 
# DeclareOperation( "BLTSetByqClan", [ IsFFECollCollColl, IsField and IsFinite ]);
#---------------------

#############################################################################
# Operations and functions for projective planes
#############################################################################

#DeclareOperation( "ProjectivePlaneByBlocks", [ IsHomogeneousList ] );
#DeclareOperation( "ProjectivePlaneByIncidenceMatrix", [ IsMatrix ] );

