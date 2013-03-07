#############################################################################
##
##  stabilizers.gd              FinInG package
##                                                              John Bamberg
##                                                              Anton Betten
##                                                              Jan De Beule
##                                                             Philippe Cara
##                                                            Michel Lavrauw
##                                                           Max Neunhoeffer
##
##  Copyright 2013	Colorado State University, Fort Collins
##					Università degli Studi di Padova
##					Universeit Gent
##					University of St. Andrews
##					University of Western Australia, Perth
##                  Vrije Universiteit Brussel
##                 
##
##  Declaration stuff for placeholders of stabilizer functions
##
#############################################################################


#############################
# Stabiliser methods that should work faster
################################

DeclareOperation( "FiningElementStabiliserOp", [IsGroup, IsObject, IsFunction] );
DeclareOperation( "FiningStabiliser", [IsProjectiveGroupWithFrob, IsElementOfIncidenceStructure]);
DeclareOperation( "FiningStabiliserOrb", [IsProjectiveGroupWithFrob, IsElementOfIncidenceStructure]);

DeclareOperation( "FiningSetwiseStabiliser", [IsProjectiveGroupWithFrob, IsSubspaceOfProjectiveSpaceCollection and IsHomogeneousList]);

#############################
# Stabiliser methods using the permutation representation of a group action
################################


DeclareOperation( "FiningStabiliserPerm", [IsProjectiveGroupWithFrob, IsElementOfIncidenceStructure]);
DeclareOperation( "FiningStabiliserPerm2", [IsProjectiveGroupWithFrob, IsElementOfIncidenceStructure]);

