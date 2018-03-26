#############################################################################
##
##  orbits-stabilizers.gd              FinInG package
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
##  Declaration stuff for placeholders of orbits/stabilizer functions
##
#############################################################################


#############################
# Orbit(s) methods that should work faster
################################

DeclareOperation( "FiningOrbit", [IsGroup, IsObject, IsFunction] );
DeclareOperation( "FiningOrbit", [IsGroup, IsObject] );
DeclareOperation( "FiningOrbits", [IsGroup, IsObject, IsFunction] );
DeclareOperation( "FiningOrbits",[IsGroup, IsObject] );
DeclareOperation( "FiningOrbitsDomain", [IsGroup, IsObject, IsFunction] );
DeclareOperation( "FiningOrbitsDomain", [IsGroup, IsObject ] );

#############################
# Stabiliser methods that should work faster
################################

DeclareOperation( "FiningElementStabiliserOp", [IsGroup, IsObject, IsFunction] );
DeclareOperation( "FiningStabiliser", [IsProjectiveGroupWithFrob, IsElementOfIncidenceStructure]);
DeclareOperation( "FiningStabiliserOrb", [IsProjectiveGroupWithFrob, IsElementOfIncidenceStructure]);

DeclareOperation( "FiningSetwiseStabiliser", [IsProjectiveGroupWithFrob, IsSubspaceOfProjectiveSpaceCollection and IsHomogeneousList]);
DeclareOperation( "FiningSetwiseStabiliser", [IsProjectiveGroupWithFrob, IsSubspaceOfAffineSpaceCollection and IsHomogeneousList]);

#############################
# Stabiliser methods using the permutation representation of a group action
################################

DeclareOperation( "FiningStabiliserPerm", [IsProjectiveGroupWithFrob, IsElementOfIncidenceStructure]);
DeclareOperation( "FiningStabiliserPerm2", [IsProjectiveGroupWithFrob, IsElementOfIncidenceStructure]);

#############################
# Action related methods.
################################

DeclareOperation( "FixedSubspaces", [IsProjectiveGroupWithFrob, IsProjectiveSpace] );

#############################
# Stabilisers of subspaces
################################

DeclareOperation( "ProjectiveStabiliserGroupOfSubspace", [IsSubspaceOfProjectiveSpace] );
DeclareOperation( "StabiliserGroupOfSubspace", [IsSubspaceOfProjectiveSpace] );
DeclareOperation( "SpecialProjectiveStabiliserGroupOfSubspace", [IsSubspaceOfProjectiveSpace] );

