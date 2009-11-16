#############################################################################
##
##  enumerators.gd              Desargues package
##                                                              John Bamberg
## 								                                Anton Betten
##                                                              Jan De Beule
##                                                             Philippe Cara
## 							                                  Michel Lavrauw
##                                                                 Maska Law
##                                                           Max Neunhoeffer
##                                                            Michael Pauley
##                                                             Sven Reichard
##
##  Copyright 2006 University of Western Australia, Perth
##                 Lehrstuhl D fuer Mathematik, RWTH Aachen
##                 Ghent University
##                 Colorado State University
##                 Vrije Universiteit Brussel
##
##  Declaration stuff for enumerators of varieties of polar spaces
##
#############################################################################

#############################################################################
# Constructor operations:
#############################################################################

DeclareGlobalFunction("specialresidual");
DeclareGlobalFunction("enumpolarspace");
DeclareGlobalFunction("enum_orthogonal");
DeclareGlobalFunction("enum_hermitian");
DeclareGlobalFunction("enum_symplectic");
DeclareGlobalFunction( "enum_line" );
DeclareGlobalFunction( "enum_BaerSubline" );
DeclareGlobalFunction( "enum_unital" );

DeclareOperation( "EnumeratorByOrbit", [IsAllSubspacesOfClassicalPolarSpace]);
DeclareOperation( "AntonEnumerator", [IsAllSubspacesOfClassicalPolarSpace]);
DeclareOperation( "nb_pts_Nbar", [IsPosInt, IsPosInt]);
DeclareOperation( "nb_pts_S", [IsPosInt, IsPosInt]);
DeclareOperation( "nb_pts_N", [IsPosInt, IsPosInt]);
DeclareOperation( "nb_pts_N1", [IsPosInt, IsPosInt]);
DeclareOperation( "nb_pts_Sbar", [IsPosInt, IsPosInt]);

DeclareOperation( "N1_unrank", [IsPosInt, IsFFECollection, IsPosInt, IsPosInt, IsInt]);
DeclareOperation( "N_unrank", [IsPosInt, IsFFECollection, IsPosInt, IsPosInt, IsInt]);
DeclareOperation( "Nbar_unrank", [IsPosInt, IsFFECollection, IsPosInt, IsPosInt, IsInt]);
DeclareOperation( "S1_unrank", [IsPosInt, IsFFECollection, IsPosInt, IsPosInt, IsInt]);
DeclareOperation( "S_unrank", [IsPosInt, IsFFECollection, IsPosInt, IsPosInt, IsInt]);
DeclareOperation( "Sbar_unrank", [IsPosInt, IsFFECollection, IsPosInt, IsPosInt, IsInt]);

DeclareOperation( "QElementNumber", [IsPosInt, IsPosInt, IsInt]);
DeclareOperation( "QplusElementNumber", [IsPosInt, IsPosInt, IsInt]);
DeclareOperation( "QminusElementNumber", [IsPosInt, IsPosInt, IsInt]);
DeclareOperation( "HermElementNumber", [IsPosInt, IsPosInt, IsInt]);

DeclareOperation( "N1_rank", [IsPosInt, IsFFECollection, IsPosInt, IsPosInt]);
DeclareOperation( "N_rank", [IsPosInt, IsFFECollection, IsPosInt, IsPosInt]);
DeclareOperation( "Nbar_rank", [IsPosInt, IsFFECollection, IsPosInt, IsPosInt]);
DeclareOperation( "S1_rank", [IsPosInt, IsFFECollection, IsPosInt, IsPosInt]);
DeclareOperation( "S_rank", [IsPosInt, IsFFECollection, IsPosInt, IsPosInt]);
DeclareOperation( "Sbar_rank", [IsPosInt, IsFFECollection, IsPosInt, IsPosInt]);
DeclareOperation( "evaluate_hyperbolic_quadratic_form", 
                   [IsPosInt, IsFFECollection, IsPosInt, IsPosInt]);

DeclareOperation( "QNumberElement", [IsPosInt, IsPosInt, IsSubspaceOfClassicalPolarSpace]);
DeclareOperation( "QplusNumberElement", [IsPosInt, IsPosInt, IsSubspaceOfClassicalPolarSpace]);
DeclareOperation( "QminusNumberElement", [IsPosInt, IsPosInt, IsSubspaceOfClassicalPolarSpace]);
DeclareOperation( "HermNumberElement", [IsPosInt, IsPosInt, IsSubspaceOfClassicalPolarSpace]);
