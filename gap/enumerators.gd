#############################################################################
##
##  enumerators.gd              FinInG package
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
##  Declaration stuff for enumerators of elements of polar spaces
##
#############################################################################

#############################################################################
# Very Low level help functions
#############################################################################

DeclareGlobalFunction( "PositionNonZeroFromRight" );
DeclareGlobalFunction( "Fining_pos" );
DeclareGlobalFunction( "Fining_div" );
DeclareGlobalFunction( "Fining_ffenumber" );
DeclareGlobalFunction( "Fining_unrank_GFQ" );
DeclareGlobalFunction( "Fining_rank_GFQ" );
DeclareGlobalFunction( "Fining_alpha_power" );
DeclareGlobalFunction( "Fining_log_alpha" );
DeclareGlobalFunction( "Fining_beta_power" );
DeclareGlobalFunction( "Fining_log_beta" );
DeclareGlobalFunction( "Fining_norm_one_element" );
DeclareGlobalFunction( "Fining_index_of_norm_one_element" );
DeclareGlobalFunction( "PG_element_normalize" );


#############################################################################
# Low level help operations (make it global functions?)
#############################################################################

DeclareOperation( "nb_pts_Nbar", [IsPosInt, IsPosInt]);
DeclareOperation( "nb_pts_S", [IsPosInt, IsPosInt]);
DeclareOperation( "nb_pts_N", [IsPosInt, IsPosInt]);
DeclareOperation( "nb_pts_N1", [IsPosInt, IsPosInt]);
DeclareOperation( "nb_pts_Sbar", [IsPosInt, IsPosInt]);



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

DeclareOperation( "EnumeratorByOrbit", [IsSubspacesOfClassicalPolarSpace]);
DeclareOperation( "AntonEnumerator", [IsSubspacesOfClassicalPolarSpace]);
DeclareOperation( "herm_nb_pts_N", [IsPosInt, IsPosInt]);
DeclareOperation( "herm_nb_pts_S", [IsPosInt, IsPosInt]);
DeclareOperation( "herm_nb_pts_N1", [IsPosInt, IsPosInt]);
DeclareOperation( "herm_nb_pts_Sbar", [IsPosInt, IsPosInt]);

DeclareOperation( "N1_unrank", [IsPosInt, IsFFECollection, IsPosInt, IsPosInt, IsInt]);
DeclareOperation( "N1_rank", [IsPosInt, IsFFECollection, IsPosInt, IsPosInt]);
DeclareOperation( "N_unrank", [IsPosInt, IsFFECollection, IsPosInt, IsPosInt, IsInt]);
DeclareOperation( "N_rank", [IsPosInt, IsFFECollection, IsPosInt, IsPosInt]);
DeclareOperation( "Nbar_unrank", [IsPosInt, IsFFECollection, IsPosInt, IsPosInt, IsInt]);
DeclareOperation( "Nbar_rank", [IsPosInt, IsFFECollection, IsPosInt, IsPosInt]);
DeclareOperation( "S1_unrank", [IsPosInt, IsFFECollection, IsPosInt, IsPosInt, IsInt]);
DeclareOperation( "S1_rank", [IsPosInt, IsFFECollection, IsPosInt, IsPosInt]);
DeclareOperation( "S_unrank", [IsPosInt, IsFFECollection, IsPosInt, IsPosInt, IsInt]);
DeclareOperation( "S_rank", [IsPosInt, IsFFECollection, IsPosInt, IsPosInt]);
DeclareOperation( "Sbar_unrank", [IsPosInt, IsFFECollection, IsPosInt, IsPosInt, IsInt]);
DeclareOperation( "Sbar_rank", [IsPosInt, IsFFECollection, IsPosInt, IsPosInt]);


DeclareOperation( "herm_N1_unrank", [IsPosInt, IsFFECollection, IsPosInt, IsPosInt, IsInt]);
DeclareOperation( "herm_N1_rank", [IsPosInt, IsFFECollection, IsPosInt, IsPosInt]);
DeclareOperation( "herm_N_unrank", [IsPosInt, IsFFECollection, IsPosInt, IsPosInt, IsInt]);
DeclareOperation( "herm_N_rank", [IsPosInt, IsFFECollection, IsPosInt, IsPosInt]);
DeclareOperation( "herm_S_unrank", [IsPosInt, IsFFECollection, IsPosInt, IsPosInt, IsInt]);
DeclareOperation( "herm_S_rank", [IsPosInt, IsFFECollection, IsPosInt, IsPosInt]);
DeclareOperation( "herm_Sbar_unrank", [IsPosInt, IsFFECollection, IsPosInt, IsPosInt, IsInt]);
DeclareOperation( "herm_Sbar_rank", [IsPosInt, IsFFECollection, IsPosInt, IsPosInt]);

DeclareOperation( "QElementNumber", [IsPosInt, IsPosInt, IsInt]);
DeclareOperation( "QplusElementNumber", [IsPosInt, IsPosInt, IsInt]);
DeclareOperation( "QminusElementNumber", [IsPosInt, IsPosInt, IsInt]);
DeclareOperation( "HermElementNumber", [IsPosInt, IsPosInt, IsInt]);

%DeclareGlobalFunction( "my_PG_element_normalize", 
%                   [IsFFECollection, IsPosInt, IsPosInt]);
DeclareOperation( "evaluate_hyperbolic_quadratic_form", 
                   [IsPosInt, IsFFECollection, IsPosInt, IsPosInt]);
DeclareOperation( "evaluate_hermitian_form", 
                   [IsPosInt, IsFFECollection, IsPosInt, IsInt]);

DeclareOperation( "QNumberElement", [IsPosInt, IsPosInt, IsSubspaceOfClassicalPolarSpace]);
DeclareOperation( "QplusNumberElement", [IsPosInt, IsPosInt, IsSubspaceOfClassicalPolarSpace]);
DeclareOperation( "QminusNumberElement", [IsPosInt, IsPosInt, IsSubspaceOfClassicalPolarSpace]);
DeclareOperation( "HermNumberElement", [IsPosInt, IsPosInt, IsSubspaceOfClassicalPolarSpace]);
