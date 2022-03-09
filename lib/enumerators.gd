#############################################################################
##
##  enumerators.gd              FinInG package
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
##  Declaration stuff for enumerators of elements of polar spaces
##
#############################################################################

#############################################################################
# The enumerator functionality is quite technical. We need many tiny an less 
# tiny helping functions. We go for a bottom up approach, where we first 
# declare the helping functions, and then the real user stuff in the end.
#############################################################################

#############################################################################
# Very Low level help functions
#############################################################################

DeclareGlobalFunction( "PositionNonZeroFromRight" );
DeclareGlobalFunction( "FG_pos" );
#DeclareGlobalFunction( "FG_div" );
DeclareGlobalFunction( "FG_ffenumber" );
#DeclareGlobalFunction( "FG_unrank_GFQ" ); #never used
#DeclareGlobalFunction( "FG_rank_GFQ" ); #never used
DeclareGlobalFunction( "FG_alpha_power" );
DeclareGlobalFunction( "FG_log_alpha" );
DeclareGlobalFunction( "FG_beta_power" );
DeclareGlobalFunction( "FG_log_beta" );
DeclareGlobalFunction( "FG_norm_one_element" );
DeclareGlobalFunction( "FG_index_of_norm_one_element" );
DeclareGlobalFunction( "PG_element_normalize" );

#DeclareOperation( "evaluate_hyperbolic_quadratic_form", 
#                   [IsPosInt, IsFFECollection, IsPosInt, IsPosInt]);
#DeclareOperation( "evaluate_hermitian_form", 
#                   [IsPosInt, IsFFECollection, IsPosInt, IsInt]);

DeclareGlobalFunction( "FG_evaluate_hyperbolic_quadratic_form" );
DeclareGlobalFunction( "FG_evaluate_hermitian_form" );

#############################################################################
# Low level help functions
#############################################################################

DeclareGlobalFunction( "FG_nb_pts_Nbar" ); 
DeclareGlobalFunction( "FG_nb_pts_S" ); 
DeclareGlobalFunction( "FG_nb_pts_N" ); 
DeclareGlobalFunction( "FG_nb_pts_N1" ); 
DeclareGlobalFunction( "FG_nb_pts_Sbar" ); 

DeclareGlobalFunction( "FG_herm_nb_pts_N" ); 
DeclareGlobalFunction( "FG_herm_nb_pts_S" ); 
DeclareGlobalFunction( "FG_herm_nb_pts_N1" ); 
DeclareGlobalFunction( "FG_herm_nb_pts_Sbar" ); 

DeclareGlobalFunction( "FG_N1_unrank" ); 
DeclareGlobalFunction( "FG_N1_rank" ); 

DeclareGlobalFunction( "FG_N_unrank" ); 
DeclareGlobalFunction( "FG_N_rank" ); 

DeclareGlobalFunction( "FG_Nbar_unrank" ); 
DeclareGlobalFunction( "FG_Nbar_rank" ); 

#DeclareGlobalFunction( "S1_unrank" ); #not found in .gi file
#DeclareGlobalFunction( "S1_rank" ); #not found in .gi file

DeclareGlobalFunction( "FG_S_unrank" );
DeclareGlobalFunction( "FG_S_rank" ); 

DeclareGlobalFunction( "FG_Sbar_unrank" ); 
DeclareGlobalFunction( "FG_Sbar_rank" ); 

DeclareGlobalFunction( "FG_herm_N1_unrank" ); 
DeclareGlobalFunction( "FG_herm_N1_rank" ); 

DeclareGlobalFunction( "FG_herm_N_unrank" ); 
DeclareGlobalFunction( "FG_herm_N_rank" ); 

DeclareGlobalFunction( "FG_herm_S_unrank" ); 
DeclareGlobalFunction( "FG_herm_S_rank" ); 

DeclareGlobalFunction( "FG_herm_Sbar_unrank" ); 
DeclareGlobalFunction( "FG_herm_Sbar_rank" ); 

#############################################################################
# Low level ElementNumber/NumberElement functions 
#############################################################################

DeclareGlobalFunction( "QElementNumber" );
DeclareGlobalFunction( "QplusElementNumber" );
DeclareGlobalFunction( "QminusElementNumber" );
DeclareGlobalFunction( "HermElementNumber" );

DeclareGlobalFunction( "QNumberElement" );
DeclareGlobalFunction( "QplusNumberElement" );
DeclareGlobalFunction( "QminusNumberElement" );
DeclareGlobalFunction( "HermNumberElement" );

#############################################################################
# The enumerator for points of a polar space, bundled in one operation.
#############################################################################

DeclareOperation( "AntonEnumerator", [IsSubspacesOfClassicalPolarSpace]);

#############################################################################
# Low level enumerators
#############################################################################

#the next three are never used.
#DeclareGlobalFunction( "enum_line" );
#DeclareGlobalFunction( "enum_BaerSubline" );
#DeclareGlobalFunction( "enum_unital" );

DeclareGlobalFunction("FG_specialresidual");
DeclareGlobalFunction("FG_enum_orthogonal");
DeclareGlobalFunction("FG_enum_hermitian");
DeclareGlobalFunction("FG_enum_symplectic");

#DeclareGlobalFunction("enumpolarspace"); #does not occur

#############################################################################
# The enumerator using the orbit. This is the only operation declaration in 
# this file for a user intended operation. 
#############################################################################

DeclareOperation( "EnumeratorByOrbit", [IsSubspacesOfClassicalPolarSpace]);
