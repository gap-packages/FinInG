#############################################################################
##
##  affinegroup.gd              FinInG package
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
##  Declaration stuff for affine group representations
##
#############################################################################

DeclareAttribute( "AffineGroup", IsAffineSpace );

DeclareGlobalFunction( "OnAffinePoints" );
DeclareGlobalFunction( "OnAffineNotPoints" );
DeclareGlobalFunction( "OnAffineSubspaces" );
