#############################################################################
##
##  init.g              FinInG package
##                                                              John Bamberg
##                                                              Anton Betten
##                                                              Jan De Beule
##                                                             Philippe Cara
##                                                            Michel Lavrauw
##                                                           Max Neunhoeffer
##
##  Copyright 2017	Colorado State University, Fort Collins
##					Sabancı Üniversitesi
##					Università degli Studi di Padova
##					Universeit Gent
##					University of St. Andrews
##					University of Western Australia, Perth
##                  Vrije Universiteit Brussel
##                 
##
##  Reading the declaration part of the FinInG package.
##
#############################################################################

#
# Compatibility between older and newer versions of the MatrixObj interface
#
if not IsBound(MultVector) then
    DeclareSynonym( "MultVector", MultRowVector );
fi;

ReadPackage("fining","lib/geometry.gd");

ReadPackage("fining","lib/liegeometry.gd");

ReadPackage("fining","lib/group.gd");

ReadPackage("fining","lib/projectivespace.gd");

ReadPackage("fining","lib/correlations.gd");

ReadPackage("fining","lib/polarspace.gd");
ReadPackage("fining","lib/morphisms.gd");

ReadPackage("fining","lib/enumerators.gd");

ReadPackage("fining","lib/diagram.gd");

ReadPackage("fining","lib/varieties.gd");

ReadPackage("fining","lib/affinespace.gd");
ReadPackage("fining","lib/affinegroup.gd");

ReadPackage("fining","lib/gpolygons.gd");

ReadPackage("fining","lib/orbits-stabilisers.gd"); # added 11/02/13 ml

ReadPackage("fining","lib/subgeometries.gd"); # added 25/05/16 jdb

