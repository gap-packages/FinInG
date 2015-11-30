#############################################################################
##
##  read.g              FinInG package
##                                                              John Bamberg
##                                                              Anton Betten
##                                                              Jan De Beule
##                                                             Philippe Cara
##                                                            Michel Lavrauw
##                                                           Max Neunhoeffer
##
##  Copyright 2014	Colorado State University, Fort Collins
##					Universitˆ degli Studi di Padova
##					Universeit Gent
##					University of St. Andrews
##					University of Western Australia, Perth
##                  Vrije Universiteit Brussel
##                 
##
##  Reading the implementation part of the FinInG package.
##
#############################################################################

#if InstalledPackageVersion("forms") < "1.2" then
#   ReadPackage("fining","lib/forms_ext.gi");
#fi;
#if InstalledPackageVersion("forms") < "1.2.2" then
#   Forms_RESET := RESET;
#fi;
#if InstalledPackageVersion("forms") <= "1.2.2" then
#   ReadPackage("fining","lib/forms_patch.gi");
#fi;

#ReadPackage("fining","lib/genss_patch.g"); became obsolete

ReadPackage("fining","lib/geometry.gi");

ReadPackage("fining","lib/liegeometry.gi"); 

ReadPackage("fining","lib/group.gi"); 

ReadPackage("fining","lib/projectivespace.gi");

ReadPackage("fining","lib/correlations.gi");

ReadPackage("fining","lib/polarspace.gi");
ReadPackage("fining","lib/morphisms.gi");

ReadPackage("fining","lib/enumerators.gi");

ReadPackage("fining","lib/diagram.gi");

ReadPackage("fining","lib/varieties.gi");

ReadPackage("fining","lib/affinespace.gi");
ReadPackage("fining","lib/affinegroup.gi");

ReadPackage("fining","lib/gpolygons.gi");

ReadPackage("fining","lib/orbits-stabilisers.gi"); # added 11/02/13 ml

Print("\n");

